library(tidyverse)
library(sf)
library(tigris)

source("code/functions.R")

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

YEAR <- 2024

agreements <- arrow::read_parquet("data/agreements.parquet")
manual_polygons <- arrow::read_parquet("data/manual-polygons.parquet")
state_xwalk <- arrow::read_parquet("data/state-xwalk.parquet")
university_boundaries <- read_sf_parquet("data/university-boundaries.parquet")

# university boundary lookup ---------------------------------------------

university_lookup <- university_boundaries |>
  left_join(state_xwalk, by = c("state" = "state_abbr")) |>
  transmute(
    match_name = name,
    university_key = norm_key(name),
    state_key = norm_state(state_full),
    state_fips,
    # campus county per the address fields, an independent check against the
    # county the polygon actually sits in
    university_county_fips = county_fips,
    geometry
  )

# manual name overrides --------------------------------------------------

university_name_overrides <-
  tribble(
    ~university_guess, ~university_guess_fixed,
    "Florida A&M University", "Florida Agricultural And Mechanical University",
    "Tallahassee State College", "Tallahassee Community College"
  )

# manual university overrides -------------------------------------------

university_overrides <- manual_polygons |>
  select(
    agency,
    state,
    county,
    manual_match_layer = match_layer,
    manual_university_match = match_name,
    manual_reason = reason,
    manual_note = note
  )

# university agreements --------------------------------------------------

university_sf <- agreements |>
  left_join(university_overrides, by = c("agency", "state", "county")) |>
  # an agreement enters if manually routed here, or if it is classified as a
  # campus and carries no override; the NA == comparison is what keeps
  # non-overridden rows out of the first clause
  filter(
    manual_match_layer == "university" |
      (geom_class == "university_polygon" & is.na(manual_match_layer))
  ) |>
  mutate(
    manual_university_match = if_else(
      manual_match_layer == "university",
      manual_university_match,
      NA_character_
    ),
    university_guess = extract_university_guess(agency)
  ) |>
  left_join(university_name_overrides, by = "university_guess") |>
  mutate(
    # manual override beats the hardcoded name fix beats the raw guess
    university_key = norm_key(
      coalesce(manual_university_match, university_guess_fixed, university_guess)
    ),
    state_key = norm_state(state)
  ) |>
  # single exact tier: normalized-key equality within state, no fuzzy fallback
  left_join(university_lookup, by = c("state_key", "university_key")) |>
  st_as_sf() |>
  mutate(
    match_type = case_when(
      is.na(match_name) ~ "unmatched",
      is.na(manual_university_match) ~ "university_name",
      .default = "manual_override"
    ),
    county_fips = university_county_fips,
    needs_review = needs_review | is.na(match_name) | st_is_empty(geometry)
  )

# the campus layer holds duplicate keys for multi-campus systems, which is fine
# unless an agreement actually matches one
stopifnot(
  "a duplicated campus key fanned an agreement out into multiple rows" = !anyDuplicated(
    university_sf$agreement_id
  )
)

# a campus is not a census unit, so its municipality comes from the place or
# county-subdivision polygon with the largest overlap; a campus outside any
# municipality legitimately stays NA
places_ref <- places(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(overlap_place_fips = PLACEFP, geometry)

matched_campuses <- university_sf |>
  filter(!st_is_empty(geometry)) |>
  select(agreement_id) |>
  st_transform(3857)

# Web Mercator areas are latitude-distorted but consistent among one campus's
# competing overlaps, which is all the ranking needs; ties break on ascending
# FIPS for determinism
place_overlap <- matched_campuses |>
  st_intersection(places_ref |> st_transform(3857)) |>
  mutate(overlap_area = st_area(geometry)) |>
  st_drop_geometry() |>
  group_by(agreement_id) |>
  arrange(desc(overlap_area), overlap_place_fips, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup()

# campuses intersecting no incorporated place fall back to the county
# subdivision (townships and New England towns are municipalities too); one
# barely clipping a place still gets that place, never a cousub
unplaced_campuses <- matched_campuses |>
  anti_join(place_overlap, by = "agreement_id")

cousub_overlap <- if (nrow(unplaced_campuses) > 0) {
  unplaced_states <- university_sf |>
    st_drop_geometry() |>
    semi_join(st_drop_geometry(unplaced_campuses), by = "agreement_id") |>
    filter(!is.na(state_fips)) |>
    distinct(state_fips) |>
    pull(state_fips)

  # cousubs download per state, only for states with an unplaced campus
  map(
    unplaced_states,
    \(fp) county_subdivisions(state = fp, cb = TRUE, year = YEAR, class = "sf")
  ) |>
    bind_rows() |>
    transmute(overlap_place_fips = COUSUBFP, geometry) |>
    st_transform(3857) |>
    # campuses as x so agreement_id survives the intersection
    st_intersection(x = unplaced_campuses) |>
    mutate(overlap_area = st_area(geometry)) |>
    st_drop_geometry() |>
    group_by(agreement_id) |>
    arrange(desc(overlap_area), overlap_place_fips, .by_group = TRUE) |>
    slice_head(n = 1) |>
    ungroup()
} else {
  tibble(agreement_id = integer(), overlap_place_fips = character())
}

# the address fields give a county independent of the polygon's, so
# disagreement flags a suspect campus match
county_overlap <- matched_campuses |>
  st_intersection(
    counties(cb = TRUE, year = YEAR, class = "sf") |>
      transmute(overlap_county_fips = GEOID, geometry) |>
      st_transform(3857)
  ) |>
  mutate(overlap_area = st_area(geometry)) |>
  st_drop_geometry() |>
  group_by(agreement_id) |>
  arrange(desc(overlap_area), overlap_county_fips, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(agreement_id, overlap_county_fips)

university_sf <- university_sf |>
  left_join(
    bind_rows(
      place_overlap |> select(agreement_id, overlap_place_fips),
      cousub_overlap |> select(agreement_id, overlap_place_fips)
    ),
    by = "agreement_id"
  ) |>
  left_join(county_overlap, by = "agreement_id") |>
  mutate(
    place_fips = overlap_place_fips,
    # the polygon-derived county wins; the address county is the fallback
    county_fips = coalesce(overlap_county_fips, county_fips),
    # only the county is compared: the address city is postal geography and
    # routinely disagrees with the census place (FIU's address is "Miami", the
    # campus is in University Park CDP)
    university_address_mismatch = coalesce(
      university_county_fips != overlap_county_fips,
      FALSE
    ),
    needs_review = needs_review | university_address_mismatch
  ) |>
  select(
    agreement_id,
    match_name,
    match_type,
    state_fips,
    county_fips,
    place_fips,
    needs_review,
    manual_reason,
    manual_note,
    university_address_mismatch,
    geometry
  )

# save university geometries ---------------------------------------------

write_sf_parquet(university_sf, "data/university-sf.parquet")
