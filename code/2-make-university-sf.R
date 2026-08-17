library(tidyverse)
library(sf)
library(arrow)

source("code/functions.R")

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

YEAR <- 2024

agencies_all <- arrow::read_parquet("data/agencies_all.parquet") |>
  normalize_agencies_all()
manual_non_facility_polygons <- arrow::read_parquet(
  "data/manual_non_facility_polygons.parquet"
)
state_xwalk <- arrow::read_parquet("data/state_xwalk.parquet")
university_boundaries <- read_sf_parquet(
  "data/university_boundaries.parquet",
  crs = 3857
)

# university boundary lookup ---------------------------------------------

university_lookup <-
  university_boundaries |>
  st_transform(4326) |>
  mutate(
    university_name = str_squish(NAME),
    state_abbr = str_to_upper(str_squish(STATE))
  ) |>
  left_join(state_xwalk, by = "state_abbr") |>
  mutate(
    university_key = norm_key(university_name),
    state_key = norm_state(state_full),
    # campus county per the HIFLD address fields; non-FIPS placeholders
    # ("NOT AVAILABLE") drop
    university_county_fips = if_else(
      str_detect(COUNTYFIPS, "^[0-9]{5}$"),
      COUNTYFIPS,
      NA_character_
    )
  ) |>
  select(
    university_name,
    university_key,
    state_key,
    state_fips,
    university_county_fips,
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

university_overrides <-
  manual_non_facility_polygons |>
  select(
    agency = agency,
    state,
    county,
    manual_match_layer,
    manual_university_match = manual_match_name,
    manual_reason,
    manual_note
  )

# university agreements --------------------------------------------------

university_agreements_sf <- agencies_all |>
  left_join(
    university_overrides,
    by = c("agency", "state", "county")
  ) |>
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
    university_guess_final = coalesce(
      manual_university_match,
      university_guess_fixed,
      university_guess
    ),
    university_key = norm_key(university_guess_final),
    state_key = norm_state(state)
  ) |>
  left_join(university_lookup, by = c("state_key", "university_key")) |>
  st_as_sf() |>
  mutate(
    src = if_else(
      is.na(university_name),
      "unmatched_university_boundary",
      if_else(
        is.na(manual_university_match),
        "university_boundary",
        "manual_university_override"
      )
    ),
    county_fips = university_county_fips,
    geoid = NA_character_,
    needs_geometry_review = is.na(university_name) | st_is_empty(geometry),
    needs_review = needs_review | needs_geometry_review
  )

# a campus is not a census unit, so its municipality comes from the place or
# county-subdivision polygon with the largest overlap; a campus outside any
# municipality legitimately stays NA
places_ref <- tigris::places(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(overlap_place_fips = PLACEFP, geometry)

matched_campuses <- university_agreements_sf |>
  filter(!st_is_empty(geometry)) |>
  select(agreement_id) |>
  st_transform(3857)

place_overlap <- matched_campuses |>
  st_intersection(places_ref |> st_transform(3857)) |>
  mutate(overlap_area = st_area(geometry)) |>
  st_drop_geometry() |>
  group_by(agreement_id) |>
  arrange(desc(overlap_area), overlap_place_fips, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup()

# campuses in no incorporated place fall back to the county subdivision
# (townships and New England towns are municipalities too)
unplaced_campuses <- matched_campuses |>
  anti_join(st_drop_geometry(place_overlap), by = "agreement_id")

cousub_overlap <- if (nrow(unplaced_campuses) > 0) {
  unplaced_states <- university_agreements_sf |>
    st_drop_geometry() |>
    semi_join(st_drop_geometry(unplaced_campuses), by = "agreement_id") |>
    filter(!is.na(state_fips)) |>
    distinct(state_fips) |>
    pull(state_fips)

  map(
    unplaced_states,
    \(fp) {
      tigris::county_subdivisions(state = fp, cb = TRUE, year = YEAR, class = "sf")
    }
  ) |>
    bind_rows() |>
    transmute(overlap_place_fips = COUSUBFP, geometry) |>
    st_transform(3857) |>
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

# the campus polygon also identifies its county; HIFLD's address fields give
# an independent county and city, so disagreement with the polygon flags a
# suspect campus match
county_overlap <- matched_campuses |>
  st_intersection(
    tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
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

university_agreements_sf <- university_agreements_sf |>
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
    county_fips = coalesce(overlap_county_fips, county_fips),
    # the address CITY is postal geography and routinely disagrees with the
    # census place a campus sits in (FIU's address is "Miami", the campus is
    # in University Park CDP), so only the county is compared
    university_address_mismatch = coalesce(
      university_county_fips != overlap_county_fips,
      FALSE
    ),
    needs_review = needs_review | university_address_mismatch
  ) |>
  select(
    state,
    county,
    agency,
    support_type,
    agency_level,
    geom_class,
    agreement_id,
    needs_review,
    signed,
    moa,
    addendum,
    university_name,
    university_guess,
    university_guess_final,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    university_address_mismatch,
    src,
    manual_match_layer,
    manual_reason,
    manual_note,
    geometry
  )

# save university geometries ---------------------------------------------

write_sf_parquet(
  university_agreements_sf,
  "data/university_agreements_sf.parquet"
)
