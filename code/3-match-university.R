# Matches campus-police agreements to university boundary polygons -> data/intermediate/match-university.parquet

library(tidyverse)
library(sf)
library(tigris)

source("code/functions.R")

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

YEAR <- 2024

agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")
manual_polygons <- arrow::read_parquet("data/intermediate/manual-non-facility-polygons.parquet")
state_xwalk <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")
university_boundaries <- st_read("data/intermediate/reference-university-campuses.parquet", quiet = TRUE)

university_lookup <- university_boundaries |>
  left_join(state_xwalk, by = c("state" = "state_abbr")) |>
  transmute(
    match_name = name,
    university_key = norm_key(name),
    state_key = norm_state(state_full),
    state_fips,
    # county from the address fields, independent of the one the polygon sits in
    university_county_fips = county_fips,
    geometry
  )

university_name_overrides <-
  tribble(
    ~university_guess, ~university_guess_fixed,
    "Florida A&M University", "Florida Agricultural And Mechanical University",
    "Tallahassee State College", "Tallahassee Community College"
  )

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

university_sf <- agreements |>
  left_join(university_overrides, by = c("agency", "state", "county")) |>
  # un-overridden rows of another class evaluate to NA; filter() drops those
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
    university_key = norm_key(
      coalesce(manual_university_match, university_guess_fixed, university_guess)
    ),
    state_key = norm_state(state),
    sheet_county_key = norm_county(county)
  ) |>
  left_join(university_lookup, by = c("state_key", "university_key")) |>
  st_as_sf() |>
  mutate(
    match_type = case_when(
      is.na(match_name) ~ "unmatched",
      is.na(manual_university_match) ~ "university_name",
      .default = "manual_override"
    ),
    county_fips = university_county_fips
  )

# the campus layer holds duplicate keys for multi-campus systems
stopifnot(
  "a duplicated campus key fanned an agreement out into multiple rows" = !anyDuplicated(
    university_sf$agreement_id
  )
)

# a campus is not a census unit, so place comes from the largest overlap and stays NA outside any
places_ref <- places(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(overlap_place_fips = PLACEFP, geometry)

matched_campuses <- university_sf |>
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

# campuses in no incorporated place fall back to cousubs (townships and New England towns are municipalities too)
unplaced_campuses <- matched_campuses |>
  anti_join(place_overlap, by = "agreement_id")

cousub_overlap <- if (nrow(unplaced_campuses) > 0) {
  unplaced_states <- university_sf |>
    st_drop_geometry() |>
    semi_join(st_drop_geometry(unplaced_campuses), by = "agreement_id") |>
    filter(!is.na(state_fips)) |>
    distinct(state_fips) |>
    pull(state_fips)

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
  tibble(agreement_id = character(), overlap_place_fips = character())
}

counties_ref <- counties_reference(YEAR)
county_overlap <- matched_campuses |>
  st_intersection(
    counties_ref |>
      transmute(overlap_county_fips = geoid, geometry) |>
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
  left_join(
    counties_ref |>
      st_drop_geometry() |>
      transmute(state_key, sheet_county_key = county_key, sheet_county_fips = geoid),
    by = c("state_key", "sheet_county_key")
  ) |>
  mutate(
    place_fips = overlap_place_fips,
    county_fips = coalesce(overlap_county_fips, county_fips),
    # a campus is not a census unit; the published geoid is its county, as for facilities
    geoid = county_fips,
    # city is never compared: the postal city routinely differs from the census place
    university_address_mismatch = coalesce(
      university_county_fips != overlap_county_fips,
      FALSE
    ),
    # one polygon per campus, so a system PD's other campuses can sit in other counties
    university_county_mismatch = coalesce(
      sheet_county_fips != county_fips,
      FALSE
    ),
    geometry_vintage = if_else(st_is_empty(geometry), NA_integer_, YEAR),
    geometry_unmatched = is.na(match_name) | st_is_empty(geometry)
  ) |>
  select(
    agreement_id,
    match_name,
    match_type,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    geometry_vintage,
    geometry_unmatched,
    manual_reason,
    manual_note,
    university_address_mismatch,
    university_county_mismatch,
    geometry
  )

sf::st_write(university_sf, dsn = "data/intermediate/match-university.parquet", driver = "Parquet", layer_options = "USE_PARQUET_GEO_TYPES=ONLY", delete_dsn = file.exists("data/intermediate/match-university.parquet"), quiet = TRUE)
