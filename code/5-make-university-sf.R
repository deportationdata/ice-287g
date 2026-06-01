library(tidyverse)
library(sf)
library(arrow)

source("code/functions.R")

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
    state_key = norm_state(state_full)
  ) |>
  select(university_name, university_key, state_key, state_fips, geometry)

# manual name overrides --------------------------------------------------

university_name_overrides <-
  tribble(
    ~university_guess, ~university_guess_fixed,
    "Florida A&M University", "Florida Agricultural And Mechanical University"
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
    county_fips = NA_character_,
    place_fips = NA_character_,
    geoid = NA_character_,
    needs_geometry_review = is.na(university_name) | st_is_empty(geometry),
    needs_review = needs_review | needs_geometry_review
  ) |>
  select(
    state,
    county,
    agency,
    support_type,
    agency_level,
    geom_class,
    ORI9,
    FSTATE,
    FCOUNTY,
    FPLACE,
    leaic_name,
    leaic_match_type,
    crime_ori,
    crime_agency_name,
    crime_match_type,
    ori_source,
    needs_review,
    has_addendum,
    moa_pending,
    university_name,
    university_guess,
    university_guess_final,
    state_fips,
    county_fips,
    place_fips,
    geoid,
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
