library(tidyverse)
library(sf)
library(tigris)
library(arrow)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

agencies_all <- arrow::read_parquet("data/processed/agencies_all.parquet") |>
  normalize_agencies_all()
manual_non_facility_polygons <- arrow::read_parquet(
  "data/processed/manual_non_facility_polygons.parquet"
)

YEAR <- 2024

# county boundaries ------------------------------------------------------

counties_sf <-
  tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    state = str_to_title(STATE_NAME),
    county = str_to_title(NAMELSAD),
    statefp = STATEFP,
    countyfp = COUNTYFP,
    geometry
  ) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_county(county)
  )

# manual county overrides -----------------------------------------------

county_overrides <-
  manual_non_facility_polygons |>
  select(
    agency = agency,
    state,
    county,
    manual_match_layer,
    manual_county_match = manual_match_name,
    manual_reason,
    manual_note
  )

# county agreements ------------------------------------------------------

county_agreements_sf <-
  agencies_all |>
  left_join(
    county_overrides,
    by = c("agency", "state", "county")
  ) |>
  filter(
    manual_match_layer == "county" |
      (geom_class == "county_polygon" & is.na(manual_match_layer))
  ) |>
  mutate(
    manual_county_match = if_else(
      manual_match_layer == "county",
      manual_county_match,
      NA_character_
    ),
    county_match = coalesce(manual_county_match, county),
    state_key = norm_state(state),
    county_key = norm_county(county_match)
  ) |>
  left_join(
    counties_sf |>
      select(state_key, county_key, statefp, countyfp, geometry),
    by = c("state_key", "county_key")
  ) |>
  mutate(
    src = if_else(
      is.na(manual_county_match),
      "tigris_county",
      "manual_county_override"
    ),
    needs_review = needs_review | is.na(geometry) | st_is_empty(geometry)
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
    county_match,
    statefp,
    countyfp,
    src,
    manual_match_layer,
    manual_reason,
    manual_note,
    geometry
  ) |>
  st_as_sf()

# save county geometries -------------------------------------------------

write_sf_parquet(
  county_agreements_sf,
  "data/processed/county_agreements_sf.parquet"
)
