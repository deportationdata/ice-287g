library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

agreements <- arrow::read_parquet("data/agreements.parquet")
manual_polygons <- arrow::read_parquet("data/manual-polygons.parquet")

YEAR <- 2024

# NAMELSAD keeps the legal suffix ("X County", "X Parish", "X city"), as does
# the ICE county field, and both sides pass through norm_county
counties_sf <- tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    county = str_to_title(NAMELSAD),
    state_key = norm_state(STATE_NAME),
    county_key = norm_county(county),
    statefp = STATEFP,
    countyfp = COUNTYFP,
    geometry
  )

county_overrides <- manual_polygons |>
  select(
    agency,
    state,
    county,
    manual_match_layer = match_layer,
    manual_county_match = match_name,
    manual_reason = reason,
    manual_note = note
  )

county_agreements_sf <- agreements |>
  left_join(county_overrides, by = c("agency", "state", "county")) |>
  # manual_match_layer is NA for non-override rows, so is.na() is what admits
  # them; an override routing elsewhere drops the row for that layer to pick up
  filter(
    manual_match_layer == "county" |
      (geom_class == "county_polygon" & is.na(manual_match_layer))
  ) |>
  mutate(
    # an override aimed at another layer must not leak its name into this one
    manual_county_match = if_else(
      manual_match_layer == "county",
      manual_county_match,
      NA_character_
    ),
    county_match = coalesce(manual_county_match, county),
    # state_key comes from the agreement's original state, so a manual override
    # can redirect the county name but never cross a state line
    state_key = norm_state(state),
    county_key = norm_county(county_match)
  ) |>
  left_join(
    counties_sf |>
      select(
        state_key,
        county_key,
        match_name = county,
        statefp,
        countyfp,
        geometry
      ),
    by = c("state_key", "county_key")
  ) |>
  mutate(
    match_type = case_when(
      is.na(statefp) ~ "unmatched",
      !is.na(manual_county_match) ~ "manual_override",
      TRUE ~ "county_name"
    ),
    state_fips = statefp,
    # both-non-NA guard so unmatched joins get NA, not "NANA"
    county_fips = if_else(
      !is.na(statefp) & !is.na(countyfp),
      paste0(statefp, countyfp),
      NA_character_
    ),
    # same string as county_fips here; both ship because they mean different
    # things downstream (admin code vs census geoid)
    geoid = county_fips,
    # OR-accumulate so an upstream flag is never reset; unmatched agreements
    # are kept with empty geometries
    needs_review = needs_review | is.na(geometry) | st_is_empty(geometry)
  ) |>
  select(
    agreement_id,
    match_name,
    match_type,
    state_fips,
    county_fips,
    geoid,
    needs_review,
    manual_reason,
    manual_note,
    geometry
  ) |>
  st_as_sf() |>
  st_transform(4326)

write_sf_parquet(county_agreements_sf, "data/county-sf.parquet")
