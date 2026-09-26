# Matches state-level agreements to state and territory polygons -> data/intermediate/match-state.parquet

library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")
manual_polygons <- arrow::read_parquet("data/intermediate/manual-non-facility-polygons.parquet")

YEAR <- 2024

# sheet and manual names must match tigris NAME exactly; str_to_title would break "District of Columbia"
states_sf <- tigris::states(cb = TRUE, year = YEAR, class = "sf") |>
  filter(as.integer(STATEFP) <= 56 | STATEFP == "72") |> # states plus PR
  transmute(
    state = NAME,
    statefp = STATEFP,
    geometry
  )

# this st_union in geographic coordinates is why sf_use_s2(FALSE) is set
territories_sf <- counties(cb = TRUE, year = YEAR, class = "sf") |>
  filter(STATEFP %in% c("60", "66", "69", "78")) |>
  group_by(STATEFP) |>
  summarise(geometry = st_union(geometry)) |>
  left_join(
    tibble(
      STATEFP = c("60", "66", "69", "78"),
      state = c(
        "American Samoa",
        "Guam",
        "Commonwealth of the Northern Mariana Islands",
        "United States Virgin Islands"
      )
    ),
    by = "STATEFP"
  ) |>
  transmute(state, statefp = STATEFP, geometry)

state_lookup <- bind_rows(states_sf, territories_sf)

state_overrides <- manual_polygons |>
  select(
    agency,
    state,
    county,
    manual_match_layer = match_layer,
    manual_state_match = match_name,
    manual_reason = reason,
    manual_note = note
  )

state_agreements_sf <- agreements |>
  left_join(state_overrides, by = c("agency", "state", "county")) |>
  # un-overridden rows of another class evaluate to NA; filter() drops those
  filter(
    manual_match_layer == "state" |
      (geometry_type == "polygon" & jurisdiction_level == "State" & is.na(manual_match_layer))
  ) |>
  mutate(
    # manual_polygons is shared by every layer, so blank a name aimed elsewhere
    manual_state_match = if_else(
      manual_match_layer == "state",
      manual_state_match,
      NA_character_
    ),
    state_match = coalesce(manual_state_match, state)
  ) |>
  left_join(state_lookup, by = c("state_match" = "state")) |>
  mutate(
    match_name = if_else(is.na(statefp), NA_character_, state_match),
    match_type = case_when(
      is.na(statefp) ~ "unmatched",
      !is.na(manual_state_match) ~ "manual_override",
      TRUE ~ "state_name"
    ),
    state_fips = statefp,
    geoid = state_fips,
    geometry_vintage = if_else(is.na(statefp), NA_integer_, YEAR),
    # keep-all: unmatched agreements ride along with empty geometries
    geometry_unmatched = is.na(geometry) | st_is_empty(geometry)
  ) |>
  select(
    agreement_id,
    match_name,
    match_type,
    state_fips,
    geoid,
    geometry_vintage,
    geometry_unmatched,
    manual_reason,
    manual_note,
    geometry
  ) |>
  st_as_sf() |>
  st_transform(4326)

sf::st_write(state_agreements_sf, dsn = "data/intermediate/match-state.parquet", driver = "Parquet", layer_options = "USE_PARQUET_GEO_TYPES=ONLY", delete_dsn = file.exists("data/intermediate/match-state.parquet"), quiet = TRUE)
