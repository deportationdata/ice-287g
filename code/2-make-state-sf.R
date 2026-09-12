# Matches state-level agreements to state and territory polygons -> data/state-sf.parquet

library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

agreements <- arrow::read_parquet("data/agreements.parquet")
manual_polygons <- arrow::read_parquet("data/manual-polygons.parquet")

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
territories_sf <- tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
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
      (geom_class == "state_polygon" & is.na(manual_match_layer))
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
    # keep-all: unmatched agreements ride along with empty geometries
    needs_review = needs_review | is.na(geometry) | st_is_empty(geometry)
  ) |>
  select(
    agreement_id,
    match_name,
    match_type,
    state_fips,
    geoid,
    needs_review,
    manual_reason,
    manual_note,
    geometry
  ) |>
  st_as_sf() |>
  st_transform(4326)

write_sf_parquet(state_agreements_sf, "data/state-sf.parquet")
