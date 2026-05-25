library(tidyverse)
library(sf)
library(tigris)
library(arrow)
library(sfarrow)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

agencies_all <- arrow::read_parquet("data/processed/agencies_all.parquet")
manual_non_facility_polygons <- arrow::read_parquet(
  "data/processed/manual_non_facility_polygons.parquet"
)

YEAR <- 2024

# place and county-subdivision boundaries --------------------------------

places_sf <- tigris::places(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    state = str_to_title(STATE_NAME),
    place_guess = str_to_title(NAME),
    statefp = STATEFP,
    placefp = PLACEFP,
    geometry
  )

# pull all states for county subdivisions
states_sf_raw <- tigris::states(cb = TRUE, year = YEAR, class = "sf")

cousubs_sf <- map_dfr(unique(states_sf_raw$STATEFP), function(fp) {
  tigris::county_subdivisions(state = fp, cb = TRUE, year = YEAR, class = "sf")
}) |>
  transmute(
    state = str_to_title(STATE_NAME),
    place_guess = str_to_title(NAME),
    statefp = STATEFP,
    placefp = COUSUBFP,
    geometry
  )

# normalized places lookup (places preferred over cousubs)
places_lookup <- bind_rows(
  places_sf |> mutate(src = "place"),
  cousubs_sf |> mutate(src = "cousub")
) |>
  mutate(
    state_key = norm_state(state),
    place_key = norm_place(place_guess),
    src_rank = if_else(src == "place", 1L, 2L)
  ) |>
  group_by(state_key, place_key) |>
  slice_min(src_rank, n = 1, with_ties = FALSE) |>
  ungroup()

# manual municipal overrides --------------------------------------------

municipal_overrides <- manual_non_facility_polygons |>
  select(
    agency = agency,
    state,
    county,
    manual_match_layer,
    manual_city_match = manual_match_name,
    manual_reason,
    manual_note
  )

# municipal agreements ---------------------------------------------------

municipal_agreements_sf <- agencies_all |>
  left_join(
    municipal_overrides,
    by = c("LAW ENFORCEMENT AGENCY" = "agency", "state", "county")
  ) |>
  filter(
    manual_match_layer == "municipal" |
      (geom_class == "municipal_polygon" & is.na(manual_match_layer))
  ) |>
  mutate(
    manual_city_match = if_else(
      manual_match_layer == "municipal",
      manual_city_match,
      NA_character_
    ),
    city_guess = extract_city_guess(`LAW ENFORCEMENT AGENCY`),
    city_match = coalesce(manual_city_match, city_guess),
    state_key = norm_state(state),
    place_key = norm_place(city_match)
  ) |>
  left_join(
    places_lookup |>
      select(state_key, place_key, geometry, src),
    by = c("state_key", "place_key")
  ) |>
  mutate(
    src = if_else(
      is.na(manual_city_match),
      src,
      paste("manual_municipal_override", src, sep = ":")
    ),
    needs_review = needs_review | is.na(geometry) | st_is_empty(geometry)
  ) |>
  st_as_sf()


# save municipal geometries ----------------------------------------------

sfarrow::st_write_parquet(
  municipal_agreements_sf,
  "data/processed/municipal_agreements_sf.parquet"
)

sfarrow::st_write_parquet(
  places_lookup,
  "data/processed/places_lookup.parquet"
)
