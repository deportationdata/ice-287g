library(tidyverse)
library(sf)
library(tigris)
library(arrow)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

agencies_all <- arrow::read_parquet("data/agencies_all.parquet") |>
  normalize_agencies_all()
manual_non_facility_polygons <- arrow::read_parquet(
  "data/manual_non_facility_polygons.parquet"
)

YEAR <- 2024

# state boundaries -------------------------------------------------------

states_sf <- tigris::states(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    state = str_to_title(NAME),
    statefp = STATEFP,
    geometry
  )

# territories
counties_sf_raw <- tigris::counties(cb = TRUE, year = YEAR, class = "sf")

territories <-
  counties_sf_raw |>
  filter(STATEFP %in% c("60", "66", "69", "78")) |>
  group_by(STATEFP) |>
  summarise(geometry = st_union(geometry)) |>
  left_join(
    tibble(
      STATEFP = c("60", "66", "69", "78"),
      NAME = c(
        "American Samoa",
        "Guam",
        "Commonwealth of the Northern Mariana Islands",
        "United States Virgin Islands"
      ),
      STUSPS = c("AS", "GU", "MP", "VI")
    ),
    by = "STATEFP"
  )

all_states_and_territories <-
  bind_rows(
    states_sf |>
      filter(as.integer(statefp) <= 56 | statefp == "72") |> # states plus PR
      transmute(
        NAME = state,
        STATEFP = statefp,
        STUSPS = NA_character_,
        geometry
      ),
    territories |>
      rename(STATEFP = STATEFP) |>
      select(NAME, STATEFP, STUSPS, geometry)
  )

state_lookup <-
  all_states_and_territories |>
  transmute(
    state = NAME,
    statefp = STATEFP,
    state_abbr = STUSPS,
    geometry
  )

# manual state overrides -------------------------------------------------

state_overrides <-
  manual_non_facility_polygons |>
  select(
    agency = agency,
    state,
    county,
    manual_match_layer,
    manual_state_match = manual_match_name,
    manual_reason,
    manual_note
  )

# state agreements -------------------------------------------------------

state_agreements_sf <-
  agencies_all |>
  left_join(
    state_overrides,
    by = c("agency", "state", "county")
  ) |>
  filter(
    manual_match_layer == "state" |
      (geom_class == "state_polygon" & is.na(manual_match_layer))
  ) |>
  mutate(
    manual_state_match = if_else(
      manual_match_layer == "state",
      manual_state_match,
      NA_character_
    ),
    state_match = coalesce(manual_state_match, state)
  ) |>
  left_join(state_lookup, by = c("state_match" = "state")) |>
  mutate(
    src = if_else(
      is.na(manual_state_match),
      "tigris_state",
      "manual_state_override"
    ),
    state_fips = statefp,
    county_fips = NA_character_,
    place_fips = NA_character_,
    geoid = state_fips,
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
    signed,
    state_match,
    statefp,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    state_abbr,
    src,
    manual_match_layer,
    manual_reason,
    manual_note,
    geometry
  ) |>
  st_as_sf()

# save state geometries --------------------------------------------------

write_sf_parquet(
  state_agreements_sf,
  "data/state_agreements_sf.parquet"
)
