# Matches county-level agreements to tigris county polygons -> data/intermediate/match-county.parquet

library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")
manual_polygons <- arrow::read_parquet("data/intermediate/manual-non-facility-polygons.parquet")
manual_judicial <- arrow::read_parquet("data/intermediate/manual-judicial-district-counties.parquet")

YEAR <- 2024

# both NAMELSAD and the ICE county field keep the legal suffix ("X County", "X Parish", "X city"), hence norm_county
counties_sf <- counties_reference(YEAR) |>
  select(county, state_key, county_key, statefp, countyfp, geometry_vintage, geometry)

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
  # un-overridden rows of another class evaluate to NA; filter() drops those
  filter(
    manual_match_layer == "county" |
      (geometry_type == "polygon" & jurisdiction_level == "County" & is.na(manual_match_layer))
  ) |>
  mutate(
    # manual_polygons is shared by every layer, so blank a name aimed elsewhere
    manual_county_match = if_else(
      manual_match_layer == "county",
      manual_county_match,
      NA_character_
    ),
    county_match = coalesce(manual_county_match, county),
    # keyed on the agreement's own state, so an override can never cross a state line
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
        geometry_vintage,
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
    # duplicates county_fips on purpose: admin code vs census geoid downstream
    geoid = county_fips,
    # keep-all: unmatched agreements ride along with empty geometries
    geometry_unmatched = is.na(geometry) | st_is_empty(geometry)
  ) |>
  select(
    agreement_id,
    match_name,
    match_type,
    state_fips,
    county_fips,
    geoid,
    geometry_vintage,
    geometry_unmatched,
    manual_reason,
    manual_note,
    geometry
  ) |>
  st_as_sf()

# a judicial district or circuit is the union of its counties, one row per county from the
# hand list, unioned at the agreement level like a regional department's members
judicial_members <- manual_judicial |>
  mutate(state_key = norm_state(state), county_key = norm_county(county)) |>
  left_join(
    counties_sf |>
      select(state_key, county_key, match_name = county, statefp, countyfp, geometry_vintage, geometry),
    by = c("state_key", "county_key")
  )
stopifnot(
  "every judicial-district county must match one census county" =
    all(!is.na(judicial_members$statefp)) && nrow(judicial_members) == nrow(manual_judicial)
)
judicial_agreements <- agreements |> filter(geometry_type == "polygon", jurisdiction_level == "Judicial District")
judicial_sf <- judicial_agreements |>
  inner_join(judicial_members |> select(agency, state, match_name, statefp, countyfp, geometry_vintage, source, geometry),
             by = c("agency", "state"), relationship = "many-to-many") |>
  transmute(
    agreement_id,
    match_name,
    match_type = "judicial_district_member_county",
    state_fips = statefp,
    county_fips = paste0(statefp, countyfp),
    geoid = county_fips,
    geometry_vintage,
    geometry_unmatched = FALSE,
    manual_reason = "judicial_district",
    manual_note = source,
    geometry
  ) |>
  st_as_sf()
# a district with no county list rides along unplaced, saying why
judicial_unmatched <- judicial_agreements |>
  anti_join(manual_judicial, by = c("agency", "state")) |>
  transmute(
    agreement_id,
    match_name = NA_character_,
    match_type = "unmatched_judicial_district_counties",
    state_fips = NA_character_,
    county_fips = NA_character_,
    geoid = NA_character_,
    geometry_vintage = NA_integer_,
    geometry_unmatched = TRUE,
    manual_reason = "judicial_district",
    manual_note = "no county list in inputs/manual-judicial-district-counties.csv"
  )
judicial_unmatched <- st_as_sf(
  judicial_unmatched,
  geometry = st_sfc(rep(list(st_geometrycollection()), nrow(judicial_unmatched)), crs = st_crs(county_agreements_sf))
)

bind_rows(county_agreements_sf, st_transform(judicial_sf, st_crs(county_agreements_sf)), judicial_unmatched) |>
  sf::st_write(dsn = "data/intermediate/match-county.parquet", driver = "Parquet", layer_options = "USE_PARQUET_GEO_TYPES=ONLY", delete_dsn = file.exists("data/intermediate/match-county.parquet"), quiet = TRUE)
