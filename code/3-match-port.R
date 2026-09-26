# Matches port-authority agreements to airport property polygons -> data/intermediate/match-port.parquet

library(tidyverse)
library(sf)

sf_use_s2(FALSE)

source("code/functions.R")

YEAR <- 2024

agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")
manual_ports <- arrow::read_parquet("data/intermediate/manual-port-airports.parquet")
manual_points <- arrow::read_parquet("data/intermediate/manual-facility-points.parquet")
airport_boundaries <- st_read("data/intermediate/reference-airport-boundaries.parquet", quiet = TRUE)

ports <- agreements |> filter(geometry_type == "polygon", jurisdiction_level == "Port")

counties_ref <- counties_reference(YEAR)

# an airport's county is the one it overlaps most
airports <- airport_boundaries |>
  filter(type == "AIRPORT", !is.na(faa_code))
airport_counties <- airports |>
  select(faa_code) |>
  st_transform(3857) |>
  st_intersection(counties_ref |> transmute(county_fips = geoid, geometry) |> st_transform(3857)) |>
  mutate(overlap_area = st_area(geometry)) |>
  st_drop_geometry() |>
  group_by(faa_code) |>
  arrange(desc(overlap_area), county_fips, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(faa_code, county_fips)

# one row per airport, unioned per agreement like a regional department's members
members <- manual_ports |>
  left_join(
    airports |>
      select(faa_code, match_name = name, state_fips, geometry) |>
      left_join(airport_counties, by = "faa_code"),
    by = "faa_code"
  )
stopifnot(
  "every listed airport must match one boundary polygon" =
    all(!is.na(members$match_name)) && nrow(members) == nrow(manual_ports)
)

port_sf <- ports |>
  inner_join(members, by = c("agency", "state")) |>
  st_as_sf() |>
  transmute(
    agreement_id,
    match_name,
    match_type = "airport_boundary",
    state_fips,
    county_fips,
    place_fips = NA_character_,
    geoid = NA_character_,
    geometry_vintage = 2016L,
    geometry_unmatched = FALSE,
    manual_reason = "port_airport",
    manual_note = note,
    geometry
  )

# an authority with no airport list keeps its airport's reference point
fallback <- ports |>
  anti_join(manual_ports, by = c("agency", "state")) |>
  left_join(
    manual_points |>
      filter(is.na(county) | county == "") |>
      select(agency, state, match_name = facility_name, latitude, longitude, manual_reason = reason, manual_note = note),
    by = c("agency", "state")
  )
fallback_points <- fallback |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_join(counties_ref |> transmute(state_fips = statefp, county_fips = geoid, geometry)) |>
  # a point on a county boundary can hit two polygons under planar containment
  distinct(agreement_id, .keep_all = TRUE) |>
  transmute(
    agreement_id,
    match_name,
    match_type = "port_reference_point",
    state_fips,
    county_fips,
    place_fips = NA_character_,
    geoid = NA_character_,
    geometry_vintage = NA_integer_,
    geometry_unmatched = FALSE,
    manual_reason,
    manual_note,
    geometry
  )
# keep-all: a port with neither rides along with an empty geometry
fallback_unmatched <- fallback |>
  filter(is.na(latitude) | is.na(longitude)) |>
  transmute(
    agreement_id,
    match_name = NA_character_,
    match_type = "unmatched_port_airport",
    state_fips = NA_character_,
    county_fips = NA_character_,
    place_fips = NA_character_,
    geoid = NA_character_,
    geometry_vintage = NA_integer_,
    geometry_unmatched = TRUE,
    manual_reason = "port_airport",
    manual_note = "no airport in inputs/manual-port-airports.csv or inputs/manual-facility-points.csv"
  )
fallback_unmatched <- st_as_sf(
  fallback_unmatched,
  geometry = st_sfc(rep(list(st_geometrycollection()), nrow(fallback_unmatched)), crs = st_crs(4326))
)

port_layer <- bind_rows(port_sf, fallback_points, fallback_unmatched) |>
  st_as_sf() |>
  st_transform(4326)

stopifnot(
  "every port agreement appears in the layer, fanning out only over its listed airports" =
    setequal(port_layer$agreement_id, ports$agreement_id) &&
      !anyDuplicated(port_layer$agreement_id[port_layer$match_type != "airport_boundary"])
)

sf::st_write(port_layer, dsn = "data/intermediate/match-port.parquet", driver = "Parquet", layer_options = "USE_PARQUET_GEO_TYPES=ONLY", delete_dsn = file.exists("data/intermediate/match-port.parquet"), quiet = TRUE)
