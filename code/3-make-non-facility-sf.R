# Stack the non-facility geometry layers -> data/non-facility-sf.parquet
library(tidyverse)
library(sf)

source("code/functions.R")

# list names become match_layer; the literals drive required_fips() in step 6
non_facility_layers <- list(
  state = read_sf_parquet("data/state-sf.parquet"),
  county = read_sf_parquet("data/county-sf.parquet"),
  university = read_sf_parquet("data/university-sf.parquet"),
  municipal = read_sf_parquet("data/municipal-sf.parquet"),
  pa_constable = read_sf_parquet("data/pa-constable-sf.parquet")
)

stopifnot(
  "every non-facility layer must arrive in EPSG:4326" = all(
    map_lgl(non_facility_layers, \(layer) st_crs(layer) == st_crs(4326))
  )
)

# unplaceable agreements ride along with empty geometries so none is dropped
unknown_rows <- arrow::read_parquet("data/agreements.parquet") |>
  filter(geom_class == "unknown") |>
  transmute(
    agreement_id,
    match_name = NA_character_,
    match_type = NA_character_,
    needs_review
  )
non_facility_layers$unknown <- st_as_sf(
  unknown_rows,
  geometry = st_sfc(
    rep(list(st_geometrycollection()), nrow(unknown_rows)),
    crs = 4326
  )
)

non_facility_sf <- bind_rows(non_facility_layers, .id = "match_layer") |>
  st_as_sf()

write_sf_parquet(non_facility_sf, "data/non-facility-sf.parquet")
