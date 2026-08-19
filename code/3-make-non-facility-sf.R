library(tidyverse)
library(sf)

source("code/functions.R")

# list names become match_layer via .id; the exact literals drive the
# required_fips() dispatch in 6-make-missing-identifiers.R, and the bind order
# fixes row order in the combined file
non_facility_layers <- list(
  state = read_sf_parquet("data/state-sf.parquet"),
  county = read_sf_parquet("data/county-sf.parquet"),
  university = read_sf_parquet("data/university-sf.parquet"),
  municipal = read_sf_parquet("data/municipal-sf.parquet"),
  pa_constable = read_sf_parquet("data/pa-constable-sf.parquet")
)

# layer scripts write EPSG:4326; a mislabeled layer would silently shift every
# geometry, so fail fast instead of re-transforming here
stopifnot(
  "every non-facility layer must arrive in EPSG:4326" = all(
    map_lgl(non_facility_layers, \(layer) st_crs(layer) == st_crs(4326))
  )
)

non_facility_sf <- bind_rows(non_facility_layers, .id = "match_layer") |>
  st_as_sf()

write_sf_parquet(non_facility_sf, "data/non-facility-sf.parquet")
