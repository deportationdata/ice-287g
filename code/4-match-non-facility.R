# Stack the non-facility geometry layers -> data/intermediate/match-non-facility.parquet
library(tidyverse)
library(sf)

source("code/functions.R")

# list names become match_layer; the literals drive required_fips() in step 6
non_facility_layers <- list(
  state = st_read("data/intermediate/match-state.parquet", quiet = TRUE),
  county = st_read("data/intermediate/match-county.parquet", quiet = TRUE),
  university = st_read("data/intermediate/match-university.parquet", quiet = TRUE),
  # PA constables are municipal (borough, township or ward)
  municipal = bind_rows(
    st_read("data/intermediate/match-municipal.parquet", quiet = TRUE),
    st_read("data/intermediate/match-pa-constable.parquet", quiet = TRUE)
  ),
  port = st_read("data/intermediate/match-port.parquet", quiet = TRUE)
)

stopifnot(
  "every non-facility layer must arrive in EPSG:4326" = all(
    map_lgl(non_facility_layers, \(layer) st_crs(layer) == st_crs(4326))
  )
)

# unplaceable agreements ride along with empty geometries so none is dropped: agreements of
# no known class, and constable precincts, for which no boundary layer exists
agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")
empty_layer <- function(rows) {
  st_as_sf(rows, geometry = st_sfc(rep(list(st_geometrycollection()), nrow(rows)), crs = 4326))
}
non_facility_layers$unknown <- agreements |>
  filter(is.na(geometry_type)) |>
  transmute(agreement_id, match_name = NA_character_, match_type = NA_character_, geometry_unmatched = TRUE) |>
  empty_layer()
non_facility_layers$precinct <- agreements |>
  filter(geometry_type == "polygon", jurisdiction_level == "Constable District") |>
  transmute(agreement_id, match_name = NA_character_, match_type = "no_precinct_boundary_layer", geometry_unmatched = TRUE) |>
  empty_layer()

non_facility_sf <- bind_rows(non_facility_layers, .id = "match_layer") |>
  st_as_sf() |>
  mutate(across(c(geometry_unmatched, ambiguous_candidates, type_mismatch,
                  university_address_mismatch, university_county_mismatch), \(x) coalesce(x, FALSE)))

stopifnot(
  "every placed census-unit feature carries a geoid" =
    !any(!st_is_empty(non_facility_sf$geometry) & is.na(non_facility_sf$geoid) &
           !non_facility_sf$match_layer %in% c("university", "port")),
  "an empty geometry is flagged unmatched and nothing else is" =
    all(st_is_empty(non_facility_sf$geometry) == non_facility_sf$geometry_unmatched)
)

sf::st_write(non_facility_sf, dsn = "data/intermediate/match-non-facility.parquet", driver = "Parquet", layer_options = "USE_PARQUET_GEO_TYPES=ONLY", delete_dsn = file.exists("data/intermediate/match-non-facility.parquet"), quiet = TRUE)
