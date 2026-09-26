# Airport property polygons -> data/intermediate/reference-airport-boundaries.parquet
library(tidyverse)
library(sf)

source("code/functions.R")

sf_use_s2(FALSE)

# FGDL's 2016 aviation facility boundaries cover Florida only, where every port authority ICE lists is
airport_boundaries <- st_read(
  "inputs/2016-florida-aviation-facility-boundaries/gc_aviationbnd_jan16.shp",
  quiet = TRUE
) |>
  transmute(
    name = str_squish(NAME),
    # the FAA location identifier; heliports and seaplane bases can share an airport's
    faa_code = na_if(str_squish(ACODE), ""),
    site_id = USID,
    type = TYPE,
    status = STATUS,
    county = str_to_title(COUNTY),
    state_fips = "12"
  ) |>
  st_transform(4326)

sf::st_write(airport_boundaries, dsn = "data/intermediate/reference-airport-boundaries.parquet", driver = "Parquet", layer_options = "USE_PARQUET_GEO_TYPES=ONLY", delete_dsn = file.exists("data/intermediate/reference-airport-boundaries.parquet"), quiet = TRUE)
