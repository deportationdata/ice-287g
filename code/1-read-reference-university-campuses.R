# Campus boundary polygons -> data/intermediate/reference-university-campuses.parquet
library(tidyverse)
library(sf)

source("code/functions.R")

sf_use_s2(FALSE)

university_boundaries <- st_read(
  "inputs/colleges-and-universities-campuses/CollegeUniversityCampuses.shp"
) |>
  transmute(
    name = str_squish(NAME),
    state = str_to_upper(str_squish(STATE)),
    # non-FIPS placeholders ("NOT AVAILABLE") drop
    county_fips = if_else(
      str_detect(COUNTYFIPS, "^[0-9]{5}$"),
      COUNTYFIPS,
      NA_character_
    )
  ) |>
  st_transform(4326)

sf::st_write(university_boundaries, dsn = "data/intermediate/reference-university-campuses.parquet", driver = "Parquet", layer_options = "USE_PARQUET_GEO_TYPES=ONLY", delete_dsn = file.exists("data/intermediate/reference-university-campuses.parquet"), quiet = TRUE)
