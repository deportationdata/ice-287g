library(tidyverse)
library(sf)

source("code/functions.R")

sf_use_s2(FALSE)

# campus polygons arrive in ESRI Web Mercator; 4326 at write means consumers
# need no CRS override
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

write_sf_parquet(university_boundaries, "data/university-boundaries.parquet")
