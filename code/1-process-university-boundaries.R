library(sf)

source("code/functions.R")

university_boundaries <- st_read(
  "inputs/colleges-and-universities-campuses/CollegeUniversityCampuses.shp"
)

write_sf_parquet(
  university_boundaries,
  "data/university_boundaries.parquet"
)
