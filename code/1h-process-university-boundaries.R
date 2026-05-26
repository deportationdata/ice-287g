library(sf)

source("code/functions.R")

university_boundaries <- st_read(
  "data/colleges-and-universities-campuses/CollegeUniversityCampuses.shp"
)

write_sf_parquet(
  university_boundaries,
  "data/processed/university_boundaries.parquet"
)
