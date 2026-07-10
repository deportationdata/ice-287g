library(sf)

source("code/functions.R")

university_boundaries <- st_read(
  "inputs/colleges-and-universities-campuses/CollegeUniversityCampuses.shp"
)

# only NAME, STATE and the geometry are used downstream (5-make-university-sf.R)
write_sf_parquet(
  university_boundaries[, c("NAME", "STATE")],
  "data/university_boundaries.parquet"
)
