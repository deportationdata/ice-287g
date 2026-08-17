library(readr)
library(arrow)

manual_points <- read_csv("inputs/manual-facility-points.csv")
manual_non_facility_polygons <- read_csv(
  "inputs/manual-non-facility-polygons.csv"
)

arrow::write_parquet(manual_points, "data/manual_points.parquet")
arrow::write_parquet(
  manual_non_facility_polygons,
  "data/manual_non_facility_polygons.parquet"
)
