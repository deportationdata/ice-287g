library(readr)
library(arrow)

manual_points <- read_csv("data/manual-facility-points.csv")
manual_non_facility_polygons <- read_csv(
  "data/manual-non-facility-polygons.csv"
)

arrow::write_parquet(manual_points, "data/processed/manual_points.parquet")
arrow::write_parquet(
  manual_non_facility_polygons,
  "data/processed/manual_non_facility_polygons.parquet"
)
