# Manual override CSVs -> data/manual-{points,polygons,regional}.parquet
library(tidyverse)

# all-character: readr would guess logical for empty columns and drop zip zeros
manual_points <- read_csv(
  "inputs/manual-facility-points.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(
    agency,
    state,
    county,
    facility_name = manual_facility_name,
    # CNMI's positive longitude is correct (Saipan is east of the meridian)
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    reason = manual_reason,
    note = manual_note
  )

# match_layer/match_name are consumed verbatim by the layer matchers
manual_polygons <- read_csv(
  "inputs/manual-non-facility-polygons.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(
    agency,
    state,
    county,
    match_layer = manual_match_layer,
    match_name = manual_match_name,
    reason = manual_reason,
    note = manual_note
  )

# a regional department's member municipalities, one per row
manual_regional <- read_csv(
  "inputs/manual-regional-municipalities.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(
    agency,
    state,
    county,
    municipality,
    # township names recur across counties (Morris Township: Greene, Washington)
    municipality_county,
    source,
    note
  )

arrow::write_parquet(manual_points, "data/manual-points.parquet")
arrow::write_parquet(manual_polygons, "data/manual-polygons.parquet")
arrow::write_parquet(manual_regional, "data/manual-regional.parquet")
