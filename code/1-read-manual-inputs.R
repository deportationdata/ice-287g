library(tidyverse)

# all-character col_types: readr would otherwise parse all-empty columns as
# logical and drop leading zeros from zips
manual_points <- read_csv(
  "inputs/manual-facility-points.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(
    agency,
    state,
    county,
    facility_name = manual_facility_name,
    # CNMI's positive longitude (145.7) is correct: Saipan sits east of the
    # prime meridian, so do not "fix" the sign
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    reason = manual_reason,
    note = manual_note
  )

# match_layer/match_name are consumed verbatim by the layer matchers ("Hopewell
# City" targets the county layer for a VA independent city); agency/state/county
# key against the cleaned agreements values
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

# a regional department polices several municipalities at once, which no single
# boundary name can express, so its members are listed one per row with the
# source that documents the membership
manual_regional <- read_csv(
  "inputs/manual-regional-municipalities.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(
    agency,
    state,
    county,
    municipality,
    # members can sit in different counties, and the same township name recurs
    # across them (Morris Township is in both Greene and Washington)
    municipality_county,
    source,
    note
  )

arrow::write_parquet(manual_points, "data/manual-points.parquet")
arrow::write_parquet(manual_polygons, "data/manual-polygons.parquet")
arrow::write_parquet(manual_regional, "data/manual-regional.parquet")
