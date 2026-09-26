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

# a judicial district's or circuit's counties, one per row, with the source for each
manual_judicial <- read_csv(
  "inputs/manual-judicial-district-counties.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(agency, state, county, source, note)

# a port authority's airports, one per row, keyed on the FAA location identifier
manual_ports <- read_csv(
  "inputs/manual-port-airports.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(agency, state, faa_code, airport, source, note)

arrow::write_parquet(manual_points, "data/intermediate/manual-facility-points.parquet")
arrow::write_parquet(manual_ports, "data/intermediate/manual-port-airports.parquet")
arrow::write_parquet(manual_polygons, "data/intermediate/manual-non-facility-polygons.parquet")
arrow::write_parquet(manual_regional, "data/intermediate/manual-regional-municipalities.parquet")
arrow::write_parquet(manual_judicial, "data/intermediate/manual-judicial-district-counties.parquet")
