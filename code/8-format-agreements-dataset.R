library(tidyverse)
library(sf)
library(arrow)

source("code/functions.R")

non_facility_agreements_sf <- read_sf_parquet(
  "data/non_facility_agreements_sf.parquet"
)

facility_agreements_sf <- read_sf_parquet(
  "data/facility_agreements_sf.parquet"
)

all_agreements_sf <-
  bind_rows(
    non_facility_agreements_sf,
    facility_agreements_sf |>
      st_transform(4326) |>
      mutate(match_layer = "facility")
  ) |>
  filter(!is.na(geometry)) |>
  st_make_valid() |>
  st_transform(4326)

write_sf_parquet(
  all_agreements_sf,
  "data/all_agreements_sf.parquet"
)

all_agreements_sf |>
  group_by(
    agency,
    state,
    county,
    signed,
    moa,
    addendum,
    ORI9,
    support_type,
    agency_level,
    geom_class,
    state_fips
  ) |>
  summarize(
    match_layer = sort(unique(match_layer)),
    geometry = st_union(geometry),
    .groups = "drop"
  ) |>
  select(
    # Preserve the source spreadsheet order, followed by derived/spatial fields.
    state,
    agency,
    agency_level,
    county,
    support_type,
    signed,
    moa,
    addendum,
    ORI9,
    state_fips,
    geom_class,
    match_layer,
    geometry
  ) |>
  write_sf_parquet(
    "data/agreement-level-sf.parquet"
  )
