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

# unmatched agreements ride along with empty geometries; nothing is dropped
all_agreements_sf <-
  bind_rows(
    non_facility_agreements_sf,
    facility_agreements_sf |>
      st_transform(4326) |>
      mutate(match_layer = "facility")
  ) |>
  st_make_valid() |>
  st_transform(4326)

write_sf_parquet(
  all_agreements_sf,
  "data/all_agreements_sf.parquet"
)

agreement_level_sf <- all_agreements_sf |>
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
    match_layer = paste(sort(unique(match_layer)), collapse = "+"),
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
    geometry_type = geom_class,
    match_layer,
    geometry
  )

# one row per agreement in the source spreadsheet
agencies_all <- arrow::read_parquet("data/agencies_all.parquet")
stopifnot(nrow(agreement_level_sf) == nrow(agencies_all))

write_sf_parquet(
  agreement_level_sf,
  "data/agreement-level-sf.parquet"
)
