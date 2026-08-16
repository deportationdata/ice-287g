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

# an agreement can span several matched features (a DOC's facilities sit in
# many counties), so codes are kept only when they identify a single area
single_or_na <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 1) ux else NA_character_
}

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
    county_fips = single_or_na(county_fips),
    place_fips = single_or_na(place_fips),
    geoid = single_or_na(geoid),
    vtd_code = single_or_na(vtd_code),
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
    county_fips,
    place_fips,
    geoid,
    vtd_code,
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
