library(tidyverse)
library(sf)
library(arrow)

sf_use_s2(FALSE)

source("code/functions.R")

non_facility_agreements_sf <- read_sf_parquet(
  "data/non_facility_agreements_sf.parquet"
)

facility_agreements_sf <- read_sf_parquet(
  "data/facility_agreements_sf.parquet"
)

# every agreement is kept, matched or not: unmatched rows carry an empty
# geometry and needs_review = TRUE; both inputs are already EPSG:4326
all_agreements_sf <-
  bind_rows(
    non_facility_agreements_sf,
    facility_agreements_sf |>
      mutate(match_layer = "facility")
  ) |>
  st_make_valid()

write_sf_parquet(
  all_agreements_sf,
  "data/all_agreements_sf.parquet"
)

all_agreements_sf |>
  # county is in the keys because agency names recycle across counties within
  # a state (two same-named municipal PDs must not collapse into one row)
  group_by(
    agency,
    state,
    county,
    support_type,
    agency_level,
    geom_class,
    has_addendum,
    moa_pending,
    state_fips
  ) |>
  summarize(
    match_layer = paste(sort(unique(match_layer)), collapse = ";"),
    geometry = st_union(geometry),
    .groups = "drop"
  ) |>
  write_sf_parquet(
    "data/agreement-level-sf.parquet"
  )
