library(tidyverse)
library(sf)
library(arrow)

source("code/functions.R")

facilities <- read_sf_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/facilities-latest-sf.parquet"
)

state_xwalk <- arrow::read_parquet("data/state_xwalk.parquet")

facilities_tbl <- facilities |>
  st_drop_geometry() |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  # the source `state` column holds two-letter abbreviations; agencies and the
  # HIFLD tables key on full state names, so resolve through the crosswalk
  left_join(
    state_xwalk,
    by = join_by(state == state_abbr)
  ) |>
  transmute(
    source = "facilities",
    source_rank = 1L,
    facility_name = str_squish(name),
    facility_address = address,
    facility_city = str_squish(city),
    facility_county = str_to_title(str_squish(county)),
    facility_county_fips = as.character(county_fips_code),
    facility_state = coalesce(state_full, str_to_title(str_squish(state))),
    facility_state_fips = as.character(state_fips_code),
    facility_zip = zip,
    facility_latitude = latitude,
    facility_longitude = longitude
  ) |>
  mutate(
    state_key = norm_state(facility_state),
    county_key = norm_place(facility_county),
    facility_key = norm_key(facility_name)
  )

arrow::write_parquet(facilities_tbl, "data/facilities_tbl.parquet")
