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
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  # state arrives as a two-letter abbreviation; key on the full name so the
  # join to the agreements (keyed on "texas", not "tx") can succeed
  left_join(state_xwalk, by = c("state" = "state_abbr")) |>
  transmute(
    source = "facilities",
    source_rank = 1L,
    detention_facility_code = as.character(detention_facility_code),
    facility_name = str_squish(name),
    facility_address = address,
    facility_city = str_squish(city),
    facility_county = str_to_title(str_squish(county)),
    facility_county_fips = as.character(county_fips_code),
    facility_state = coalesce(state_full, str_to_title(str_squish(state))),
    facility_state_fips = as.character(state_fips_code),
    facility_zip = zip,
    facility_address_full = address_full,
    facility_latitude = latitude,
    facility_longitude = longitude,
    facility_field_office = field_office,
    geometry
  ) |>
  mutate(
    state_key = norm_state(facility_state),
    county_key = norm_ori_county(facility_county),
    facility_key = norm_key(facility_name)
  )

write_sf_parquet(facilities_tbl, "data/facilities_tbl.parquet")
