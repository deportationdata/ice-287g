library(tidyverse)
library(sf)
library(tigris)
library(arrow)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

YEAR <- 2024

state_xwalk <- arrow::read_parquet("data/state_xwalk.parquet")

# use type (constable, local state agency, ...)
hifld <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-local-law-enforcement-facilities.parquet"
)

# use type column
hifld_prisons <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-prisons.parquet"
)

# use type column
jails_prisons <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/jails_prisons.parquet"
)

counties_lookup <- tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  st_transform(4326) |>
  transmute(
    src_county = str_to_title(NAME),
    src_county_fips = paste0(STATEFP, COUNTYFP),
    county_key_spatial = norm_place(NAME),
    geometry
  )

hifld_raw <- bind_rows(
  hifld |>
    transmute(
      src_dataset = "hifld",
      src_id = as.character(hifld_id),
      src_name = str_squish(name),
      src_address = address,
      src_city = str_squish(city),
      src_state = str_squish(state),
      src_zip = zip,
      src_type = type,
      src_status = status,
      src_population = NA_real_,
      src_hold_72 = NA,
      src_latitude = latitude,
      src_longitude = longitude,
      src_date = date
    ),
  hifld_prisons |>
    transmute(
      src_dataset = "hifld_prisons",
      src_id = as.character(hifld_id),
      src_name = str_squish(name),
      src_address = address,
      src_city = str_squish(city),
      src_state = str_squish(state),
      src_zip = zip,
      src_type = type,
      src_status = status,
      src_population = population,
      src_hold_72 = NA,
      src_latitude = latitude,
      src_longitude = longitude,
      src_date = date
    ),
  jails_prisons |>
    transmute(
      src_dataset = "jails_prisons",
      src_id = as.character(bjs_facility_ID),
      src_name = str_squish(name),
      src_address = address,
      src_city = str_squish(city),
      src_state = str_squish(state),
      src_zip = zip,
      src_type = NA_character_,
      src_status = NA_character_,
      src_population = NA_real_,
      src_hold_72 = hold_72,
      src_latitude = NA_real_,
      src_longitude = NA_real_,
      src_date = date
    )
) |>
  mutate(hifld_row_id = row_number()) |>
  left_join(state_xwalk, by = c("src_state" = "state_abbr")) |>
  mutate(
    src_state_full = coalesce(state_full, src_state),
    state_key = norm_state(src_state_full),
    agency_key_src = norm_key(src_name)
  )

hifld_counties_from_xy <- hifld_raw |>
  filter(!is.na(src_latitude), !is.na(src_longitude)) |>
  st_as_sf(
    coords = c("src_longitude", "src_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  st_join(counties_lookup, join = st_within, left = TRUE) |>
  st_drop_geometry() |>
  distinct(hifld_row_id, .keep_all = TRUE) |>
  select(
    hifld_row_id,
    src_county,
    src_county_fips,
    county_key_spatial
  )

hifld_tbl <- hifld_raw |>
  left_join(hifld_counties_from_xy, by = "hifld_row_id") |>
  mutate(county_key = county_key_spatial) |>
  select(
    -hifld_row_id,
    -state_full,
    -county_key_spatial
  )

hifld_facility_tbl <- hifld_tbl |>
  transmute(
    source = src_dataset,
    source_rank = case_when(
      src_dataset == "hifld" ~ 2L,
      src_dataset == "hifld_prisons" ~ 3L,
      src_dataset == "jails_prisons" ~ 4L,
      TRUE ~ 99L
    ),
    detention_facility_code = src_id,
    facility_name = src_name,
    facility_address = src_address,
    facility_city = src_city,
    facility_county = src_county,
    facility_county_fips = src_county_fips,
    facility_state = src_state,
    facility_state_fips = str_sub(src_county_fips, 1, 2),
    facility_zip = src_zip,
    facility_address_full = NA_character_,
    facility_latitude = src_latitude,
    facility_longitude = src_longitude,
    facility_field_office = NA_character_,
    state_key,
    county_key,
    facility_key = agency_key_src
  )

arrow::write_parquet(
  hifld_facility_tbl,
  "data/hifld_facility_tbl.parquet"
)
