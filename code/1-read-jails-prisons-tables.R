library(tidyverse)
library(sf)
library(tigris)
library(arrow)
library(tidygeocoder)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

YEAR <- 2024

state_xwalk <- arrow::read_parquet("data/state_xwalk.parquet")

hifld_prisons <- read_parquet_retry(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-prisons.parquet"
)

jails_prisons <- read_parquet_retry(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/jails_prisons.parquet"
)

hifld_prisons <- ensure_columns(
  hifld_prisons,
  list(
    population = NA_real_,
    county = NA_character_,
    county_fips = NA_character_,
    naics_code = NA_character_,
    naics_desc = NA_character_,
    source_url = NA_character_,
    source_date = NA_character_,
    website = NA_character_,
    secure_level = NA_character_,
    capacity = NA_real_
  )
)

jails_prisons <- ensure_columns(
  jails_prisons,
  list(
    bjs_facility_ID = NA_character_,
    operator_name = NA_character_,
    state_fips = NA_character_,
    county = NA_character_,
    county_fips = NA_character_,
    status = NA_character_,
    is_regional = NA_character_,
    is_private = NA_character_,
    hold_lt_1yr = NA_character_,
    hold_1yr_plus = NA_character_,
    hold_lt_72 = NA_character_,
    function_adult = NA_character_,
    function_work_release = NA_character_,
    function_reception = NA_character_,
    function_juvenile = NA_character_,
    function_medical = NA_character_,
    function_mental = NA_character_,
    function_alcohol = NA_character_,
    function_drug = NA_character_
  )
)

# ensure_columns backfills silently, and an upstream artifact that drops
# operator_name would quietly kill the facility matcher's operator tier
# (111 matches vanished on 2026-08-16); fail loudly instead
stopifnot(
  "remote jails_prisons.parquet carries no operator_name; the facility operator match tier would silently die" = any(
    !is.na(jails_prisons$operator_name) & jails_prisons$operator_name != ""
  )
)

# ICPSR jails records carry street addresses but no coordinates, so a
# jails-only facility match could never map; geocode them through the same
# ArcGIS path as the facilities repo, cached by address so only new records
# hit the API
jails_geocode_cache_path <- "data/jails-prisons-geocoded-arcgis.rds"

jails_prisons <- jails_prisons |>
  mutate(
    geocode_address = if_else(
      !is.na(address) & address != "" & !is.na(city) & !is.na(state),
      str_remove(
        str_squish(paste(address, city, state, coalesce(zip, ""), sep = ", ")),
        ",\\s*$"
      ),
      NA_character_
    )
  )

jails_geocode_cache <- if (file.exists(jails_geocode_cache_path)) {
  read_rds(jails_geocode_cache_path)
} else {
  tibble(address_full = character())
}

new_addresses <- setdiff(
  na.omit(unique(jails_prisons$geocode_address)),
  jails_geocode_cache$address_full
)

for (chunk in split(new_addresses, ceiling(seq_along(new_addresses) / 250))) {
  jails_geocode_cache <- bind_rows(
    jails_geocode_cache,
    tibble(address_full = chunk) |>
      geocode(
        address_full,
        method = "arcgis",
        lat = latitude,
        long = longitude,
        limit = 1,
        full_results = TRUE
      )
  ) |>
    distinct(address_full, .keep_all = TRUE)
  write_rds(jails_geocode_cache, jails_geocode_cache_path)
  message(nrow(jails_geocode_cache), " jail addresses geocoded")
}

# accept street-level matches only: a city or zip centroid would place the
# jail arbitrarily
jails_geocoded <- if ("attributes.Addr_type" %in% names(jails_geocode_cache)) {
  jails_geocode_cache |>
    filter(
      `attributes.Addr_type` %in%
        c("PointAddress", "StreetAddress", "Subaddress", "StreetInt")
    ) |>
    select(address_full, latitude, longitude)
} else {
  tibble(
    address_full = character(),
    latitude = numeric(),
    longitude = numeric()
  )
}

jails_prisons <-
  jails_prisons |>
  left_join(jails_geocoded, by = c("geocode_address" = "address_full")) |>
  select(-geocode_address)

counties_lookup <-
  tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  st_transform(4326) |>
  transmute(
    src_county_spatial = str_to_title(NAME),
    src_county_fips_spatial = paste0(STATEFP, COUNTYFP),
    county_key_spatial = norm_ori_county(NAME),
    geometry
  )

sources_raw <- bind_rows(
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
      src_population = as_numeric_scalar(population),
      src_hold_72 = NA,
      src_operator_name = NA_character_,
      src_state_fips = NA_character_,
      src_county = county,
      src_county_fips = county_fips,
      src_is_regional = NA_character_,
      src_is_private = NA_character_,
      src_hold_lt_1yr = NA_character_,
      src_hold_1yr_plus = NA_character_,
      src_hold_lt_72 = NA_character_,
      src_function_adult = NA_character_,
      src_function_work_release = NA_character_,
      src_function_reception = NA_character_,
      src_function_juvenile = NA_character_,
      src_function_medical = NA_character_,
      src_function_mental = NA_character_,
      src_function_alcohol = NA_character_,
      src_function_drug = NA_character_,
      src_secure_level = secure_level,
      src_capacity = as_numeric_scalar(capacity),
      src_naics_code = naics_code,
      src_naics_desc = naics_desc,
      src_source_url = source_url,
      src_source_date = source_date,
      src_website = website,
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
      src_type = "JAILS",
      src_status = status,
      src_population = NA_real_,
      src_hold_72 = hold_72,
      src_operator_name = operator_name,
      src_state_fips = state_fips,
      src_county = county,
      src_county_fips = county_fips,
      src_is_regional = is_regional,
      src_is_private = is_private,
      src_hold_lt_1yr = hold_lt_1yr,
      src_hold_1yr_plus = hold_1yr_plus,
      src_hold_lt_72 = hold_lt_72,
      src_function_adult = function_adult,
      src_function_work_release = function_work_release,
      src_function_reception = function_reception,
      src_function_juvenile = function_juvenile,
      src_function_medical = function_medical,
      src_function_mental = function_mental,
      src_function_alcohol = function_alcohol,
      src_function_drug = function_drug,
      src_secure_level = NA_character_,
      src_capacity = NA_real_,
      src_naics_code = NA_character_,
      src_naics_desc = NA_character_,
      src_source_url = NA_character_,
      src_source_date = NA_character_,
      src_website = NA_character_,
      src_latitude = latitude,
      src_longitude = longitude,
      src_date = date
    )
) |>
  mutate(source_row_id = row_number()) |>
  left_join(state_xwalk, by = c("src_state" = "state_abbr")) |>
  mutate(
    src_state_full = coalesce(state_full, src_state),
    state_key = norm_state(src_state_full),
    agency_key_src = norm_key(src_name)
  )

sources_counties_from_xy <- sources_raw |>
  filter(!is.na(src_latitude), !is.na(src_longitude)) |>
  st_as_sf(
    coords = c("src_longitude", "src_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  st_join(counties_lookup, join = st_within, left = TRUE) |>
  st_drop_geometry() |>
  distinct(source_row_id, .keep_all = TRUE) |>
  select(
    source_row_id,
    src_county_spatial,
    src_county_fips_spatial,
    county_key_spatial
  )

sources_tbl <- sources_raw |>
  left_join(sources_counties_from_xy, by = "source_row_id") |>
  mutate(
    src_county = coalesce(src_county, src_county_spatial),
    src_county_fips = coalesce(src_county_fips, src_county_fips_spatial),
    county_key = coalesce(norm_ori_county(src_county), county_key_spatial),
    src_state_fips = coalesce(src_state_fips, str_sub(src_county_fips, 1, 2))
  ) |>
  select(
    -source_row_id,
    -state_full,
    -src_county_spatial,
    -src_county_fips_spatial,
    -county_key_spatial
  )

jails_prisons_tbl <- sources_tbl |>
  transmute(
    source = src_dataset,
    source_rank = case_when(
      src_dataset == "hifld_prisons" ~ 2L,
      src_dataset == "jails_prisons" ~ 3L,
      TRUE ~ 99L
    ),
    detention_facility_code = src_id,
    facility_name = src_name,
    facility_address = src_address,
    facility_city = src_city,
    facility_county = src_county,
    facility_county_fips = src_county_fips,
    facility_state = src_state_full,
    facility_state_fips = str_sub(src_county_fips, 1, 2),
    facility_zip = src_zip,
    facility_address_full = NA_character_,
    facility_latitude = src_latitude,
    facility_longitude = src_longitude,
    facility_field_office = NA_character_,
    facility_source_type = src_type,
    facility_status = src_status,
    facility_operator_name = src_operator_name,
    facility_source_state_fips = src_state_fips,
    facility_source_county_fips = src_county_fips,
    facility_source_county = src_county,
    facility_population = src_population,
    facility_hold_72 = src_hold_72,
    facility_is_regional = src_is_regional,
    facility_is_private = src_is_private,
    facility_hold_lt_1yr = src_hold_lt_1yr,
    facility_hold_1yr_plus = src_hold_1yr_plus,
    facility_hold_lt_72 = src_hold_lt_72,
    facility_function_adult = src_function_adult,
    facility_function_work_release = src_function_work_release,
    facility_function_reception = src_function_reception,
    facility_function_juvenile = src_function_juvenile,
    facility_function_medical = src_function_medical,
    facility_function_mental = src_function_mental,
    facility_function_alcohol = src_function_alcohol,
    facility_function_drug = src_function_drug,
    facility_secure_level = src_secure_level,
    facility_capacity = src_capacity,
    facility_naics_code = src_naics_code,
    facility_naics_desc = src_naics_desc,
    facility_source_url = src_source_url,
    facility_source_date = src_source_date,
    facility_website = src_website,
    state_key,
    county_key,
    facility_key = agency_key_src
  )

arrow::write_parquet(
  jails_prisons_tbl,
  "data/jails_prisons_tbl.parquet"
)
