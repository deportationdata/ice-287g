library(tidyverse)
library(sf)
library(tigris)
library(arrow)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

YEAR <- 2024

as_numeric_scalar <- function(x) {
  suppressWarnings(as.numeric(x))
}

# columns the shared transmute below cannot proceed without; a remote schema
# change should fail loudly here rather than produce silently-empty output
assert_columns <- function(x, cols, label) {
  missing <- setdiff(cols, names(x))

  if (length(missing) > 0) {
    stop(
      label,
      " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  x
}

ensure_columns <- function(x, defaults) {
  for (col in names(defaults)) {
    if (!col %in% names(x)) {
      x[[col]] <- defaults[[col]]
    }
  }

  x
}

state_xwalk <- arrow::read_parquet("data/state_xwalk.parquet")

hifld <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-local-law-enforcement-facilities.parquet"
) |>
  assert_columns(
    c("hifld_id", "name", "state", "latitude", "longitude", "date"),
    label = "hifld-local-law-enforcement-facilities.parquet"
  ) |>
  ensure_columns(
    list(
      address = NA_character_,
      city = NA_character_,
      zip = NA_character_,
      type = NA_character_,
      status = NA_character_,
      population = NA_real_,
      county = NA_character_,
      county_fips = NA_character_,
      naics_code = NA_character_,
      naics_desc = NA_character_,
      source_url = NA_character_,
      source_date = NA_character_,
      website = NA_character_,
      ci_id = NA_character_,
      csllea08id = NA_character_,
      subtype1 = NA_character_,
      subtype2 = NA_character_,
      tribal = NA_character_
    )
  )

hifld_prisons <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-prisons.parquet"
) |>
  assert_columns(
    c("hifld_id", "name", "state", "latitude", "longitude", "date"),
    label = "hifld-prisons.parquet"
  ) |>
  ensure_columns(
    list(
      address = NA_character_,
      city = NA_character_,
      zip = NA_character_,
      type = NA_character_,
      status = NA_character_,
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

jails_prisons <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/jails_prisons.parquet"
) |>
  assert_columns(
    c("name", "state", "date"),
    label = "jails_prisons.parquet"
  ) |>
  ensure_columns(
    list(
      bjs_facility_ID = NA_character_,
      operator_name = NA_character_,
      address = NA_character_,
      city = NA_character_,
      zip = NA_character_,
      hold_72 = NA_character_,
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

counties_lookup <- tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  st_transform(4326) |>
  transmute(
    src_county_spatial = str_to_title(NAME),
    src_county_fips_spatial = paste0(STATEFP, COUNTYFP),
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
      src_secure_level = NA_character_,
      src_capacity = NA_real_,
      src_naics_code = naics_code,
      src_naics_desc = naics_desc,
      src_source_url = source_url,
      src_source_date = source_date,
      src_website = website,
      src_ci_id = ci_id,
      src_csllea08id = csllea08id,
      src_subtype1 = subtype1,
      src_subtype2 = subtype2,
      src_tribal = tribal,
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
      src_ci_id = NA_character_,
      src_csllea08id = NA_character_,
      src_subtype1 = NA_character_,
      src_subtype2 = NA_character_,
      src_tribal = NA_character_,
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
      src_ci_id = NA_character_,
      src_csllea08id = NA_character_,
      src_subtype1 = NA_character_,
      src_subtype2 = NA_character_,
      src_tribal = NA_character_,
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
  st_join(counties_lookup, join = st_intersects, left = TRUE) |>
  st_drop_geometry() |>
  distinct(hifld_row_id, .keep_all = TRUE) |>
  select(
    hifld_row_id,
    src_county_spatial,
    src_county_fips_spatial,
    county_key_spatial
  )

hifld_tbl <- hifld_raw |>
  left_join(hifld_counties_from_xy, by = "hifld_row_id") |>
  mutate(
    src_county = coalesce(src_county, src_county_spatial),
    src_county_fips = coalesce(src_county_fips, src_county_fips_spatial),
    county_key = coalesce(norm_place(src_county), county_key_spatial),
    src_state_fips = coalesce(src_state_fips, str_sub(src_county_fips, 1, 2))
  ) |>
  select(
    -hifld_row_id,
    -state_full,
    -src_county_spatial,
    -src_county_fips_spatial,
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
    facility_name = src_name,
    facility_address = src_address,
    facility_city = src_city,
    facility_county = src_county,
    facility_county_fips = src_county_fips,
    facility_state = src_state,
    facility_state_fips = str_sub(src_county_fips, 1, 2),
    facility_zip = src_zip,
    facility_latitude = src_latitude,
    facility_longitude = src_longitude,
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
    facility_ci_id = src_ci_id,
    facility_csllea08id = src_csllea08id,
    facility_subtype1 = src_subtype1,
    facility_subtype2 = src_subtype2,
    facility_tribal = src_tribal,
    state_key,
    county_key,
    facility_key = agency_key_src
  )

arrow::write_parquet(
  hifld_facility_tbl,
  "data/hifld_facility_tbl.parquet"
)
