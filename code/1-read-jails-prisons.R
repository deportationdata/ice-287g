library(tidyverse)
library(sf)
library(tigris)
library(tidygeocoder)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

YEAR <- 2024

state_xwalk <- arrow::read_parquet("data/state-xwalk.parquet")

hifld_prisons <- read_parquet_retry(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-prisons.parquet"
)

jails_prisons <- read_parquet_retry(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/jails_prisons.parquet"
)

stopifnot(
  "remote hifld-prisons.parquet no longer carries the columns this script reads" = all(
    c(
      "hifld_id",
      "name",
      "address",
      "city",
      "state",
      "zip",
      "type",
      "latitude",
      "longitude"
    ) %in%
      names(hifld_prisons)
  ),
  "remote jails_prisons.parquet no longer carries the columns this script reads" = all(
    c("bjs_facility_ID", "name", "address", "city", "state", "zip") %in%
      names(jails_prisons)
  )
)

# the facility matcher's operator tier depends on this column; without the
# guard an upstream artifact that drops it silently loses ~100 matches
stopifnot(
  "remote jails_prisons.parquet carries no operator_name; the facility operator match tier would silently die" = any(
    !is.na(jails_prisons$operator_name) & jails_prisons$operator_name != ""
  )
)

# ICPSR jails records carry street addresses but no coordinates, so a
# jails-only match could never map. The cache is keyed by address and
# append-only: only new records hit the API
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

jails_prisons <- jails_prisons |>
  left_join(jails_geocoded, by = c("geocode_address" = "address_full")) |>
  select(-geocode_address)

counties_lookup <- tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  st_transform(4326) |>
  transmute(
    county_spatial = str_to_title(NAME),
    county_fips_spatial = paste0(STATEFP, COUNTYFP),
    county_key_spatial = norm_ori_county(NAME),
    geometry
  )

# neither source ships county fields, so these start NA and the st_within fill
# below supplies them; a source-named county would take precedence
jails_prisons_tbl <- bind_rows(
  hifld_prisons |>
    transmute(
      source = "hifld_prisons",
      source_rank = 2L,
      source_id = as.character(hifld_id),
      facility_name = str_squish(name),
      facility_address = address,
      facility_city = str_squish(city),
      facility_county = NA_character_,
      facility_state = str_squish(state),
      facility_zip = zip,
      county_fips = NA_character_,
      type,
      latitude,
      longitude,
      facility_key = norm_key(facility_name)
    ),
  jails_prisons |>
    transmute(
      source = "jails_prisons",
      source_rank = 3L,
      source_id = as.character(bjs_facility_ID),
      facility_name = str_squish(name),
      facility_address = address,
      facility_city = str_squish(city),
      facility_county = NA_character_,
      facility_state = str_squish(state),
      facility_zip = zip,
      county_fips = NA_character_,
      type = "JAILS",
      facility_operator_name = operator_name,
      latitude,
      longitude,
      facility_key = norm_key(facility_name)
    )
) |>
  mutate(source_row_id = row_number()) |>
  left_join(
    state_xwalk |> select(state_abbr, state_full),
    by = c("facility_state" = "state_abbr")
  ) |>
  mutate(
    facility_state = coalesce(state_full, facility_state),
    state_key = norm_state(facility_state)
  ) |>
  select(-state_full)

counties_from_xy <- jails_prisons_tbl |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  st_join(counties_lookup, join = st_within, left = TRUE) |>
  st_drop_geometry() |>
  distinct(source_row_id, .keep_all = TRUE) |>
  select(source_row_id, county_spatial, county_fips_spatial, county_key_spatial)

# regional jails list several counties in one field and the operator tier tests
# membership against county_key, so a source-named county passes through
# norm_ori_county intact instead of being replaced by the single spatial county
jails_prisons_tbl <- jails_prisons_tbl |>
  left_join(counties_from_xy, by = "source_row_id") |>
  mutate(
    facility_county = coalesce(facility_county, county_spatial),
    county_fips = coalesce(county_fips, county_fips_spatial),
    county_key = coalesce(norm_ori_county(facility_county), county_key_spatial),
    state_fips = str_sub(county_fips, 1, 2)
  ) |>
  select(
    source,
    source_rank,
    source_id,
    facility_name,
    facility_address,
    facility_city,
    facility_county,
    facility_state,
    facility_zip,
    state_fips,
    county_fips,
    type,
    facility_operator_name,
    latitude,
    longitude,
    state_key,
    county_key,
    facility_key
  )

arrow::write_parquet(jails_prisons_tbl, "data/jails-prisons.parquet")
