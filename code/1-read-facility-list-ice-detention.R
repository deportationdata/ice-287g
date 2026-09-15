# ICE detention facility roster (remote) -> data/intermediate/facility-list-ice-detention.parquet
library(tidyverse)

source("code/functions.R")

# the roster is GeoParquet, but only its columns are used, so it is read as a
# plain table with arrow: the runner's GDAL has no Parquet driver, and sf cannot
# open the file there
facilities_file <- tempfile(fileext = ".parquet")
download.file(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/facilities-latest-sf.parquet",
  facilities_file, mode = "wb", quiet = TRUE
)
facilities <- arrow::read_parquet(facilities_file) |>
  select(-any_of("geometry"))

state_xwalk <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")

facilities <- facilities |>
  # a source table, not the keep-all agreements spine: coordinate-less rows drop
  filter(!is.na(latitude), !is.na(longitude)) |>
  left_join(state_xwalk, by = c("state" = "state_abbr")) |>
  transmute(
    source = "facilities",
    # dedup tie-break order: manual 0, facilities 1, jails/prisons 2-3, HIFLD 4
    source_rank = 1L,
    detention_facility_code = as.character(detention_facility_code),
    source_id = as.character(detention_facility_code),
    facility_name = str_squish(name),
    facility_address = address,
    facility_city = str_squish(city),
    facility_county = str_to_title(str_squish(county)),
    facility_state = coalesce(state_full, str_to_title(str_squish(state))),
    facility_zip = zip,
    state_fips = as.character(state_fips_code),
    county_fips = as.character(county_fips_code),
    latitude,
    longitude,
    state_key = norm_state(facility_state),
    county_key = norm_ori_county(facility_county),
    # norm_key, not norm_ori_agency: facility names get no LEAIC expansion
    facility_key = norm_key(facility_name)
  )

arrow::write_parquet(facilities, "data/intermediate/facility-list-ice-detention.parquet")
