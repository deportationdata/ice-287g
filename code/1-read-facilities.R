# ICE detention facility roster (remote) -> data/facilities.parquet
library(tidyverse)
library(sf)

source("code/functions.R")

# read_parquet_retry cannot be reused: geometry metadata needs read_sf_parquet
read_sf_parquet_retry <- function(path, times = 4, timeout_seconds = 300) {
  old_timeout <- getOption("timeout")
  options(timeout = max(old_timeout, timeout_seconds))
  on.exit(options(timeout = old_timeout), add = TRUE)

  last_error <- NULL

  for (attempt in seq_len(times)) {
    result <- tryCatch(
      read_sf_parquet(path),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(result)) {
      return(result)
    }

    if (attempt < times) {
      pause <- min(10 * attempt, 60)
      message(
        "Failed to read parquet on attempt ",
        attempt,
        " of ",
        times,
        "; retrying in ",
        pause,
        " seconds: ",
        conditionMessage(last_error)
      )
      Sys.sleep(pause)
    }
  }

  stop(
    "Failed to read parquet after ",
    times,
    " attempts: ",
    conditionMessage(last_error),
    call. = FALSE
  )
}

facilities <- read_sf_parquet_retry(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/facilities-latest-sf.parquet"
)

state_xwalk <- arrow::read_parquet("data/state-xwalk.parquet")

facilities <- facilities |>
  # a source table, not the keep-all agreements spine: coordinate-less rows drop
  st_drop_geometry() |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  left_join(state_xwalk, by = c("state" = "state_abbr")) |>
  transmute(
    source = "facilities",
    # dedup tie-break order: manual 0, facilities 1, jails/prisons 2-3, HIFLD 4
    source_rank = 1L,
    detention_facility_code = as.character(detention_facility_code),
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

arrow::write_parquet(facilities, "data/facilities.parquet")
