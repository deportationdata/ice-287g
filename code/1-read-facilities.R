library(tidyverse)
library(sf)

source("code/functions.R")

# the GitHub raw URL is flaky enough that retrying is load-bearing;
# read_parquet_retry cannot be reused because the geometry metadata has to come
# through read_sf_parquet, so mirror its backoff around that reader
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
  # upstream geometry is redundant with lat/lon, so the output is a plain
  # parquet. Coordinate-less rows are dropped outright: this is a candidate
  # source table, not the keep-all agreements spine
  st_drop_geometry() |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  # agreements key on the full state name ("texas", not "tx")
  left_join(state_xwalk, by = c("state" = "state_abbr")) |>
  transmute(
    source = "facilities",
    # source_rank breaks every downstream dedup tie: manual points (0L) beat
    # this table, which beats jails/prisons (2L/3L) and HIFLD stations (4L)
    source_rank = 1L,
    detention_facility_code = as.character(detention_facility_code),
    facility_name = str_squish(name),
    facility_address = address,
    facility_city = str_squish(city),
    facility_county = str_to_title(str_squish(county)),
    # an abbreviation missing from the xwalk falls back to its title-cased form
    # rather than NA, and state_key derives from that fallback
    facility_state = coalesce(state_full, str_to_title(str_squish(state))),
    facility_zip = zip,
    state_fips = as.character(state_fips_code),
    county_fips = as.character(county_fips_code),
    latitude,
    longitude,
    state_key = norm_state(facility_state),
    county_key = norm_ori_county(facility_county),
    # plain norm_key, not norm_ori_agency: facility names get no LEAIC
    # abbreviation expansion
    facility_key = norm_key(facility_name)
  )

arrow::write_parquet(facilities, "data/facilities.parquet")
