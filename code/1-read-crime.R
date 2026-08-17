library(tidyverse)
library(arrow)
library(httr2)

source("code/functions.R")

# FBI Crime Data Explorer agency roster, one request per state. The raw
# download is committed, so the API is hit only when the cache is missing —
# delete it to force a refresh. Needs a free api.data.gov key:
# https://api.data.gov/signup/
crime_cache_path <- "data/crime-data-all-states.parquet"

if (!file.exists(crime_cache_path)) {
  api_key <- Sys.getenv("CDE_API_KEY")

  if (api_key == "") {
    stop("CDE_API_KEY is not set; get a key at https://api.data.gov/signup/")
  }

  states <- c(state.abb, "DC")
  territories <- c("AS", "GU", "MP", "PR", "VI")

  fetch_state <- function(state_abbr) {
    response <- request(
      "https://api.usa.gov/crime/fbi/cde/agency/byStateAbbr"
    ) |>
      req_url_path_append(state_abbr) |>
      req_url_query(API_KEY = api_key) |>
      req_retry(max_tries = 3) |>
      req_throttle(capacity = 2, fill_time_s = 1) |>
      req_error(is_error = \(response) FALSE) |>
      req_perform()

    if (resp_status(response) != 200) {
      message(state_abbr, ": HTTP ", resp_status(response), ", skipped")
      return(tibble())
    }

    # the API nests agencies under county name
    agencies <- response |>
      resp_body_json(simplifyVector = TRUE) |>
      keep(is.data.frame) |>
      bind_rows(.id = "county")

    message(state_abbr, ": ", nrow(agencies), " agencies")

    agencies
  }

  crime_data <- c(states, territories) |>
    set_names() |>
    map(fetch_state) |>
    bind_rows(.id = "state")

  stopifnot(
    all(c("ori", "agency_name", "state_name") %in% names(crime_data)),
    all(states %in% crime_data$state)
  )

  message(
    "territories returned: ",
    paste(intersect(territories, crime_data$state), collapse = ", ")
  )

  arrow::write_parquet(crime_data, crime_cache_path)
}

crime <- arrow::read_parquet(crime_cache_path) |>
  transmute(
    crime_state = str_squish(state_name),
    crime_county = str_squish(county),
    crime_ori = str_squish(ori),
    crime_agency_name = str_squish(agency_name),
    crime_agency_type = agency_type_name,
    crime_nibrs_start = nibrs_start_date,
    crime_latitude = latitude,
    crime_longitude = longitude
  ) |>
  filter(!is.na(crime_ori), crime_ori != "") |>
  mutate(
    state_key = norm_state(crime_state),
    county_key = norm_ori_county(crime_county),
    agency_key = norm_ori_agency(crime_agency_name),
    crime_fullname_key = norm_ori_fullname(crime_agency_name)
  )

# CDE names an agency's county, sometimes several ("LEE, MACON"); convert each
# to a FIPS code so downstream checks can test membership. Spacing differs
# between sources ("DE KALB" vs "DeKalb"), so keys are compared without it,
# and the handful of keys that becomes ambiguous is dropped
county_fips_xwalk <- tigris::fips_codes |>
  transmute(
    state_key = norm_state(state_name),
    county_join_key = str_remove_all(norm_ori_county(county), "\\s"),
    county_fips = paste0(state_code, county_code)
  ) |>
  group_by(state_key, county_join_key) |>
  filter(n() == 1) |>
  ungroup()

crime_county_fips_tbl <- crime |>
  distinct(state_key, crime_county) |>
  mutate(county_component = crime_county) |>
  separate_longer_delim(county_component, ",") |>
  mutate(
    county_join_key = str_remove_all(norm_ori_county(county_component), "\\s")
  ) |>
  left_join(county_fips_xwalk, by = c("state_key", "county_join_key")) |>
  group_by(state_key, crime_county) |>
  summarize(
    # any unconvertible component voids the set, else a partial list could
    # miss the county that would have agreed
    crime_county_fips = if_else(
      any(is.na(county_fips)),
      NA_character_,
      paste(sort(unique(county_fips)), collapse = ";")
    ),
    .groups = "drop"
  )

crime <- crime |>
  left_join(crime_county_fips_tbl, by = c("state_key", "crime_county"))

arrow::write_parquet(crime, "data/crime.parquet")
