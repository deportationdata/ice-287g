library(tidyverse)
library(arrow)
library(httr2)

source("code/functions.R")

# FBI Crime Data Explorer agency roster, one request per state. The raw
# download is committed, so the API is hit only when the cache is missing:
# delete it to refresh. Needs a free key from https://api.data.gov/signup/
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
    "CDE payload must include ori, agency_name, and state_name" =
      all(c("ori", "agency_name", "state_name") %in% names(crime_data)),
    "all 50 states plus DC must return agencies (territories are best-effort)" =
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
    state = str_squish(state_name),
    county = str_squish(county),
    ori = str_squish(ori),
    name = str_squish(agency_name)
  ) |>
  filter(!is.na(ori), ori != "") |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(name),
    fullname_key = norm_ori_fullname(name)
  )

# CDE names an agency's county, sometimes several ("LEE, MACON"); convert each
# to FIPS so 5-format can test membership. Spacing differs between sources
# ("DE KALB" vs "DeKalb"), so keys drop it and the few that turn ambiguous go
county_fips_xwalk <- tigris::fips_codes |>
  transmute(
    state_key = norm_state(state_name),
    county_join_key = str_remove_all(norm_ori_county(county), "\\s"),
    county_fips = paste0(state_code, county_code)
  ) |>
  group_by(state_key, county_join_key) |>
  filter(n() == 1) |>
  ungroup()

county_fips_tbl <- crime |>
  distinct(state_key, county) |>
  mutate(county_component = county) |>
  separate_longer_delim(county_component, ",") |>
  mutate(
    county_join_key = str_remove_all(norm_ori_county(county_component), "\\s")
  ) |>
  left_join(county_fips_xwalk, by = c("state_key", "county_join_key")) |>
  group_by(state_key, county) |>
  summarize(
    # any unconvertible component voids the set: a partial list could miss the
    # county that would have agreed
    county_fips = if_else(
      any(is.na(county_fips)),
      NA_character_,
      paste(sort(unique(county_fips)), collapse = ";")
    ),
    .groups = "drop"
  )

# this join keys on the raw county string, so county must stay untouched above
crime <- crime |>
  left_join(county_fips_tbl, by = c("state_key", "county")) |>
  select(state_key, county_key, agency_key, fullname_key, ori, name, county_fips)

arrow::write_parquet(crime, "data/crime.parquet")
