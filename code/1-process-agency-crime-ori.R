library(tidyverse)
library(arrow)

source("code/functions.R")

agencies_all <- arrow::read_parquet("data/agencies_all.parquet") |>
  normalize_agencies_all()

crime_lookup <- arrow::read_parquet("inputs/crime-data-all-states.parquet") |>
  transmute(
    state_key = norm_state(state_name),
    county_key = norm_place(county),
    agency_key = norm_key(agency_name),
    crime_ori = str_squish(ori),
    crime_agency_name = str_squish(agency_name),
    crime_agency_type = agency_type_name,
    crime_latitude = latitude,
    crime_longitude = longitude,
    crime_nibrs_start = nibrs_start_date
  ) |>
  filter(!is.na(crime_ori), crime_ori != "") |>
  group_by(state_key, county_key, agency_key) |>
  slice_head(n = 1) |>
  ungroup()

agencies_all <- agencies_all |>
  select(
    -any_of(c(
      "crime_ori",
      "crime_agency_name",
      "crime_agency_type",
      "crime_latitude",
      "crime_longitude",
      "crime_nibrs_start",
      "crime_match_type",
      "ori_source"
    ))
  ) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_place(county),
    agency_key = norm_key(agency)
  ) |>
  left_join(
    crime_lookup,
    by = c("state_key", "county_key", "agency_key")
  ) |>
  mutate(
    crime_match_type = if_else(
      is.na(crime_ori),
      NA_character_,
      "exact_state_county_agency_name"
    ),
    ORI9 = coalesce(ORI9, crime_ori),
    ori_source = case_when(
      !is.na(leaic_match_type) ~ "leaic",
      !is.na(crime_match_type) ~ "crime_lookup",
      TRUE ~ NA_character_
    )
  ) |>
  select(-state_key, -county_key, -agency_key)

arrow::write_parquet(agencies_all, "data/agencies_all.parquet")
