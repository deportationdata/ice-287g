library(tidyverse)
library(arrow)

source("code/functions.R")

agencies_all <- arrow::read_parquet("data/agencies_all.parquet") |>
  normalize_agencies_all()

# LEAIC crosswalk (ICPSR 35158): authoritative ORI source -----------------

load("inputs/35158-0001-Data.rda")

leaic_lookup <- da35158.0001 |>
  as_tibble() |>
  transmute(
    leaic_name = str_squish(NAME),
    ORI9 = str_squish(ORI9),
    FSTATE,
    FCOUNTY,
    FPLACE,
    state_key = norm_state(str_squish(STATENAME)),
    county_key = norm_place(str_squish(COUNTYNAME)),
    agency_key = norm_key(leaic_name)
  ) |>
  filter(!is.na(ORI9), ORI9 != "") |>
  distinct(state_key, county_key, agency_key, .keep_all = TRUE)

# FBI crime-data ORI lookup: fallback ORI source --------------------------

crime_lookup <- arrow::read_parquet("inputs/crime-data-all-states.parquet") |>
  transmute(
    crime_ori = str_squish(ori),
    crime_agency_name = str_squish(agency_name),
    state_key = norm_state(state_name),
    county_key = norm_place(county),
    agency_key = norm_key(agency_name)
  ) |>
  filter(!is.na(crime_ori), crime_ori != "") |>
  distinct(state_key, county_key, agency_key, .keep_all = TRUE)

# attach ORIs, preferring LEAIC over the crime lookup ---------------------

agencies_all <- agencies_all |>
  select(
    -any_of(c(
      "ORI9",
      "FSTATE",
      "FCOUNTY",
      "FPLACE",
      "leaic_name",
      "leaic_match_type",
      "crime_ori",
      "crime_agency_name",
      "crime_match_type",
      "ori_source"
    ))
  ) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_place(county),
    agency_key = norm_key(agency)
  ) |>
  left_join(leaic_lookup, by = c("state_key", "county_key", "agency_key")) |>
  left_join(crime_lookup, by = c("state_key", "county_key", "agency_key")) |>
  mutate(
    leaic_match_type = if_else(
      is.na(ORI9),
      NA_character_,
      "exact_state_county_agency_name"
    ),
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
