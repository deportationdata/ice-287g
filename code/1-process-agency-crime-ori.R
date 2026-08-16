library(tidyverse)
library(arrow)

source("code/functions.R")

agencies_all <- arrow::read_parquet("data/agencies_all.parquet") |>
  normalize_agencies_all()

crime_cols <- c(
  "crime_ori",
  "crime_agency_name",
  "crime_agency_type",
  "crime_latitude",
  "crime_longitude",
  "crime_nibrs_start"
)

crime_lookup <- arrow::read_parquet("inputs/crime-data-all-states.parquet") |>
  transmute(
    state_key = norm_state(state_name),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(agency_name),
    crime_fullname_key = norm_ori_fullname(agency_name),
    crime_ori = str_squish(ori),
    crime_agency_name = str_squish(agency_name),
    crime_agency_type = agency_type_name,
    crime_latitude = latitude,
    crime_longitude = longitude,
    crime_nibrs_start = nibrs_start_date
  ) |>
  filter(!is.na(crime_ori), crime_ori != "") |>
  group_by(state_key, county_key, agency_key, crime_fullname_key) |>
  slice_head(n = 1) |>
  ungroup()

manual_agency_ori <- read_csv(
  "inputs/manual-agency-ori.csv",
  col_types = cols(.default = "c")
) |>
  filter(!is.na(ORI9), ORI9 != "") |>
  distinct(state, county, agency, .keep_all = TRUE)

agencies_all <- agencies_all |>
  select(-any_of(c(crime_cols, "crime_match_type", "ori_source"))) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(agency),
    fullname_key = norm_ori_fullname(agency)
  ) |>
  left_join(
    crime_lookup,
    by = c("state_key", "county_key", "agency_key"),
    relationship = "many-to-many"
  ) |>
  group_by(agreement_id) |>
  arrange(
    desc(crime_fullname_key == fullname_key),
    crime_ori,
    .by_group = TRUE
  ) |>
  slice_head(n = 1) |>
  ungroup() |>
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
    ),
    # LEAIC and the FBI lookup naming the same agency with different ORIs
    # means one of the two matches is wrong
    needs_review = needs_review |
      (!is.na(leaic_match_type) & !is.na(crime_ori) & ORI9 != crime_ori)
  ) |>
  left_join(
    manual_agency_ori |>
      filter(!is.na(county), county != "") |>
      select(state, county, agency, manual_ori_specific = ORI9),
    by = c("state", "county", "agency")
  ) |>
  left_join(
    manual_agency_ori |>
      filter(is.na(county) | county == "") |>
      select(state, agency, manual_ori_general = ORI9),
    by = c("state", "agency")
  ) |>
  mutate(
    manual_ori = coalesce(manual_ori_specific, manual_ori_general),
    ori_source = if_else(
      is.na(ORI9) & !is.na(manual_ori),
      "manual",
      ori_source
    ),
    ORI9 = coalesce(ORI9, manual_ori)
  ) |>
  select(
    -state_key,
    -county_key,
    -agency_key,
    -fullname_key,
    -crime_fullname_key,
    -manual_ori_specific,
    -manual_ori_general,
    -manual_ori
  )

arrow::write_parquet(agencies_all, "data/agencies_all.parquet")
