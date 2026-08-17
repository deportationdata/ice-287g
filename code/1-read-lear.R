library(tidyverse)
library(arrow)
library(haven)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/state_xwalk.parquet")

# ICPSR negative codes (-1, -3, ...) mark missing values
lear_value <- function(x) {
  x <- str_squish(as.character(x))
  if_else(x %in% c("", "-1", "-2", "-3", "-8", "-9"), NA_character_, x)
}

lear <- haven::read_dta("inputs/ICPSR_36697/DS0001/36697-0001-Data.dta") |>
  transmute(
    lear_id = as.character(LEAR_ID),
    lear_name = str_squish(NAME),
    lear_address = lear_value(STREET_ADDRESS),
    lear_city = str_to_title(lear_value(CITY)),
    lear_state_abbr = str_to_upper(str_squish(STATE)),
    lear_zip = lear_value(ZIP),
    lear_county = str_to_title(lear_value(COUNTY)),
    lear_county_fips = if_else(
      !is.na(FIPS),
      str_pad(as.character(FIPS), 5, pad = "0"),
      NA_character_
    ),
    lear_agency_type = as.character(as_factor(SAMPTYPE)),
    lear_source = lear_value(SOURCE),
    csllea08_id = lear_value(CSLLEA08_ID),
    ORI9 = lear_value(ORI9),
    ORI7 = lear_value(ORI7),
    lear_fulltime_sworn = LEAR_FULLTIME_SWORN,
    lear_parttime_sworn = LEAR_PARTTIME_SWORN,
    lear_no_policing = as.logical(NO_POLICING)
  ) |>
  left_join(state_xwalk, by = c("lear_state_abbr" = "state_abbr")) |>
  mutate(
    lear_state = coalesce(state_full, lear_state_abbr),
    state_key = norm_state(lear_state),
    county_key = norm_ori_county(lear_county),
    agency_key = norm_ori_agency(lear_name),
    lear_fullname_key = norm_ori_fullname(lear_name)
  ) |>
  select(-state_full, -state_fips)

arrow::write_parquet(lear, "data/lear.parquet")
