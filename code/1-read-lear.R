# LEAR agency roster (ICPSR 36697) -> data/lear.parquet
library(tidyverse)
library(arrow)
library(haven)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/state-xwalk.parquet")

# ICPSR negative codes (-1, -3, ...) mark missing values
lear_value <- function(x) {
  x <- str_squish(as.character(x))
  if_else(x %in% c("", "-1", "-2", "-3", "-8", "-9"), NA_character_, x)
}

lear <- haven::read_dta("inputs/ICPSR_36697/DS0001/36697-0001-Data.dta") |>
  transmute(
    name = str_squish(NAME),
    state_abbr = str_to_upper(str_squish(STATE)),
    county = str_to_title(lear_value(COUNTY)),
    # 5-digit state+county: 5-format's FIPS checks need the widths to agree
    county_fips = if_else(
      !is.na(FIPS),
      str_pad(as.character(FIPS), 5, pad = "0"),
      NA_character_
    ),
    ori = lear_value(ORI9)
  ) |>
  left_join(
    state_xwalk |> select(state_abbr, state_full),
    by = "state_abbr"
  ) |>
  mutate(
    state_key = norm_state(coalesce(state_full, state_abbr)),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(name),
    fullname_key = norm_ori_fullname(name)
  ) |>
  filter(!is.na(ori)) |>
  select(
    state_key,
    county_key,
    agency_key,
    fullname_key,
    ori,
    name,
    county_fips
  )

arrow::write_parquet(lear, "data/lear.parquet")
