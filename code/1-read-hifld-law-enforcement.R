library(tidyverse)
library(arrow)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/state_xwalk.parquet")

hifld <- read_parquet_retry(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-local-law-enforcement-facilities.parquet"
) |>
  ensure_columns(
    list(
      population = NA_real_,
      county = NA_character_,
      county_fips = NA_character_,
      naics_code = NA_character_,
      naics_desc = NA_character_,
      source_url = NA_character_,
      source_date = NA_character_,
      website = NA_character_,
      ci_id = NA_character_,
      csllea08id = NA_character_,
      subtype1 = NA_character_,
      subtype2 = NA_character_,
      tribal = NA_character_
    )
  )

hifld_law_enforcement <- hifld |>
  left_join(state_xwalk, by = c("state" = "state_abbr")) |>
  mutate(
    state_full = coalesce(state_full, state),
    state_key = norm_state(state_full),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(name),
    fullname_key = norm_ori_fullname(name)
  ) |>
  select(-state_fips)

arrow::write_parquet(
  hifld_law_enforcement,
  "data/hifld_law_enforcement.parquet"
)
