# HIFLD local law enforcement stations -> data/intermediate/agency-roster-hifld.parquet
library(tidyverse)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")

hifld <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-local-law-enforcement-facilities.parquet"
)

hifld_law_enforcement <- hifld |>
  left_join(state_xwalk, by = c("state" = "state_abbr")) |>
  mutate(state = coalesce(state_full, state)) |>
  transmute(
    state_key = norm_state(state),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(name),
    fullname_key = norm_ori_fullname(name),
    name,
    address,
    city,
    state,
    zip,
    county,
    county_fips,
    type,
    latitude,
    longitude
  )

arrow::write_parquet(
  hifld_law_enforcement,
  "data/intermediate/agency-roster-hifld.parquet"
)
