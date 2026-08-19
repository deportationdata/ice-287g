library(tidyverse)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/state-xwalk.parquet")

# the GitHub raw URL is flaky enough that the retry/backoff is load-bearing
hifld <- read_parquet_retry(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-local-law-enforcement-facilities.parquet"
)

hifld_law_enforcement <- hifld |>
  # downstream joins key on the full state name; an abbreviation absent from
  # the xwalk falls back to itself rather than becoming NA
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

# plain (non-sf) parquet: the facility matcher builds points from lat/lon
arrow::write_parquet(
  hifld_law_enforcement,
  "data/hifld-law-enforcement.parquet"
)
