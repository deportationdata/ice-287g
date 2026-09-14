# The Census counties as a plain table, for scripts that need county names and codes
# without loading geometry -> data/intermediate/reference-counties.parquet
library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

source("code/functions.R")

counties_reference(2024) |>
  st_drop_geometry() |>
  transmute(state = state_name, state_key, county, county_key, county_fips = geoid) |>
  arrow::write_parquet("data/intermediate/reference-counties.parquet")
