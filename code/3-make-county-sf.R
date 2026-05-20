library(tidyverse)
library(sf)
library(tigris)
library(arrow)
library(sfarrow)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

agencies_all <- arrow::read_parquet("data/processed/agencies_all.parquet")

YEAR <- 2024

# county boundaries ------------------------------------------------------

counties_sf <- tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    state = str_to_title(STATE_NAME),
    county = str_to_title(NAME),
    statefp = STATEFP,
    countyfp = COUNTYFP,
    geometry
  )

# county agreements ------------------------------------------------------

county_agreements_sf <- agencies_all |>
  filter(geom_class == "county_polygon") |>
  left_join(counties_sf, by = c("state", "county")) |>
  st_as_sf()

# save county geometries -------------------------------------------------

sfarrow::st_write_parquet(
  county_agreements_sf,
  "data/processed/county_agreements_sf.parquet"
)
