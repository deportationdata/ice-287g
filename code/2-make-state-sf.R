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

# state boundaries -------------------------------------------------------

states_sf <- tigris::states(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    state = str_to_title(NAME),
    statefp = STATEFP,
    geometry
  )

# territories
counties_sf_raw <- tigris::counties(cb = TRUE, year = YEAR, class = "sf")

territories <- counties_sf_raw |>
  filter(STATEFP %in% c("60", "66", "69", "78")) |>
  group_by(STATEFP) |>
  summarise(geometry = st_union(geometry)) |>
  left_join(
    tibble(
      STATEFP = c("60", "66", "69", "78"),
      NAME = c(
        "American Samoa",
        "Guam",
        "Commonwealth of the Northern Mariana Islands",
        "United States Virgin Islands"
      ),
      STUSPS = c("AS", "GU", "MP", "VI")
    ),
    by = "STATEFP"
  )

all_states_and_territories <- bind_rows(
  states_sf |>
    filter(as.integer(statefp) <= 56 | statefp == "72") |>
    transmute(
      NAME = state,
      STATEFP = statefp,
      STUSPS = NA_character_,
      geometry
    ),
  territories |>
    rename(STATEFP = STATEFP) |>
    select(NAME, STATEFP, STUSPS, geometry)
)

state_lookup <- all_states_and_territories |>
  transmute(
    state = NAME,
    statefp = STATEFP,
    state_abbr = STUSPS,
    geometry
  )

# state agreements -------------------------------------------------------

state_agreements_sf <- agencies_all |>
  filter(geom_class == "state_polygon") |>
  left_join(state_lookup, by = "state") |>
  st_as_sf()

# save state geometries --------------------------------------------------

sfarrow::st_write_parquet(
  state_agreements_sf,
  "data/processed/state_agreements_sf.parquet"
)
sfarrow::st_write_parquet(
  all_states_and_territories,
  "data/processed/all_states_and_territories.parquet"
)
