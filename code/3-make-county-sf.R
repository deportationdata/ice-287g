source("code/1-process-agencies-data.R")

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

# county centroids (fallback for unmatched municipalities) ---------------

county_centroids <- counties_sf |>
  st_transform(5070) |>
  mutate(centroid = st_centroid(geometry)) |>
  st_drop_geometry() |>
  st_as_sf(sf_column_name = "centroid") |>
  st_transform(4326) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_place(county)
  ) |>
  rename(geometry = centroid)

# county agreements ------------------------------------------------------

county_agreements_sf <- agencies_all |>
  filter(geom_class == "county_polygon") |>
  left_join(counties_sf, by = c("state", "county")) |>
  st_as_sf()
