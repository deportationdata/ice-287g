source("code/3-make-county-sf.R")

YEAR <- 2024

# place and county-subdivision boundaries --------------------------------

places_sf <- tigris::places(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    state = str_to_title(STATE_NAME),
    place_guess = str_to_title(NAME),
    statefp = STATEFP,
    placefp = PLACEFP,
    geometry
  )

# pull all states for county subdivisions
states_sf_raw <- tigris::states(cb = TRUE, year = YEAR, class = "sf")

cousubs_sf <- map_dfr(unique(states_sf_raw$STATEFP), function(fp) {
  tigris::county_subdivisions(state = fp, cb = TRUE, year = YEAR, class = "sf")
}) |>
  transmute(
    state = str_to_title(STATE_NAME),
    place_guess = str_to_title(NAME),
    statefp = STATEFP,
    placefp = COUSUBFP,
    geometry
  )

# normalized places lookup (places preferred over cousubs)
places_lookup <- bind_rows(
  places_sf |> mutate(src = "place"),
  cousubs_sf |> mutate(src = "cousub")
) |>
  mutate(
    state_key = norm_state(state),
    place_key = norm_place(place_guess),
    src_rank = if_else(src == "place", 1L, 2L)
  ) |>
  group_by(state_key, place_key) |>
  slice_min(src_rank, n = 1, with_ties = FALSE) |>
  ungroup()

# municipal agreements ---------------------------------------------------

municipal_agreements_sf <- agencies_all |>
  filter(geom_class == "municipal_polygon") |>
  mutate(
    city_guess = extract_city_guess(`LAW ENFORCEMENT AGENCY`),
    state_key = norm_state(state),
    place_key = norm_place(city_guess),
    county_key = norm_place(county)
  ) |>
  left_join(
    places_lookup |> select(state_key, place_key, geometry, src) |> st_transform(4326), # note added st_transform to fix below merging problem
    by = c("state_key", "place_key")
  ) |>
  # TODO: why do we need centroids?
  left_join(
    county_centroids |>
      select(state_key, county_key, geometry) |>
      rename(county_geometry = geometry) |> 
      st_transform(4326), # this fixes the issue below 
    by = c("state_key", "county_key")
  ) |>
  mutate(
    missing_place = is.na(src) |
      st_is_empty(geometry) |
      is.na(st_dimension(geometry)),
    src = if_else(missing_place, "county_centroid_fallback", src)
  ) |>
  # TODO: the below we should be able to avoid using if_else but need coord system to align (see above)
  mutate(geometry = if_else(missing_place, county_geometry, geometry)) |> 
  # (\(df) {
  #  df$geometry[df$missing_place] <- df$county_geometry[df$missing_place]
  #  df
  # })() |>
  select(-county_geometry, -missing_place) |>
  mutate(
    needs_review = needs_review | is.na(geometry) | st_is_empty(geometry)
  ) |>
  st_as_sf()
# TODO: 301 need review -- what's next?