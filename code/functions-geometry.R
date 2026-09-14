# Reference geometry shared by the layer scripts.

# Census counties for the pipeline's vintage, except Connecticut: from 2022 on
# tigris carries its planning regions, while ICE, the rosters and the panel name
# the eight legacy counties, so those come from the last vintage that drew them
counties_reference <- function(year = 2024, ct_year = 2021) {
  current <- tigris::counties(cb = TRUE, year = year, class = "sf")
  legacy_ct <- tigris::counties(cb = TRUE, year = ct_year, class = "sf") |> filter(STATEFP == "09")
  bind_rows(
    current |> filter(STATEFP != "09") |> mutate(geometry_vintage = as.integer(year)),
    legacy_ct |> mutate(geometry_vintage = as.integer(ct_year))
  ) |>
    transmute(
      county = str_to_title(NAMELSAD),
      state_name = STATE_NAME,
      state_key = norm_state(STATE_NAME),
      county_key = norm_county(county),
      statefp = STATEFP,
      countyfp = COUNTYFP,
      geoid = GEOID,
      geometry_vintage,
      geometry
    ) |>
    sf::st_transform(4326)
}

# one row per group under a total order: the caller's ranking first, then the
# tiebreak columns, so no pick ever depends on row order
slice_best <- function(x, ..., by, tiebreak) {
  x |>
    arrange(..., across(all_of(tiebreak))) |>
    slice_head(n = 1, by = all_of(by))
}
