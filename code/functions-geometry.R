# Reference geometry shared by the layer scripts.

# Census LSAD code -> government type; unlisted codes are statistical divisions (CCDs)
lsad_type <- c(
  "21" = "borough",
  "25" = "city",
  "35" = "township",
  "43" = "town",
  "44" = "township",
  "47" = "village",
  "49" = "township"
)
# New England towns (county subdivisions) outrank same-named CDPs
new_england_fips <- c("09", "23", "25", "33", "44", "50")

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

# Census county subdivisions with a working government: the gazetteer's FUNCSTAT drops
# inactive (I), nonfunctioning (N) and statistical (S) units
county_subdivisions_reference <- function(year = 2024) {
  status <- readr::read_tsv(
    sprintf("inputs/%d-census-gazetteer-county-subdivisions/%d_Gaz_cousubs_national.txt", year, year),
    col_types = readr::cols(.default = "c"),
    trim_ws = TRUE
  ) |>
    transmute(GEOID, FUNCSTAT)
  tigris::states(cb = TRUE, year = year, class = "sf")$STATEFP |>
    unique() |>
    purrr::map(\(fp) tigris::county_subdivisions(state = fp, cb = TRUE, year = year, class = "sf")) |>
    bind_rows() |>
    inner_join(status, by = "GEOID") |>
    filter(!FUNCSTAT %in% c("I", "N", "S"))
}

# a boundary in several counties lists their codes with semicolons, largest share first
among <- function(code, listed) {
  str_detect(paste0(";", listed, ";"), fixed(paste0(";", code, ";")))
}
# one row per group under a total order: the caller's ranking first, then the
# tiebreak columns, so no pick ever depends on row order
slice_best <- function(x, ..., by, tiebreak) {
  x |>
    arrange(..., across(all_of(tiebreak))) |>
    slice_head(n = 1, by = all_of(by))
}
