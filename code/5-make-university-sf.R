source("code/1-process-agencies-data.R")

# university boundary lookup ---------------------------------------------

university_lookup <- university_boundaries |>
  st_transform(4326) |>
  mutate(
    university_name = str_squish(NAME),
    state_abbr = str_to_upper(str_squish(STATE))
  ) |>
  left_join(state_xwalk, by = "state_abbr") |>
  mutate(
    university_key = norm_key(university_name),
    state_key = norm_state(state_full)
  ) |>
  select(university_name, university_key, state_key, geometry)

# manual name overrides --------------------------------------------------

university_name_overrides <- tribble(
  ~university_guess        , ~university_guess_fixed                          ,
  "Florida A&M University" , "Florida Agricultural And Mechanical University"
)

# university agreements --------------------------------------------------

# TODO: noting Tallahassee State College Police Department didn't match

university_agreements_sf <- agencies_all |>
  filter(geom_class == "university_polygon") |>
  mutate(
    university_guess = extract_university_guess(`LAW ENFORCEMENT AGENCY`)
  ) |>
  left_join(university_name_overrides, by = "university_guess") |>
  mutate(
    university_guess_final = coalesce(university_guess_fixed, university_guess),
    university_key = norm_key(university_guess_final),
    state_key = norm_state(state)
  ) |>
  left_join(university_lookup, by = c("state_key", "university_key")) |>
  st_as_sf() |>
  mutate(
    src = if_else(
      is.na(university_name),
      "unmatched_university_boundary",
      "university_boundary"
    ),
    needs_geometry_review = is.na(university_name) | st_is_empty(geometry),
    needs_review = needs_review | needs_geometry_review
  )
