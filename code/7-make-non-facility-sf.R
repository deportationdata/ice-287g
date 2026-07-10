library(tidyverse)
library(sf)
library(arrow)

sf_use_s2(FALSE)

source("code/functions.R")

state_agreements_sf <- read_sf_parquet(
  "data/state_agreements_sf.parquet"
)

county_agreements_sf <- read_sf_parquet(
  "data/county_agreements_sf.parquet"
)

municipal_agreements_sf <- read_sf_parquet(
  "data/municipal_agreements_sf.parquet"
)

pa_constable_agreements_sf <- read_sf_parquet(
  "data/pa_constable_agreements_sf.parquet"
)

university_agreements_sf <- read_sf_parquet(
  "data/university_agreements_sf.parquet"
)

# bind non-facility layers -----------------------------------------------
# every layer is already written in EPSG:4326

non_facility_agreements_sf <- bind_rows(
  state_agreements_sf |> mutate(match_layer = "state"),
  county_agreements_sf |> mutate(match_layer = "county"),
  university_agreements_sf |> mutate(match_layer = "university"),
  municipal_agreements_sf |> mutate(match_layer = "municipal"),
  pa_constable_agreements_sf |> mutate(match_layer = "pa_constable")
) |>
  st_as_sf()

non_facility_unmatched <- non_facility_agreements_sf[
  is.na(st_geometry(non_facility_agreements_sf)) |
    st_is_empty(st_geometry(non_facility_agreements_sf)),
] |>
  st_drop_geometry() |>
  select(
    match_layer,
    state,
    county,
    agency,
    geom_class,
    needs_review,
    any_of(c(
      "state_match",
      "county_match",
      "city_guess",
      "city_match",
      "match_ambiguous",
      "manual_match_layer",
      "src",
      "manual_reason",
      "manual_note",
      "university_name",
      "university_guess",
      "university_guess_final",
      "municipality_guess",
      "municipality_type_hint",
      "pa_constable_jurisdiction",
      "resolved_county",
      "county_match_status",
      "review_reason",
      "vtd_name",
      "ward_name",
      "ward_vtd_qa_status"
    ))
  )

readr::write_csv(
  non_facility_unmatched,
  "data/non_facility_matches_needing_review.csv"
)

write_sf_parquet(
  non_facility_agreements_sf,
  "data/non_facility_agreements_sf.parquet"
)
