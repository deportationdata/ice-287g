# Binds the facility and non-facility layers into one feature layer, names the census
# unit each placed feature is, and finds the counties and place around it
# -> data/intermediate/match-features.parquet
library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

source("code/functions.R")

YEAR <- 2024

non_facility_sf <- st_read(
  "data/intermediate/match-non-facility.parquet",
  quiet = TRUE
)
facility_sf <- st_read("data/intermediate/match-facility.parquet", quiet = TRUE)

agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")

stopifnot(
  "non-facility layer file must arrive in EPSG:4326" = st_crs(
    non_facility_sf
  ) ==
    st_crs(4326),
  "facility layer file must arrive in EPSG:4326" = st_crs(facility_sf) ==
    st_crs(4326)
)

non_facility_ids <- non_facility_sf |>
  st_drop_geometry() |>
  filter(coalesce(
    !match_type %in%
      c("regional_member_municipality", "judicial_district_member_county", "airport_boundary"),
    TRUE
  )) |>
  pull(agreement_id)
stopifnot(
  "every agreement is in exactly one layer" = setequal(
    c(non_facility_sf$agreement_id, facility_sf$agreement_id),
    agreements$agreement_id
  ) &&
    !any(facility_sf$agreement_id %in% non_facility_sf$agreement_id),
  "only regional departments, judicial districts and port authorities fan out in the non-facility layers" = !anyDuplicated(
    non_facility_ids
  ),
  "the facility layer holds exactly the facility-point agreements" = setequal(
    facility_sf$agreement_id,
    agreements$agreement_id[agreements$geometry_type %in% "point"]
  )
)

# unmatched agreements ride along with empty geometries; nothing is dropped
features_sf <-
  bind_rows(
    non_facility_sf,
    facility_sf |>
      mutate(match_layer = "facility")
  ) |>
  st_make_valid() |>
  mutate(
    # the census unit a placed feature is; a jail, campus or airport is none
    geoid_type = case_when(
      is.na(geoid) ~ NA_character_,
      match_layer == "state" ~ "state",
      match_layer == "county" ~ "county",
      match_type == "lrc_ward" ~ "pa_ward",
      match_type == "lrc_voting_district" ~ "pa_voting_district",
      match_layer == "municipal" & nchar(geoid) == 7 ~ "place",
      match_layer == "municipal" & nchar(geoid) == 10 ~ "county_subdivision"
    )
  )
stopifnot(
  "every geoid names its code system" = !any(
    !is.na(features_sf$geoid) & is.na(features_sf$geoid_type)
  )
)

# ---- the census counties and place around each feature ----
counties_ref <- counties_reference(YEAR) |>
  transmute(county_fips = geoid, county, geometry)
# a census place, else the county subdivision around it; New England towns outrank
# same-named places
places_ref <- bind_rows(
  places(cb = TRUE, year = YEAR, class = "sf") |>
    transmute(place_geoid = GEOID, place = NAME, lsad = LSAD, place_type = NAMELSAD, geometry),
  county_subdivisions_reference(YEAR) |>
    transmute(place_geoid = GEOID, place = NAME, lsad = LSAD, place_type = NAMELSAD, geometry)
) |>
  mutate(
    # NAMELSAD is the name plus its type; a consolidated city-county has it in the name and a blank LSAD
    place_type = str_squish(str_remove(place_type, fixed(place))),
    place_type = case_when(
      nchar(place_geoid) == 7 & lsad == "00" ~ "consolidated city-county",
      place_type == "CDP" ~ place_type,
      TRUE ~ na_if(str_to_lower(place_type), "")
    ),
    .rank = if_else(
      (str_sub(place_geoid, 1, 2) %in% new_england_fips) == (nchar(place_geoid) == 10),
      1L,
      2L
    )
  )

# the share of each feature in every reference unit around it: a point's whole, a
# polygon's overlap area
surrounding <- function(x, ref) {
  x <- x |> select(.row) |> st_transform(3857)
  ref <- ref |> st_transform(3857)
  is_point <- st_dimension(x) == 0
  bind_rows(
    if (any(is_point)) {
      x[is_point, ] |>
        st_join(ref, join = st_intersects, left = FALSE) |>
        st_drop_geometry() |>
        mutate(.share = 1)
    },
    if (any(!is_point)) {
      x[!is_point, ] |>
        st_intersection(ref) |>
        mutate(.share = as.numeric(st_area(geometry))) |>
        st_drop_geometry()
    }
  ) |>
    filter(.share > 0) |>
    mutate(.share = .share / sum(.share), .by = .row)
}

features_sf <- features_sf |> mutate(.row = row_number())
placed <- features_sf |> filter(!st_is_empty(geometry))
# every county holding over one percent of the feature, largest first
county_around <- placed |>
  filter(!geoid_type %in% c("state", "county", "county_subdivision")) |>
  surrounding(counties_ref |> select(county_fips)) |>
  filter(.share > 0.01) |>
  arrange(.row, desc(.share)) |>
  summarize(county_fips_around = paste(county_fips, collapse = ";"), .by = .row)
# the place holding most of the feature; ties go to the reference's .rank
place_around <- placed |>
  filter(!geoid_type %in% c("state", "county", "county_subdivision", "place")) |>
  surrounding(places_ref |> select(place_geoid, .rank)) |>
  arrange(.row, desc(.share), .rank) |>
  slice_head(n = 1, by = .row) |>
  select(.row, place_geoid_around = place_geoid)

county_names <- counties_ref |> st_drop_geometry() |> deframe()

features_sf <- features_sf |>
  left_join(county_around, by = ".row") |>
  left_join(place_around, by = ".row") |>
  mutate(
    # the layer's own county stays beside the census one for the QA report
    layer_county_fips = county_fips,
    county_fips = case_when(
      geoid_type == "state" ~ NA_character_,
      geoid_type == "county" ~ geoid,
      geoid_type == "county_subdivision" ~ str_sub(geoid, 1, 5),
      TRUE ~ coalesce(county_fips_around, county_fips)
    ),
    county = name_codes(county_fips, county_names),
    place_geoid = case_when(
      geoid_type %in% c("place", "county_subdivision") ~ geoid,
      geoid_type %in% c("state", "county") ~ NA_character_,
      TRUE ~ place_geoid_around
    )
  ) |>
  left_join(places_ref |> st_drop_geometry() |> select(place_geoid, place, place_type), by = "place_geoid") |>
  select(-.row, -county_fips_around, -place_geoid_around)

stopifnot(
  "every place a feature sits in has a type" = !any(
    !is.na(features_sf$place) & is.na(features_sf$place_type)
  )
)

sf::st_write(
  features_sf,
  dsn = "data/intermediate/match-features.parquet",
  driver = "Parquet",
  layer_options = "USE_PARQUET_GEO_TYPES=ONLY",
  delete_dsn = file.exists("data/intermediate/match-features.parquet"),
  quiet = TRUE
)
