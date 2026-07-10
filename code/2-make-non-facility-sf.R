library(tidyverse)
library(sf)
library(tigris)
library(arrow)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

agencies_all <- arrow::read_parquet("data/agencies_all.parquet") |>
  normalize_agencies_all()
manual_non_facility_polygons <- arrow::read_parquet(
  "data/manual_non_facility_polygons.parquet"
)

YEAR <- 2024

# boundary lookups --------------------------------------------------------
#
# All lookups are EPSG:4326 MULTIPOLYGON. Casting matters: dplyr joins fill
# unmatched rows from a homogeneous sfc with a typed empty geometry
# (MULTIPOLYGON EMPTY), which downstream scripts detect via st_is_empty().
#
# Shared lookup columns: match_key (+ state_key where the join is within a
# state), cand_county_key (candidate's county, for disambiguating same-named
# municipalities), state_fips/county_fips/place_fips/geoid, boundary_src,
# src_rank, geometry.

states_lookup <- tigris::states(cb = TRUE, year = YEAR, class = "sf") |>
  st_transform(4326) |>
  st_cast("MULTIPOLYGON") |>
  transmute(
    match_key = norm_state(NAME),
    cand_county_key = NA_character_,
    state_fips = STATEFP,
    county_fips = NA_character_,
    place_fips = NA_character_,
    geoid = STATEFP,
    state_abbr = STUSPS,
    boundary_src = "tigris_state",
    src_rank = 1L,
    geometry
  )

counties_raw <- tigris::counties(cb = TRUE, year = YEAR, class = "sf")

counties_lookup <- counties_raw |>
  st_transform(4326) |>
  st_cast("MULTIPOLYGON") |>
  transmute(
    state_key = norm_state(STATE_NAME),
    match_key = norm_county(NAMELSAD),
    cand_county_key = NA_character_,
    state_fips = STATEFP,
    county_fips = paste0(STATEFP, COUNTYFP),
    place_fips = NA_character_,
    geoid = paste0(STATEFP, COUNTYFP),
    boundary_src = "tigris_county",
    src_rank = 1L,
    geometry
  )

# county names by FIPS, to attach a county to each county subdivision
county_names_by_fp <- counties_raw |>
  st_drop_geometry() |>
  transmute(
    STATEFP,
    COUNTYFP,
    cand_county_key = norm_place(NAMELSAD)
  )

# the places file carries no county attribute, so assign counties spatially;
# a place intersecting several counties gets one candidate row per county —
# without this, same-named places in different counties of a state could not
# be disambiguated by the agreement's county
county_key_polygons <- counties_raw |>
  st_transform(4326) |>
  transmute(cand_county_key = norm_place(NAMELSAD))

places_lookup <- tigris::places(cb = TRUE, year = YEAR, class = "sf") |>
  st_transform(4326) |>
  st_cast("MULTIPOLYGON") |>
  transmute(
    state_key = norm_state(STATE_NAME),
    match_key = norm_place(NAME),
    state_fips = STATEFP,
    county_fips = NA_character_,
    place_fips = PLACEFP,
    geoid = GEOID,
    boundary_src = "place",
    src_rank = 1L,
    geometry
  ) |>
  st_join(county_key_polygons, join = st_intersects, left = TRUE)

cousubs_lookup <- unique(counties_raw$STATEFP) |>
  map(
    \(fp) {
      tigris::county_subdivisions(
        state = fp,
        cb = TRUE,
        year = YEAR,
        class = "sf"
      )
    }
  ) |>
  bind_rows() |>
  st_transform(4326) |>
  st_cast("MULTIPOLYGON") |>
  left_join(county_names_by_fp, by = c("STATEFP", "COUNTYFP")) |>
  transmute(
    state_key = norm_state(STATE_NAME),
    match_key = norm_place(NAME),
    cand_county_key,
    state_fips = STATEFP,
    county_fips = paste0(STATEFP, COUNTYFP),
    place_fips = COUSUBFP,
    geoid = GEOID,
    boundary_src = "cousub",
    src_rank = 2L,
    geometry
  )

# places preferred over county subdivisions when both match a name
municipal_lookup <- bind_rows(places_lookup, cousubs_lookup)

# manual overrides ---------------------------------------------------------

manual_overrides <- manual_non_facility_polygons |>
  select(
    agency,
    state,
    county,
    manual_match_layer,
    manual_match_name,
    manual_reason,
    manual_note
  )

# generic layer matcher ----------------------------------------------------
#
# For each agreement routed to `layer` (by manual override or geom_class),
# resolve a boundary polygon: coalesce the manual match name over the
# automatic one, normalize it to the lookup's match key, join candidates,
# prefer lower src_rank then candidates in the agreement's own county, and
# flag agreements whose top candidates remain tied.

match_polygon_layer <- function(
  layer,
  polygon_class,
  lookup,
  auto_match_fn,
  match_key_fn,
  join_on_state = TRUE
) {
  base <- agencies_all |>
    left_join(manual_overrides, by = c("agency", "state", "county")) |>
    filter(
      manual_match_layer == layer |
        (geom_class == polygon_class & is.na(manual_match_layer))
    )

  if (layer == "municipal") {
    base <- base |>
      filter(!is_pa_constable_agency(state, agency))
  }

  join_cols <- if (join_on_state) c("state_key", "match_key") else "match_key"

  resolved <- base |>
    mutate(
      .row_id = row_number(),
      manual_match = if_else(
        manual_match_layer == layer,
        manual_match_name,
        NA_character_
      ),
      auto_match = auto_match_fn(base),
      match_value = coalesce(manual_match, auto_match),
      match_key = match_key_fn(match_value),
      state_key = norm_state(state),
      county_key_agency = norm_place(county)
    ) |>
    left_join(lookup, by = join_cols, relationship = "many-to-many") |>
    group_by(.row_id) |>
    mutate(
      # distinct geoids: the places lookup carries one row per intersecting
      # county, so the same polygon can appear as several candidate rows
      candidate_count = n_distinct(geoid, na.rm = TRUE),
      county_pref = if_else(
        !is.na(cand_county_key) &
          !is.na(county_key_agency) &
          cand_county_key == county_key_agency,
        0L,
        1L
      )
    ) |>
    arrange(src_rank, county_pref, geoid, .by_group = TRUE) |>
    mutate(
      top_ties = n_distinct(
        geoid[src_rank == first(src_rank) & county_pref == first(county_pref)],
        na.rm = TRUE
      )
    ) |>
    slice_head(n = 1) |>
    ungroup()

  resolved |>
    mutate(
      match_ambiguous = candidate_count > 1 & top_ties > 1,
      src = if_else(
        is.na(manual_match),
        boundary_src,
        if (layer == "municipal") {
          paste("manual_municipal_override", boundary_src, sep = ":")
        } else {
          paste0("manual_", layer, "_override")
        }
      ),
      needs_review = needs_review | st_is_empty(geometry) | match_ambiguous
    )
}

select_layer_columns <- function(x, extra_cols) {
  x |>
    select(
      state,
      county,
      agency,
      support_type,
      agency_level,
      geom_class,
      ORI9,
      FSTATE,
      FCOUNTY,
      FPLACE,
      leaic_name,
      leaic_match_type,
      crime_ori,
      crime_agency_name,
      crime_match_type,
      ori_source,
      needs_review,
      has_addendum,
      moa_pending,
      all_of(extra_cols),
      match_ambiguous,
      state_fips,
      county_fips,
      place_fips,
      geoid,
      src,
      manual_match_layer,
      manual_reason,
      manual_note,
      geometry
    ) |>
    st_as_sf()
}

# state agreements ---------------------------------------------------------

state_agreements_sf <- match_polygon_layer(
  layer = "state",
  polygon_class = "state_polygon",
  lookup = states_lookup,
  auto_match_fn = \(x) x$state,
  match_key_fn = norm_state,
  join_on_state = FALSE
) |>
  rename(state_match = match_value) |>
  select_layer_columns(c("state_match", "state_abbr"))

write_sf_parquet(
  state_agreements_sf,
  "data/state_agreements_sf.parquet"
)

# county agreements --------------------------------------------------------

county_agreements_sf <- match_polygon_layer(
  layer = "county",
  polygon_class = "county_polygon",
  lookup = counties_lookup,
  auto_match_fn = \(x) x$county,
  match_key_fn = norm_county
) |>
  rename(county_match = match_value) |>
  select_layer_columns("county_match")

write_sf_parquet(
  county_agreements_sf,
  "data/county_agreements_sf.parquet"
)

# municipal agreements -----------------------------------------------------

municipal_agreements_sf <- match_polygon_layer(
  layer = "municipal",
  polygon_class = "municipal_polygon",
  lookup = municipal_lookup,
  auto_match_fn = \(x) extract_city_guess(x$agency),
  match_key_fn = norm_place
) |>
  rename(
    city_guess = auto_match,
    city_match = match_value
  ) |>
  select_layer_columns(c("city_guess", "city_match"))

write_sf_parquet(
  municipal_agreements_sf,
  "data/municipal_agreements_sf.parquet"
)
