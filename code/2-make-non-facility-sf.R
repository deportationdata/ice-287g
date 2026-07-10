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
    cand_name = NAME,
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
    cand_name = NAMELSAD,
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
    cand_name = NAME,
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
    cand_name = NAME,
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
#
# Rows the exact join leaves unmatched get a state-blocked fuzzy rescue pass
# (ICE ships typos like "Adaitr County"). fuzzy_mode = "accept" patches in
# the unique best candidate (flagged needs_review, src suffixed ":fuzzy");
# "suggest" only reports candidates. Manual-override rows are never
# second-guessed. Returns list(matches, fuzzy) — fuzzy feeds the
# unmatched_polygon_suggestions.csv review sheet.

match_polygon_layer <- function(
  layer,
  polygon_class,
  lookup,
  auto_match_fn,
  match_key_fn,
  join_on_state = TRUE,
  fuzzy_mode = c("off", "accept", "suggest"),
  fuzzy_max_dist = 0.12
) {
  fuzzy_mode <- match.arg(fuzzy_mode)
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

  out <- resolved |>
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
      needs_review = needs_review | st_is_empty(geometry) | match_ambiguous,
      match_fuzzy = FALSE,
      match_fuzzy_dist = NA_real_,
      fuzzy_matched_name = NA_character_
    )

  fuzzy_report <- tibble()

  if (fuzzy_mode != "off") {
    unmatched <- out |>
      filter(st_is_empty(geometry), is.na(manual_match))
    ids <- unmatched |>
      as_tibble() |>
      select(.row_id, state, county, agency, match_value)

    cands <- fuzzy_polygon_candidates(unmatched, lookup, fuzzy_max_dist)

    accepted_ids <- integer(0)
    if (fuzzy_mode == "accept" && nrow(cands) > 0) {
      accepted <- cands |> filter(cand_rank == 1L, unique_best)
      accepted_ids <- accepted$.row_id

      if (nrow(accepted) > 0) {
        lookup_value_cols <- setdiff(
          names(sf::st_drop_geometry(lookup)),
          c("match_key", "state_key")
        )
        lookup_one <- lookup |>
          select(-match_key, -any_of("state_key")) |>
          group_by(geoid) |>
          slice_head(n = 1) |>
          ungroup()

        fuzzy_rows <- out |>
          filter(.row_id %in% accepted$.row_id) |>
          select(-all_of(c(lookup_value_cols, "geometry"))) |>
          left_join(
            accepted |> select(.row_id, geoid, match_dist),
            by = ".row_id"
          ) |>
          left_join(as_tibble(lookup_one), by = "geoid") |>
          mutate(
            match_fuzzy = TRUE,
            match_fuzzy_dist = match_dist,
            fuzzy_matched_name = cand_name,
            src = paste0(boundary_src, ":fuzzy"),
            needs_review = TRUE
          ) |>
          select(-match_dist)

        out <- bind_rows(
          out |> filter(!.row_id %in% accepted$.row_id),
          fuzzy_rows
        )
      }
    }

    cand_report <- if (nrow(cands) > 0) {
      cands |>
        left_join(ids, by = ".row_id") |>
        transmute(
          status = if_else(
            .row_id %in% accepted_ids & cand_rank == 1L,
            "auto_accepted",
            "suggestion"
          ),
          layer = .env$layer,
          state,
          county,
          agency,
          match_value,
          suggested_name = cand_name,
          suggested_geoid = geoid,
          boundary_src,
          match_dist,
          n_within_threshold,
          unique_best
        )
    } else {
      tibble()
    }

    no_cand <- ids |>
      filter(!.row_id %in% cands$.row_id) |>
      transmute(
        status = "no_candidate",
        layer = .env$layer,
        state,
        county,
        agency,
        match_value,
        suggested_name = NA_character_,
        suggested_geoid = NA_character_,
        boundary_src = NA_character_,
        match_dist = NA_real_,
        n_within_threshold = NA_integer_,
        unique_best = NA
      )

    fuzzy_report <- bind_rows(cand_report, no_cand)
  }

  list(matches = out, fuzzy = fuzzy_report)
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
      match_fuzzy,
      match_fuzzy_dist,
      fuzzy_matched_name,
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
#
# no fuzzy pass: malformed state names are snapped at ingestion against the
# closed state vocabulary (snap_state_name in 1-process-agencies.R)

state_result <- match_polygon_layer(
  layer = "state",
  polygon_class = "state_polygon",
  lookup = states_lookup,
  auto_match_fn = \(x) x$state,
  match_key_fn = norm_state,
  join_on_state = FALSE
)

state_agreements_sf <- state_result$matches |>
  rename(state_match = match_value) |>
  select_layer_columns(c("state_match", "state_abbr"))

write_sf_parquet(
  state_agreements_sf,
  "data/state_agreements_sf.parquet"
)

# county agreements --------------------------------------------------------
#
# fuzzy auto-accept: county names within a state are a closed vocabulary, so
# a tight-threshold unique best candidate is safe (still needs_review = TRUE)

county_result <- match_polygon_layer(
  layer = "county",
  polygon_class = "county_polygon",
  lookup = counties_lookup,
  auto_match_fn = \(x) x$county,
  match_key_fn = norm_county,
  fuzzy_mode = "accept",
  fuzzy_max_dist = 0.12
)

county_agreements_sf <- county_result$matches |>
  rename(county_match = match_value) |>
  select_layer_columns("county_match")

write_sf_parquet(
  county_agreements_sf,
  "data/county_agreements_sf.parquet"
)

# municipal agreements -----------------------------------------------------
#
# suggestion-only: place/cousub names are an open-ish vocabulary, so
# candidates go to the review CSV for a human to promote into
# inputs/manual-non-facility-polygons.csv

municipal_result <- match_polygon_layer(
  layer = "municipal",
  polygon_class = "municipal_polygon",
  lookup = municipal_lookup,
  auto_match_fn = \(x) extract_city_guess(x$agency),
  match_key_fn = norm_place,
  fuzzy_mode = "suggest",
  fuzzy_max_dist = 0.25
)

municipal_agreements_sf <- municipal_result$matches |>
  rename(
    city_guess = auto_match,
    city_match = match_value
  ) |>
  select_layer_columns(c("city_guess", "city_match"))

write_sf_parquet(
  municipal_agreements_sf,
  "data/municipal_agreements_sf.parquet"
)

# fuzzy suggestions review sheet --------------------------------------------
#
# One row per (unmatched agreement, candidate polygon), plus no_candidate
# rows for agreements nothing came close to. The manual_* columns are
# paste-ready for inputs/manual-non-facility-polygons.csv; promoted rows
# become exact manual matches on the next run and leave the fuzzy path.

unmatched_polygon_suggestions <- bind_rows(
  county_result$fuzzy,
  municipal_result$fuzzy
) |>
  mutate(
    manual_match_layer = layer,
    manual_match_name = suggested_name,
    manual_reason = "fuzzy_promoted",
    manual_note = if_else(
      is.na(match_dist),
      NA_character_,
      sprintf("jw=%.3f", match_dist)
    )
  ) |>
  arrange(status, layer, state, county, agency, match_dist)

write_csv(
  unmatched_polygon_suggestions,
  "data/unmatched_polygon_suggestions.csv"
)
