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

# place and county-subdivision boundaries --------------------------------

# Census LSAD codes carry the legal entity type
lsad_type <- c(
  "21" = "borough",
  "25" = "city",
  "35" = "township",
  "43" = "town",
  "44" = "township",
  "47" = "village",
  "49" = "township"
)

places_sf <- tigris::places(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    state = str_to_title(STATE_NAME),
    place_guess = str_to_title(NAME),
    cand_type = unname(lsad_type[LSAD]),
    statefp = STATEFP,
    countyfp = NA_character_,
    placefp = PLACEFP,
    geoid = GEOID,
    geometry
  )

# pull all states for county subdivisions
states_sf_raw <- tigris::states(cb = TRUE, year = YEAR, class = "sf")

cousubs_sf <-
  unique(states_sf_raw$STATEFP) |>
  map_dfr(function(fp) {
    tigris::county_subdivisions(
      state = fp,
      cb = TRUE,
      year = YEAR,
      class = "sf"
    )
  }) |>
  transmute(
    state = str_to_title(STATE_NAME),
    place_guess = str_to_title(NAME),
    cand_type = unname(lsad_type[LSAD]),
    statefp = STATEFP,
    countyfp = COUNTYFP,
    placefp = COUSUBFP,
    geoid = GEOID,
    geometry
  )

# normalized places lookup: keep every same-named candidate and let the
# sheet's county pick between them below; in New England the town (county
# subdivision) is the municipal government, so it outranks the same-named CDP
new_england <- c(
  "connecticut",
  "maine",
  "massachusetts",
  "newhampshire",
  "rhodeisland",
  "vermont"
)

places_lookup <-
  bind_rows(
    places_sf |> mutate(src = "place"),
    cousubs_sf |> mutate(src = "cousub")
  ) |>
  mutate(
    state_key = norm_state(state),
    place_key = norm_place(place_guess),
    src_rank = case_when(
      state_key %in% new_england & src == "cousub" ~ 1L,
      state_key %in% new_england ~ 2L,
      src == "place" ~ 1L,
      TRUE ~ 2L
    )
  )

# counties each candidate touches: cousubs carry their county FIPS; places
# can span several counties, so take every county they intersect
counties_ref <- tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    state_key = norm_state(STATE_NAME),
    sheet_county_key = norm_county(NAMELSAD),
    sheet_county_fips = GEOID,
    geometry
  )

candidate_counties <- bind_rows(
  places_sf |>
    select(geoid) |>
    st_join(counties_ref |> select(cand_county_fips = sheet_county_fips)) |>
    st_drop_geometry(),
  cousubs_sf |>
    st_drop_geometry() |>
    transmute(geoid, cand_county_fips = paste0(statefp, countyfp))
) |>
  distinct(geoid, cand_county_fips)

# manual municipal overrides --------------------------------------------

municipal_overrides <-
  manual_non_facility_polygons |>
  select(
    agency = agency,
    state,
    county,
    manual_match_layer,
    manual_city_match = manual_match_name,
    manual_reason,
    manual_note
  )

# municipal agreements ---------------------------------------------------

municipal_base <- agencies_all |>
  left_join(
    municipal_overrides,
    by = c("agency", "state", "county")
  ) |>
  filter(
    !(state == "Pennsylvania" &
      str_detect(
        str_to_lower(agency),
        "\\bconstables?\\b"
      ))
  ) |>
  filter(
    manual_match_layer == "municipal" |
      (geom_class == "municipal_polygon" & is.na(manual_match_layer))
  ) |>
  mutate(
    manual_city_match = if_else(
      manual_match_layer == "municipal",
      manual_city_match,
      NA_character_
    ),
    city_guess = extract_city_guess(agency),
    city_match = coalesce(manual_city_match, city_guess),
    state_key = norm_state(state),
    place_key = norm_place(city_match),
    sheet_county_key = norm_county(county),
    municipal_type_hint = case_when(
      str_detect(str_to_lower(agency), "\\btownship\\b|\\btwp\\b") ~ "township",
      str_detect(str_to_lower(agency), "\\bborough\\b|\\bboro\\b") ~ "borough",
      str_detect(str_to_lower(agency), "\\bvillage\\b") ~ "village",
      str_detect(str_to_lower(agency), "\\btown\\b") ~ "town",
      str_detect(str_to_lower(agency), "\\bcity\\b") ~ "city",
      TRUE ~ NA_character_
    ),
    municipal_row_id = row_number()
  ) |>
  left_join(
    counties_ref |>
      st_drop_geometry() |>
      distinct(state_key, sheet_county_key, sheet_county_fips),
    by = c("state_key", "sheet_county_key")
  )

municipal_matches <- municipal_base |>
  left_join(
    places_lookup |>
      as_tibble() |>
      select(
        state_key,
        place_key,
        cand_type,
        statefp,
        countyfp,
        placefp,
        geoid,
        src,
        src_rank,
        geometry
      ),
    by = c("state_key", "place_key"),
    relationship = "many-to-many"
  ) |>
  left_join(
    candidate_counties,
    by = "geoid",
    relationship = "many-to-many"
  ) |>
  mutate(
    county_confirmed = if_else(
      !is.na(sheet_county_fips) & !is.na(cand_county_fips),
      sheet_county_fips == cand_county_fips,
      NA
    ),
    # "Briar Creek Township PD" must take the township, not the same-named
    # borough
    type_match = !is.na(municipal_type_hint) &
      coalesce(cand_type == municipal_type_hint, FALSE)
  ) |>
  group_by(municipal_row_id) |>
  mutate(n_candidates = n_distinct(geoid, na.rm = TRUE)) |>
  arrange(
    desc(coalesce(county_confirmed, FALSE)),
    desc(type_match),
    src_rank,
    geoid,
    .by_group = TRUE
  ) |>
  # candidate_counties carries one row per county a candidate touches; keep
  # the best-ranked row per candidate polygon before picking one
  distinct(geoid, .keep_all = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_ambiguous = !is.na(geoid) &
      ((n_candidates > 1 & !coalesce(county_confirmed, FALSE)) |
        (!is.na(sheet_county_fips) & !coalesce(county_confirmed, TRUE)))
  )

municipal_agreements_sf <- municipal_matches |>
  mutate(
    src = if_else(
      is.na(manual_city_match),
      src,
      paste("manual_municipal_override", src, sep = ":")
    ),
    state_fips = statefp,
    # places carry no county attribute; when the sheet's county was verified
    # against the matched polygon, use it
    county_fips = case_when(
      !is.na(statefp) & !is.na(countyfp) ~ paste0(statefp, countyfp),
      coalesce(county_confirmed, FALSE) ~ sheet_county_fips,
      TRUE ~ NA_character_
    ),
    place_fips = placefp,
    # LEAIC codes the agency's governmental unit independently of our match,
    # so disagreement flags a suspect geometry
    leaic_fips_mismatch = coalesce(
      (!is.na(FSTATE) &
        !is.na(FCOUNTY) &
        !is.na(county_fips) &
        paste0(FSTATE, FCOUNTY) != county_fips) |
        (!is.na(FPLACE) &
          FPLACE != "00000" &
          !is.na(place_fips) &
          FPLACE != place_fips),
      FALSE
    ),
    geometry = st_sfc(
      map(geometry, \(g) {
        if (inherits(g, "sfg")) g else st_geometrycollection()
      }),
      crs = st_crs(places_sf)
    )
  ) |>
  st_as_sf() |>
  mutate(
    needs_review = needs_review |
      match_ambiguous |
      leaic_fips_mismatch |
      is.na(geometry) |
      st_is_empty(geometry)
  ) |>
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
    leaic_fips_mismatch,
    needs_review,
    signed,
    moa,
    addendum,
    city_guess,
    city_match,
    statefp,
    countyfp,
    placefp,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    src,
    manual_match_layer,
    manual_reason,
    manual_note,
    geometry
  )


# save municipal geometries ----------------------------------------------

write_sf_parquet(
  municipal_agreements_sf,
  "data/municipal_agreements_sf.parquet"
)
