library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

YEAR <- 2024

agreements <- arrow::read_parquet("data/agreements.parquet")
manual_polygons <- arrow::read_parquet("data/manual-polygons.parquet")
manual_regional <- arrow::read_parquet("data/manual-regional.parquet")

stopifnot(
  "agreements.parquet must carry the columns the municipal matcher uses" =
    all(
      c("agreement_id", "state", "county", "agency", "geom_class", "needs_review") %in%
        names(agreements)
    ),
  "manual-polygons.parquet must carry the manual override columns" =
    all(
      c("agency", "state", "county", "match_layer", "match_name", "reason", "note") %in%
        names(manual_polygons)
    )
)

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
    placefp = PLACEFP,
    geoid = GEOID,
    geometry
  )

# tigris serves county subdivisions one state at a time
cousubs_sf <-
  tigris::states(cb = TRUE, year = YEAR, class = "sf")$STATEFP |>
  unique() |>
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

# keep every same-named candidate and let the sheet's county pick between them
# below; in New England the town (county subdivision) is the municipal
# government, so it outranks the same-named CDP
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

municipal_overrides <- manual_polygons |>
  select(
    agency,
    state,
    county,
    manual_match_layer = match_layer,
    manual_city_match = match_name,
    manual_reason = reason,
    manual_note = note
  )

# regional departments ---------------------------------------------------

# a regional department polices a set of municipalities, so it gets one row per
# member and the agreement-level union covers the whole force. Members are
# matched as county subdivisions, which is what a Pennsylvania borough or
# township is, and the county disambiguates repeated names
regional_members <- manual_regional |>
  mutate(
    state_key = norm_state(state),
    place_key = norm_place(municipality),
    member_county_key = norm_county(municipality_county),
    # norm_place drops the type word, so Dover borough and Dover township share
    # a key; the type written in the input file tells them apart
    member_type = case_when(
      str_detect(str_to_lower(municipality), "\\btownship\\b") ~ "township",
      str_detect(str_to_lower(municipality), "\\bborough\\b") ~ "borough",
      str_detect(str_to_lower(municipality), "\\bvillage\\b") ~ "village",
      str_detect(str_to_lower(municipality), "\\btown\\b") ~ "town",
      str_detect(str_to_lower(municipality), "\\bcity\\b") ~ "city",
      TRUE ~ NA_character_
    )
  ) |>
  left_join(
    counties_ref |>
      st_drop_geometry() |>
      distinct(
        state_key,
        member_county_key = sheet_county_key,
        member_county_fips = sheet_county_fips
      ),
    by = c("state_key", "member_county_key")
  ) |>
  left_join(
    cousubs_sf |>
      mutate(
        state_key = norm_state(state),
        place_key = norm_place(place_guess),
        member_county_fips = paste0(statefp, countyfp),
        member_type = cand_type
      ) |>
      select(
        state_key,
        place_key,
        member_county_fips,
        member_type,
        match_name = place_guess,
        statefp,
        placefp,
        geoid,
        geometry
      ),
    by = c("state_key", "place_key", "member_county_fips", "member_type")
  )

# a member that fails to resolve would silently shrink a department's
# jurisdiction, and one that matches twice would double-count it, so either
# must stop the run rather than ship a wrong boundary
stopifnot(
  "every regional member municipality must match one county subdivision" =
    all(!is.na(regional_members$geoid)) &&
      nrow(regional_members) == nrow(manual_regional)
)

regional_sf <- agreements |>
  inner_join(regional_members, by = c("agency", "state", "county")) |>
  st_as_sf() |>
  transmute(
    agreement_id,
    match_name,
    match_type = "regional_member_municipality",
    state_fips = statefp,
    county_fips = member_county_fips,
    place_fips = placefp,
    geoid,
    needs_review,
    match_ambiguous = FALSE,
    type_mismatch = FALSE,
    manual_reason = "regional_department",
    manual_note = note,
    geometry
  )

# municipal agreements ---------------------------------------------------

municipal_base <- agreements |>
  # regional departments are matched above, one row per member municipality
  anti_join(regional_members, by = c("agency", "state", "county")) |>
  left_join(
    municipal_overrides,
    by = c("agency", "state", "county")
  ) |>
  # the exact complement of 2-make-pa-constable-sf.R's inclusion filter, which
  # matches constables to wards and precincts instead
  filter(
    !(state == "Pennsylvania" &
      str_detect(
        str_to_lower(agency),
        "\\bconstables?\\b"
      ))
  ) |>
  # a manual "municipal" layer pulls a row in regardless of geom_class, and a
  # manual non-municipal layer routes a municipal_polygon row to another script
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
    # township is tested before town so "X Township" never reads as a town
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
        place_guess,
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
    # borough — but only when the token is not part of the candidate's own
    # name ("Cross City" is a town named Cross City)
    hint_is_type_claim = !is.na(municipal_type_hint) &
      !coalesce(
        str_detect(
          str_to_lower(place_guess),
          paste0("\\b", municipal_type_hint, "\\b")
        ),
        FALSE
      ),
    type_match = hint_is_type_claim &
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
  # candidate_counties holds one row per county a candidate touches, so collapse
  # to the best-ranked row per polygon before picking a winner; geoid is the
  # deterministic final tiebreak
  distinct(geoid, .keep_all = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_ambiguous = !is.na(geoid) &
      ((n_candidates > 1 & !coalesce(county_confirmed, FALSE)) |
        (!is.na(sheet_county_fips) & !coalesce(county_confirmed, TRUE))),
    # the agency name claims an entity type the winning candidate lacks
    type_mismatch = !is.na(geoid) &
      hint_is_type_claim &
      !coalesce(cand_type == municipal_type_hint, FALSE)
  )

municipal_sf <- municipal_matches |>
  mutate(
    match_type = case_when(
      is.na(geoid) ~ "unmatched",
      !is.na(manual_city_match) ~ "manual_override",
      src == "cousub" ~ "cousub_name",
      src == "place" ~ "place_name"
    ),
    # ships the matched polygon's own name, not the query that found it
    match_name = if_else(is.na(geoid), NA_character_, place_guess),
    state_fips = statefp,
    # places carry no county attribute, so fall back to the sheet's county once
    # the matched polygon has confirmed it
    county_fips = case_when(
      !is.na(statefp) & !is.na(countyfp) ~ paste0(statefp, countyfp),
      coalesce(county_confirmed, FALSE) ~ sheet_county_fips,
      TRUE ~ NA_character_
    ),
    place_fips = placefp,
    # unmatched agreements ride along with an empty sentinel geometry
    geometry = st_sfc(
      map(geometry, \(g) {
        if (inherits(g, "sfg")) g else st_geometrycollection()
      }),
      crs = st_crs(places_sf)
    )
  ) |>
  st_as_sf() |>
  mutate(
    # match_ambiguous and type_mismatch stay separate columns rather than
    # folding in here: 5-format clears them when LEAIC's independently coded
    # place confirms the match, and flags them otherwise
    needs_review = needs_review |
      is.na(geometry) |
      st_is_empty(geometry)
  ) |>
  select(
    agreement_id,
    match_name,
    match_type,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    needs_review,
    match_ambiguous,
    type_mismatch,
    manual_reason,
    manual_note,
    geometry
  )

# a matched municipality whose sheet county is missing or unverified still
# lacks one, so take the county overlapping the polygon most (planar areas are
# fine for ranking overlaps of a single polygon)
county_overlap <- municipal_sf |>
  filter(!is.na(geoid), is.na(county_fips)) |>
  select(agreement_id) |>
  st_transform(3857) |>
  st_intersection(
    counties_ref |>
      select(overlap_county_fips = sheet_county_fips) |>
      st_transform(3857)
  ) |>
  mutate(overlap_area = st_area(geometry)) |>
  st_drop_geometry() |>
  group_by(agreement_id) |>
  arrange(desc(overlap_area), overlap_county_fips, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(agreement_id, overlap_county_fips)

municipal_sf <- municipal_sf |>
  left_join(county_overlap, by = "agreement_id") |>
  mutate(county_fips = coalesce(county_fips, overlap_county_fips)) |>
  select(-overlap_county_fips)

# save municipal geometries ----------------------------------------------

bind_rows(municipal_sf, st_transform(regional_sf, st_crs(municipal_sf))) |>
  st_transform(4326) |>
  write_sf_parquet("data/municipal-sf.parquet")
