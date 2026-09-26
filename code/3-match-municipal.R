# Matches municipal agreements to census places/county subdivisions -> data/intermediate/match-municipal.parquet

library(tidyverse)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

YEAR <- 2024

agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")
manual_polygons <- arrow::read_parquet("data/intermediate/manual-non-facility-polygons.parquet")
manual_regional <- arrow::read_parquet("data/intermediate/manual-regional-municipalities.parquet")

stopifnot(
  "agreements.parquet must carry the columns the municipal matcher uses" =
    all(
      c("agreement_id", "state", "county", "agency", "jurisdiction_level", "geometry_type", "needs_review") %in%
        names(agreements)
    ),
  "manual-polygons.parquet must carry the manual override columns" =
    all(
      c("agency", "state", "county", "match_layer", "match_name", "reason", "note") %in%
        names(manual_polygons)
    )
)

places_sf <- places(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    state = str_to_title(STATE_NAME),
    place_guess = str_to_title(NAME),
    cand_type = unname(lsad_type[LSAD]),
    statefp = STATEFP,
    placefp = PLACEFP,
    geoid = GEOID,
    geometry
  )

# only subdivisions with a working government can be a police department's jurisdiction
cousubs_sf <- county_subdivisions_reference(YEAR) |>
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

places_lookup <-
  bind_rows(
    places_sf |> mutate(src = "place"),
    cousubs_sf |> mutate(src = "cousub")
  ) |>
  mutate(
    state_key = norm_state(state),
    place_key = norm_place(place_guess),
    src_rank = case_when(
      statefp %in% new_england_fips & src == "cousub" ~ 1L,
      statefp %in% new_england_fips ~ 2L,
      src == "place" ~ 1L,
      TRUE ~ 2L
    )
  )

# places can span several counties, so take every county they intersect; cousubs
# carry their own, except in Connecticut where the 2024 code names a planning
# region and the legacy county comes from the reference polygons
counties_ref <- counties_reference(YEAR) |>
  transmute(state_key, sheet_county_key = county_key, sheet_county_fips = geoid, geometry) |>
  st_transform(st_crs(places_sf))

candidate_counties <- bind_rows(
  places_sf |>
    select(geoid) |>
    st_join(counties_ref |> select(cand_county_fips = sheet_county_fips)) |>
    st_drop_geometry(),
  cousubs_sf |>
    filter(statefp != "09") |>
    st_drop_geometry() |>
    transmute(geoid, cand_county_fips = paste0(statefp, countyfp)),
  # Connecticut towns nest inside the legacy counties, so a point inside the town names its county
  cousubs_sf |>
    filter(statefp == "09") |>
    select(geoid) |>
    st_point_on_surface() |>
    st_join(counties_ref |> select(cand_county_fips = sheet_county_fips)) |>
    st_drop_geometry()
) |>
  distinct(geoid, cand_county_fips)

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

# regional departments: one row per member municipality, unioned at the agreement level. A
# member is the county subdivision of its name and type in its county (Pennsylvania boroughs
# and townships, Michigan cities and townships); where a state's subdivisions are not its
# municipalities (Missouri's townships), it is the place of that name in the county
member_type_word <- \(x) {
  # the last type word names the entity: Velda Village Hills is a city
  str_extract(str_to_lower(x), "\\b(township|borough|village|town|city)\\b(?!.*\\b(township|borough|village|town|city)\\b)")
}
member_candidates <- bind_rows(
  cousubs_sf |> mutate(member_county_fips = paste0(statefp, countyfp), src_rank = 1L),
  places_sf |>
    inner_join(candidate_counties |> rename(member_county_fips = cand_county_fips), by = "geoid") |>
    mutate(src_rank = 2L)
) |>
  transmute(
    state_key = norm_state(state),
    place_key = norm_place(place_guess),
    member_county_fips,
    member_type = cand_type,
    src_rank,
    match_name = place_guess,
    statefp,
    placefp,
    geoid,
    geometry
  )
regional_members <- manual_regional |>
  mutate(
    member_row = row_number(),
    state_key = norm_state(state),
    place_key = norm_place(municipality),
    member_county_key = norm_county(municipality_county),
    # norm_place drops the type word, so Dover borough and Dover township share a key
    member_type = member_type_word(municipality)
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
  left_join(member_candidates, by = c("state_key", "place_key", "member_county_fips", "member_type")) |>
  # a city that is both a subdivision and a place (Michigan) is taken once, as the subdivision
  slice_min(src_rank, n = 1, by = member_row, with_ties = FALSE) |>
  select(-member_row, -src_rank)

# an unresolved member silently shrinks a department's jurisdiction, a doubled one inflates it
stopifnot(
  "every regional member municipality must match one county subdivision" =
    all(!is.na(regional_members$geoid)) &&
      nrow(regional_members) == nrow(manual_regional)
)

regional_sf <- agreements |>
  filter(geometry_type == "polygon", jurisdiction_level == "Regional") |>
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
    geometry_vintage = YEAR,
    geometry_unmatched = FALSE,
    ambiguous_candidates = FALSE,
    type_mismatch = FALSE,
    manual_reason = "regional_department",
    manual_note = note,
    geometry
  )
stopifnot(
  "every department in the member list is a Regional agreement that matched its members" =
    nrow(regional_sf) == nrow(agreements |> inner_join(regional_members, by = c("agency", "state", "county")))
)
# a regional body with no member list rides along unplaced, saying why, rather than being
# read as a town by the city matcher
regional_unmatched <- agreements |>
  filter(geometry_type == "polygon", jurisdiction_level == "Regional") |>
  anti_join(regional_members, by = c("agency", "state", "county")) |>
  transmute(
    agreement_id,
    match_name = NA_character_,
    match_type = "unmatched_regional_members",
    state_fips = NA_character_,
    county_fips = NA_character_,
    place_fips = NA_character_,
    geoid = NA_character_,
    geometry_vintage = NA_integer_,
    geometry_unmatched = TRUE,
    ambiguous_candidates = FALSE,
    type_mismatch = FALSE,
    manual_reason = "regional_department",
    manual_note = "no member list in inputs/manual-regional-municipalities.csv"
  )
regional_unmatched <- st_as_sf(
  regional_unmatched,
  geometry = st_sfc(rep(list(st_geometrycollection()), nrow(regional_unmatched)), crs = st_crs(regional_sf))
)

# municipal agreements

municipal_base <- agreements |>
  anti_join(regional_members, by = c("agency", "state", "county")) |>
  left_join(
    municipal_overrides,
    by = c("agency", "state", "county")
  ) |>
  # exact complement of 3-match-pa-constable.R's inclusion filter; constables must not land here
  filter(
    !(state == "Pennsylvania" & geometry_type == "polygon" & jurisdiction_level == "Municipal" &
      str_detect(
        str_to_lower(agency),
        "\\bconstables?\\b"
      ))
  ) |>
  # un-overridden rows of another class evaluate to NA; filter() drops those
  filter(
    manual_match_layer == "municipal" |
      (geometry_type == "polygon" & jurisdiction_level == "Municipal" & is.na(manual_match_layer))
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
    # township must be tested before town so "X Township" never reads as a town
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
    # a type word inside the candidate's own name is not a type claim ("Cross City" is a town)
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
  # candidate_counties fans one row per county touched, so collapse per polygon before picking
  distinct(geoid, .keep_all = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    ambiguous_candidates = !is.na(geoid) &
      ((n_candidates > 1 & !coalesce(county_confirmed, FALSE)) |
        (!is.na(sheet_county_fips) & !coalesce(county_confirmed, TRUE))),
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
    match_name = if_else(is.na(geoid), NA_character_, place_guess),
    state_fips = statefp,
    # places carry no county attribute, so fall back to the sheet's confirmed county
    county_fips = case_when(
      !is.na(statefp) & !is.na(countyfp) ~ paste0(statefp, countyfp),
      coalesce(county_confirmed, FALSE) ~ sheet_county_fips,
      TRUE ~ NA_character_
    ),
    place_fips = placefp,
    # keep-all: unmatched agreements ride along with an empty sentinel geometry
    geometry = st_sfc(
      map(geometry, \(g) {
        if (inherits(g, "sfg")) g else st_geometrycollection()
      }),
      crs = st_crs(places_sf)
    )
  ) |>
  st_as_sf() |>
  mutate(
    geometry_vintage = if_else(is.na(geoid), NA_integer_, YEAR),
    geometry_unmatched = is.na(geometry) | st_is_empty(geometry)
  ) |>
  select(
    agreement_id,
    match_name,
    match_type,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    geometry_vintage,
    geometry_unmatched,
    ambiguous_candidates,
    type_mismatch,
    manual_reason,
    manual_note,
    geometry
  )

# a polygon with no confirmed county takes the county it overlaps most
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

bind_rows(municipal_sf, st_transform(regional_sf, st_crs(municipal_sf)), st_transform(regional_unmatched, st_crs(municipal_sf))) |>
  st_transform(4326) |>
  sf::st_write(dsn = "data/intermediate/match-municipal.parquet", driver = "Parquet", layer_options = "USE_PARQUET_GEO_TYPES=ONLY", delete_dsn = file.exists("data/intermediate/match-municipal.parquet"), quiet = TRUE)
