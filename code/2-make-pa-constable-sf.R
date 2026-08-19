library(tidyverse)
library(sf)

sf_use_s2(FALSE)

source("code/functions.R")

# municipality boundaries from PA Spatial Data Access
pasda_municipalities <- st_read(
  "inputs/2026-pennsylvania-municipalities/PaMunicipalities2026_04.shp",
  quiet = TRUE
)

# voting district boundaries from PA Legislative Reapportionment Commission (LRC)
lrc_voting_districts <- st_read(
  "inputs/2021-pennsylvania-lrc-voting-district-boundaries/WP_VotingDistricts.shp",
  quiet = TRUE
)

# ward boundaries from PA LRC
lrc_wards <- st_read(
  "inputs/2021-pennsylvania-lrc-voting-district-boundaries/WP_Wards.shp",
  quiet = TRUE
)

# county boundaries from PA LRC; the ward layer carries only a county FIPS, so
# names for keying come from here
lrc_counties <- st_read(
  "inputs/2021-pennsylvania-lrc-voting-district-boundaries/WP_Counties.shp",
  quiet = TRUE
) |>
  st_drop_geometry() |>
  transmute(FIPS, county_name = str_to_title(NAME20))

muni_type_from_pasda <- function(x) {
  case_when(
    str_detect(x, "TWP") ~ "township",
    x == "BORO" ~ "borough",
    x == "CITY" ~ "city",
    TRUE ~ NA_character_
  )
}

muni_type_from_lrc <- function(x) {
  case_when(
    str_to_upper(x) == "TOWNSHIP" ~ "township",
    str_to_upper(x) == "BOROUGH" ~ "borough",
    str_to_upper(x) == "CITY" ~ "city",
    TRUE ~ NA_character_
  )
}

# layer names carry the number after the label word ("Ward 3", "Precinct 2"),
# as digits with an optional ordinal suffix or spelled out first..tenth
parse_layer_number <- function(x, label) {
  token <- str_match(
    x,
    regex(
      paste0(
        "\\b",
        label,
        "\\s+([0-9]+(?:st|nd|rd|th)?|first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth)\\b"
      ),
      ignore_case = TRUE
    )
  )[, 2]
  pa_constable_ordinal_number(token)
}

# the source sheet spells Mifflin County "Miffin"; every lookup layer runs
# through the same repair so keys stay comparable
pa_county_key <- function(x) {
  x |>
    str_replace(regex("\\bMiffin\\b", ignore_case = TRUE), "Mifflin") |>
    norm_place()
}

pasda_lookup <- pasda_municipalities |>
  st_transform(4326) |>
  # PASDA FIPS fields are numeric, so pad back to 2/3 digits
  mutate(
    state_fips = str_pad(as.character(FIPS_STATE), 2, pad = "0"),
    county_fips = paste0(
      state_fips,
      str_pad(as.character(FIPS_COUNT), 3, pad = "0")
    ),
    place_fips = as.character(FIPS_MUN_C),
    geoid = as.character(GEOID),
    municipality_match = str_to_title(MUNICIPAL1),
    municipality_type = muni_type_from_pasda(CLASS_OF_M),
    resolved_county_key = pa_county_key(COUNTY_NAM),
    municipality_key = norm_place(municipality_match)
  ) |>
  select(
    state_fips,
    county_fips,
    place_fips,
    geoid,
    resolved_county_key,
    municipality_match,
    municipality_key,
    municipality_type,
    geometry
  )

# VTDST20 ships as both place_fips and vtd_code: the agreement-level contract
# carries both
vtd_lookup <- lrc_voting_districts |>
  st_transform(4326) |>
  mutate(
    state_fips = STATEFP20,
    county_fips = paste0(STATEFP20, COUNTYFP20),
    place_fips = VTDST20,
    geoid = GEOID20,
    municipality_match = str_to_title(MCD_NAME),
    municipality_type = muni_type_from_lrc(MCD_TYP_NM),
    precinct_number = parse_layer_number(NAME, "precinct"),
    resolved_county_key = pa_county_key(COUNTY_NME),
    municipality_key = norm_place(municipality_match)
  ) |>
  select(
    state_fips,
    county_fips,
    place_fips,
    geoid,
    resolved_county_key,
    municipality_match,
    municipality_key,
    municipality_type,
    precinct_number,
    vtd_code = VTDST20,
    vtd_name = NAME,
    geometry
  )

# the ward layer has no state FIPS field; Pennsylvania's 42 is hardcoded
ward_lookup <- lrc_wards |>
  st_transform(4326) |>
  left_join(lrc_counties, by = "FIPS") |>
  mutate(
    state_fips = "42",
    county_fips = paste0("42", FIPS),
    place_fips = as.character(FIPS_MCD),
    geoid = as.character(cou_cbt_wa),
    municipality_match = str_to_title(MUNICIPALI),
    # LRC encodes the municipality class as CBT: 2 = city, 4 = township,
    # 6 = borough
    municipality_type = case_when(
      CBT == "2" ~ "city",
      CBT == "4" ~ "township",
      CBT == "6" ~ "borough",
      TRUE ~ NA_character_
    ),
    ward_number = parse_layer_number(WARDNAME, "ward"),
    resolved_county_key = pa_county_key(county_name),
    municipality_key = norm_place(municipality_match)
  ) |>
  select(
    state_fips,
    county_fips,
    place_fips,
    geoid,
    resolved_county_key,
    municipality_match,
    municipality_key,
    municipality_type,
    ward_number,
    ward_name = WARDNAME,
    geometry
  )

# constable geometry comes from PASDA/LRC layers, never tigris cousubs; this
# filter must stay the exact complement of 2-make-municipal-sf.R's exclusion so
# the two scripts partition PA municipal agencies
pa_constables <- arrow::read_parquet("data/agreements.parquet") |>
  filter(
    state == "Pennsylvania",
    str_detect(str_to_lower(agency), "\\bconstables?\\b")
  ) |>
  select(agreement_id, county, agency, needs_review)

pa_constables <- bind_cols(
  pa_constables,
  extract_pa_constable_parts(pa_constables$agency)
) |>
  mutate(
    municipality_key = norm_place(municipality_guess),
    # an empty county would key to "" and never match, so treat it as unknown
    source_county_key = na_if(pa_county_key(county), "")
  )

# county and municipality type are post-join gates, not join keys: a constable
# with no usable county or type hint passes rather than losing every candidate
filter_candidates <- function(candidates) {
  candidates |>
    filter(
      is.na(source_county_key) |
        source_county_key == resolved_county_key,
      is.na(municipality_type_hint) |
        municipality_type_hint == municipality_type
    )
}

# unique-match-only: a constable with more than one surviving candidate falls
# through to the unmatched sentinel rather than being arbitrarily assigned
select_unique_matches <- function(candidates, match_type) {
  candidates |>
    add_count(agreement_id, name = "candidate_count") |>
    filter(candidate_count == 1L) |>
    mutate(match_type = .env$match_type) |>
    select(-candidate_count)
}

ward_matches <- pa_constables |>
  filter(pa_constable_jurisdiction == "ward") |>
  inner_join(
    ward_lookup,
    by = c("municipality_key", "ward_number"),
    relationship = "many-to-many"
  ) |>
  filter_candidates() |>
  select_unique_matches("lrc_ward")

precinct_matches <- pa_constables |>
  filter(pa_constable_jurisdiction == "precinct") |>
  inner_join(
    vtd_lookup |>
      filter(!is.na(precinct_number)),
    by = c("municipality_key", "precinct_number"),
    relationship = "many-to-many"
  ) |>
  filter_candidates() |>
  select_unique_matches("lrc_voting_district")

municipality_matches <- pa_constables |>
  filter(pa_constable_jurisdiction == "municipality") |>
  inner_join(
    pasda_lookup,
    by = "municipality_key",
    relationship = "many-to-many"
  ) |>
  filter_candidates() |>
  select_unique_matches("pasda_municipality")

pa_matches <- bind_rows(
  ward_matches,
  precinct_matches,
  municipality_matches
) |>
  transmute(
    agreement_id,
    # the most specific matched unit names the geometry
    match_name = coalesce(vtd_name, ward_name, municipality_match),
    match_type,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    vtd_code,
    geometry
  )

matched <- pa_constables |>
  select(agreement_id, needs_review) |>
  inner_join(pa_matches, by = "agreement_id")

# keep-all: constables with no unique candidate ride along with empty
# geometries and needs_review = TRUE
unmatched <- pa_constables |>
  filter(!agreement_id %in% pa_matches$agreement_id) |>
  transmute(
    agreement_id,
    match_type = "unmatched_pa_constable_geometry",
    state_fips = "42",
    needs_review = TRUE
  )

unmatched <- st_sf(
  unmatched,
  geometry = st_sfc(
    rep(list(st_geometrycollection()), nrow(unmatched)),
    crs = st_crs(4326)
  )
)

pa_constable_sf <- bind_rows(matched, unmatched) |>
  st_as_sf() |>
  arrange(agreement_id) |>
  select(
    agreement_id,
    match_name,
    match_type,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    vtd_code,
    needs_review,
    geometry
  )

stopifnot(
  "every PA constable agreement appears exactly once in the layer" =
    nrow(pa_constable_sf) == nrow(pa_constables)
)

write_sf_parquet(pa_constable_sf, "data/pa-constable-sf.parquet")
