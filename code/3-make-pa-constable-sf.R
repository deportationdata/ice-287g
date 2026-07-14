library(tidyverse)
library(sf)
library(tigris)
library(arrow)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

# same vintage as 2-make-non-facility-sf-by-type.R so the cache is shared
YEAR <- 2024

agencies_all <-
  arrow::read_parquet("data/agencies_all.parquet") |>
  normalize_agencies_all()

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

# county names from tigris (3-digit FIPS -> name); county geometry is never
# used here, only names for candidate filtering and QA columns
pa_counties <-
  tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  st_drop_geometry() |>
  filter(STATEFP == "42") |>
  transmute(
    county_fips = COUNTYFP,
    resolved_county = NAME
  )

muni_type_from_namelsad <- function(x) {
  case_when(
    str_detect(x, " township$") ~ "township",
    str_detect(x, " borough$") ~ "borough",
    str_detect(x, " city$") ~ "city",
    str_detect(x, " municipality$") ~ "municipality",
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

pa_county_key <- function(x) {
  x |>
    str_replace(regex("\\bMiffin\\b", ignore_case = TRUE), "Mifflin") |>
    norm_place()
}

# in PA every municipality (township, borough, city) is a county
# subdivision, and NAMELSAD carries the type suffix needed to disambiguate
# same-named municipalities (e.g. Troy township vs Troy borough)
cousub_lookup <-
  tigris::county_subdivisions(
    state = "42",
    cb = TRUE,
    year = YEAR,
    class = "sf"
  ) |>
  st_transform(4326) |>
  st_cast("MULTIPOLYGON") |>
  left_join(pa_counties, by = c("COUNTYFP" = "county_fips")) |>
  mutate(
    src = paste0("tigris_cousub_", YEAR),
    state_fips = STATEFP,
    county_fips = paste0(STATEFP, COUNTYFP),
    place_fips = COUSUBFP,
    geoid = GEOID,
    municipality_match = NAME,
    municipality_type = muni_type_from_namelsad(NAMELSAD),
    resolved_county_key = pa_county_key(resolved_county),
    municipality_key = norm_place(municipality_match)
  ) |>
  select(
    src,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    resolved_county,
    resolved_county_key,
    municipality_match,
    municipality_key,
    municipality_type,
    geometry
  )

vtd_lookup <- lrc_voting_districts |>
  st_transform(4326) |>
  st_cast("MULTIPOLYGON") |>
  mutate(
    src = "lrc_voting_districts_2021",
    state_fips = STATEFP20,
    county_fips = paste0(STATEFP20, COUNTYFP20),
    place_fips = VTDST20,
    geoid = GEOID20,
    resolved_county = str_to_title(COUNTY_NME),
    municipality_match = str_to_title(MCD_NAME),
    municipality_type = muni_type_from_lrc(MCD_TYP_NM),
    ward_number = parse_layer_number(NAME, "ward"),
    precinct_number = parse_layer_number(NAME, "precinct"),
    resolved_county_key = pa_county_key(resolved_county),
    municipality_key = norm_place(municipality_match)
  ) |>
  select(
    src,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    resolved_county,
    resolved_county_key,
    municipality_match,
    municipality_key,
    municipality_type,
    ward_number,
    precinct_number,
    vtd_code = VTDST20,
    vtd_name = NAME,
    geometry
  )

ward_component_counts <-
  vtd_lookup |>
  st_drop_geometry() |>
  filter(!is.na(ward_number)) |>
  count(
    resolved_county_key,
    municipality_key,
    municipality_type,
    ward_number,
    name = "ward_component_vtd_count"
  )

ward_lookup <-
  lrc_wards |>
  st_transform(4326) |>
  st_cast("MULTIPOLYGON") |>
  left_join(pa_counties, by = c("FIPS" = "county_fips")) |>
  mutate(
    src = "lrc_wards_2021",
    state_fips = "42",
    # the wards shapefile carries a 3-digit county FIPS; every other layer
    # writes the 5-digit state+county code
    county_fips = paste0("42", FIPS),
    place_fips = as.character(FIPS_MCD),
    geoid = as.character(cou_cbt_wa),
    municipality_match = str_to_title(MUNICIPALI),
    municipality_type = case_when(
      CBT == "2" ~ "city",
      CBT == "4" ~ "township",
      CBT == "6" ~ "borough",
      TRUE ~ NA_character_
    ),
    ward_number = parse_layer_number(WARDNAME, "ward"),
    resolved_county_key = pa_county_key(resolved_county),
    municipality_key = norm_place(municipality_match)
  ) |>
  left_join(
    ward_component_counts,
    by = c(
      "resolved_county_key",
      "municipality_key",
      "municipality_type",
      "ward_number"
    )
  ) |>
  select(
    src,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    resolved_county,
    resolved_county_key,
    municipality_match,
    municipality_key,
    municipality_type,
    ward_number,
    ward_name = WARDNAME,
    ward_component_vtd_count,
    geometry
  )

pa_constables <-
  agencies_all |>
  filter(is_pa_constable_agency(state, agency)) |>
  mutate(
    pa_constable_row_id = row_number(),
    source_county_key = pa_county_key(county)
  )

pa_constables <- bind_cols(
  pa_constables,
  extract_pa_constable_parts(pa_constables$agency)
) |>
  mutate(
    municipality_key = norm_place(municipality_guess),
    source_county_key = na_if(source_county_key, "")
  )

filter_candidates <- function(candidates) {
  candidates |>
    filter(
      is.na(source_county_key) |
        source_county_key == resolved_county_key,
      is.na(municipality_type_hint) |
        municipality_type_hint == municipality_type
    )
}

# keep only constables with exactly one candidate; ambiguous ones fall
# through to the unmatched/review path
select_unique_matches <- function(candidates, match_type) {
  candidates |>
    add_count(pa_constable_row_id, name = "candidate_count") |>
    filter(candidate_count == 1L) |>
    mutate(match_type = match_type)
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
    cousub_lookup,
    by = "municipality_key",
    relationship = "many-to-many"
  ) |>
  filter_candidates() |>
  select_unique_matches("tigris_cousub")

pa_matches <- bind_rows(
  ward_matches,
  precinct_matches,
  municipality_matches
) |>
  mutate(
    needs_geometry_review = FALSE,
    county_match_status = case_when(
      is.na(source_county_key) ~ "county_inferred_from_geometry",
      source_county_key == resolved_county_key ~ "county_matched",
      TRUE ~ "county_mismatch"
    ),
    ward_vtd_qa_status = case_when(
      match_type == "lrc_ward" & !is.na(ward_component_vtd_count) ~
        paste0("ward_has_", ward_component_vtd_count, "_vtd_components"),
      match_type == "lrc_ward" ~ "ward_has_no_vtd_components",
      TRUE ~ NA_character_
    )
  ) |>
  select(
    pa_constable_row_id,
    src,
    match_type,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    resolved_county,
    county_match_status,
    municipality_match,
    municipality_type,
    ward_name,
    ward_vtd_qa_status,
    vtd_code,
    vtd_name,
    needs_geometry_review,
    geometry
  )

matched_ids <- pa_matches |>
  st_drop_geometry() |>
  pull(pa_constable_row_id)

unmatched <- pa_constables |>
  filter(!pa_constable_row_id %in% matched_ids) |>
  mutate(
    src = NA_character_,
    match_type = "unmatched_pa_constable_geometry",
    state_fips = "42",
    county_fips = NA_character_,
    place_fips = NA_character_,
    geoid = NA_character_,
    resolved_county = NA_character_,
    county_match_status = if_else(
      is.na(source_county_key),
      "county_missing",
      "county_not_resolved"
    ),
    municipality_match = NA_character_,
    municipality_type = NA_character_,
    ward_name = NA_character_,
    ward_vtd_qa_status = NA_character_,
    vtd_code = NA_character_,
    vtd_name = NA_character_,
    needs_geometry_review = TRUE
  ) |>
  select(
    pa_constable_row_id,
    src,
    match_type,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    resolved_county,
    county_match_status,
    municipality_match,
    municipality_type,
    ward_name,
    ward_vtd_qa_status,
    vtd_code,
    vtd_name,
    needs_geometry_review
  )

# empty MULTIPOLYGON keeps the geometry type consistent with the matched rows
unmatched_sf <- st_sf(
  unmatched,
  geometry = st_sfc(
    rep(list(st_multipolygon()), nrow(unmatched)),
    crs = st_crs(4326)
  )
)

# one row per pa_constable_row_id, so the join below hits every constable
match_sf <- bind_rows(pa_matches, unmatched_sf) |>
  st_as_sf()

pa_constable_agreements_sf <- pa_constables |>
  left_join(as_tibble(match_sf), by = "pa_constable_row_id") |>
  st_as_sf() |>
  mutate(
    needs_review = needs_review | needs_geometry_review,
    review_reason = case_when(
      is.na(municipality_guess) | municipality_guess == "" ~
        "missing_municipality_guess",
      needs_geometry_review ~ match_type,
      needs_review ~ "agency_flagged_by_source",
      TRUE ~ NA_character_
    )
  ) |>
  arrange(pa_constable_row_id) |>
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
    municipality_guess,
    municipality_type_hint,
    pa_constable_jurisdiction,
    ward_number,
    precinct_number,
    src,
    match_type,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    resolved_county,
    county_match_status,
    municipality_match,
    municipality_type,
    ward_name,
    ward_vtd_qa_status,
    vtd_code,
    vtd_name,
    review_reason,
    geometry
  )

review_reasons <- pa_constable_agreements_sf |>
  st_drop_geometry() |>
  filter(!is.na(review_reason)) |>
  select(
    review_reason,
    state,
    county,
    resolved_county,
    agency,
    municipality_guess,
    municipality_type_hint,
    pa_constable_jurisdiction,
    ward_number,
    precinct_number,
    match_type,
    src,
    municipality_match,
    vtd_name,
    ward_name,
    ward_vtd_qa_status
  )

write_sf_parquet(
  pa_constable_agreements_sf,
  "data/pa_constable_agreements_sf.parquet"
)

readr::write_csv(
  review_reasons,
  "data/pa_constable_matches_needing_review.csv"
)
