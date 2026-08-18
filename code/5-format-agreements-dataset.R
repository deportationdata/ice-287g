library(tidyverse)
library(sf)

source("code/functions.R")

non_facility_sf <- read_sf_parquet("data/non-facility-sf.parquet")
facility_sf <- read_sf_parquet("data/facility-sf.parquet")

agreements <- arrow::read_parquet("data/agreements.parquet")

# ORIs and roster county codes are annotations rather than match inputs, so
# they join on by agreement_id here; the per-source match_type provenance stays
# in 4-match-rosters.R's own table
agreement_identifiers <-
  arrow::read_parquet("data/agreement-identifiers.parquet") |>
  select(
    agreement_id,
    ORI9,
    ori_source,
    ori_conflict,
    roster_key_unique,
    leaic_ori,
    leaic_name,
    leaic_county_fips,
    leaic_place_fips,
    lear_ori,
    lear_name,
    lear_county_fips,
    crime_ori,
    crime_name,
    crime_county_fips,
    hifld_county_fips
  )

stopifnot(
  "non-facility layer file must arrive in EPSG:4326" =
    st_crs(non_facility_sf) == st_crs(4326),
  "facility layer file must arrive in EPSG:4326" =
    st_crs(facility_sf) == st_crs(4326)
)

# unmatched agreements ride along with empty geometries; nothing is dropped
all_agreements_sf <-
  bind_rows(
    non_facility_sf,
    facility_sf |>
      mutate(match_layer = "facility")
  ) |>
  st_make_valid() |>
  # layer files carry only match results; the sheet's descriptive columns come
  # back from agreements.parquet
  left_join(
    agreements |>
      select(
        agreement_id,
        state,
        county,
        agency,
        agency_level,
        support_type,
        signed,
        moa,
        addendum,
        geom_class
      ),
    by = "agreement_id"
  ) |>
  left_join(agreement_identifiers, by = "agreement_id") |>
  mutate(
    # facility rows have no census polygon; the published contract that
    # slicer-shiny-app reads carries the facility's county fips as its geoid
    geoid = if_else(match_layer == "facility", county_fips, geoid),
    # each roster codes the agency's county independently of the match, so
    # disagreement flags a suspect geometry; only layers matched against a
    # county or place boundary can be checked this way
    checkable_layer = match_layer %in% c("county", "municipal"),
    leaic_fips_mismatch = case_when(
      match_layer == "county" ~ coalesce(
        !is.na(leaic_county_fips) &
          !is.na(county_fips) &
          leaic_county_fips != county_fips,
        FALSE
      ),
      match_layer == "municipal" ~ coalesce(
        (!is.na(leaic_county_fips) &
          !is.na(county_fips) &
          leaic_county_fips != county_fips) |
          (!is.na(leaic_place_fips) &
            leaic_place_fips != "00000" &
            !is.na(place_fips) &
            leaic_place_fips != place_fips),
        FALSE
      ),
      TRUE ~ NA
    ),
    lear_fips_mismatch = if_else(
      checkable_layer,
      coalesce(
        !is.na(lear_county_fips) &
          !is.na(county_fips) &
          lear_county_fips != county_fips,
        FALSE
      ),
      NA
    ),
    # CDE can list several counties for one agency ("LEE, MACON" becomes
    # "01081;01087"), so test membership rather than equality
    crime_fips_mismatch = if_else(
      checkable_layer,
      coalesce(
        !is.na(crime_county_fips) &
          !is.na(county_fips) &
          !str_detect(crime_county_fips, fixed(county_fips)),
        FALSE
      ),
      NA
    ),
    hifld_fips_mismatch = if_else(
      checkable_layer,
      coalesce(
        !is.na(hifld_county_fips) &
          !is.na(county_fips) &
          hifld_county_fips != county_fips,
        FALSE
      ),
      NA
    ),
    # an ambiguous municipal match clears when LEAIC's independently coded place
    # lands on the same census place and no roster shows a second agency with
    # that cleaned name statewide, since a shared name cannot identify a
    # record; 99xxx are balance-of-county pseudo-codes
    leaic_place_confirmed = coalesce(
      match_layer == "municipal" &
        !is.na(leaic_place_fips) &
        leaic_place_fips != "00000" &
        !str_starts(leaic_place_fips, "99") &
        !is.na(place_fips) &
        leaic_place_fips == place_fips &
        roster_key_unique,
      FALSE
    ),
    needs_review = needs_review |
      ((coalesce(match_ambiguous, FALSE) | coalesce(type_mismatch, FALSE)) &
        !leaic_place_confirmed) |
      coalesce(leaic_fips_mismatch, FALSE) |
      coalesce(lear_fips_mismatch, FALSE) |
      coalesce(crime_fips_mismatch, FALSE) |
      coalesce(hifld_fips_mismatch, FALSE) |
      coalesce(ori_conflict, FALSE)
  ) |>
  select(
    agreement_id,
    state,
    county,
    agency,
    agency_level,
    support_type,
    signed,
    moa,
    addendum,
    geom_class,
    match_layer,
    match_name,
    match_type,
    match_score,
    detention_facility_code,
    source,
    source_id,
    facility_address,
    facility_city,
    facility_state,
    facility_zip,
    facility_operator_name,
    latitude,
    longitude,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    vtd_code,
    ORI9,
    ori_source,
    ori_conflict,
    leaic_ori,
    leaic_name,
    leaic_county_fips,
    leaic_place_fips,
    lear_ori,
    lear_name,
    lear_county_fips,
    crime_ori,
    crime_name,
    crime_county_fips,
    hifld_county_fips,
    roster_key_unique,
    leaic_fips_mismatch,
    lear_fips_mismatch,
    crime_fips_mismatch,
    hifld_fips_mismatch,
    leaic_place_confirmed,
    match_ambiguous,
    type_mismatch,
    university_address_mismatch,
    manual_reason,
    manual_note,
    needs_review,
    geometry
  )

write_sf_parquet(all_agreements_sf, "data/all_agreements_sf.parquet")

# an agreement can span several matched features (a DOC's facilities sit in
# many counties), so codes are kept only when they identify a single area
single_or_na <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 1) ux else NA_character_
}

agreement_level_sf <- all_agreements_sf |>
  group_by(
    agreement_id,
    agency,
    state,
    county,
    signed,
    moa,
    addendum,
    ORI9,
    support_type,
    agency_level,
    geom_class,
    state_fips
  ) |>
  summarize(
    match_layer = paste(sort(unique(match_layer)), collapse = "+"),
    county_fips = single_or_na(county_fips),
    place_fips = single_or_na(place_fips),
    geoid = single_or_na(geoid),
    vtd_code = single_or_na(vtd_code),
    geometry = st_union(geometry),
    .groups = "drop"
  ) |>
  select(
    # preserve the source spreadsheet order, followed by derived/spatial fields
    state,
    agency,
    agency_level,
    county,
    support_type,
    signed,
    moa,
    addendum,
    ORI9,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    vtd_code,
    geometry_type = geom_class,
    match_layer,
    geometry
  )

stopifnot(
  "agreement-level dataset must have one row per agreement in the source sheet" =
    nrow(agreement_level_sf) == nrow(agreements)
)

write_sf_parquet(agreement_level_sf, "data/agreement-level-sf.parquet")
