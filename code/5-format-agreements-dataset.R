library(tidyverse)
library(sf)
library(arrow)

source("code/functions.R")

non_facility_agreements_sf <- read_sf_parquet(
  "data/non_facility_agreements_sf.parquet"
)

facility_agreements_sf <- read_sf_parquet(
  "data/facility_agreements_sf.parquet"
)

# ORIs and each source's county codes are annotations rather than match
# inputs, so they are merged in 4-merge-agency-oris.R and joined on here by
# agreement_id
agency_identifiers <- arrow::read_parquet("data/agencies_all.parquet") |>
  select(
    agreement_id,
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
    ori_conflict,
    lear_county_fips,
    crime_county_fips,
    hifld_county_fips,
    roster_key_unique
  )

# unmatched agreements ride along with empty geometries; nothing is dropped
all_agreements_sf <-
  bind_rows(
    non_facility_agreements_sf,
    facility_agreements_sf |>
      st_transform(4326) |>
      mutate(match_layer = "facility")
  ) |>
  st_make_valid() |>
  st_transform(4326) |>
  left_join(agency_identifiers, by = "agreement_id") |>
  mutate(
    # each roster codes the agency's county independently of our match, so
    # disagreement flags a suspect geometry. Only the layers matched against a
    # county or place boundary can be checked this way.
    checkable_layer = match_layer %in% c("county", "municipal"),
    leaic_fips_mismatch = case_when(
      match_layer == "county" ~ coalesce(
        !is.na(FSTATE) &
          !is.na(FCOUNTY) &
          !is.na(county_fips) &
          paste0(FSTATE, FCOUNTY) != county_fips,
        FALSE
      ),
      match_layer == "municipal" ~ coalesce(
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
    # an ambiguous municipal match is cleared when LEAIC's independently coded
    # FPLACE lands on the same census place AND no roster shows a second
    # agency with the same cleaned name statewide (a shared name cannot be
    # trusted to identify a record); 99xxx are balance-of-county pseudo-codes
    leaic_place_confirmed = coalesce(
      match_layer == "municipal" &
        !is.na(FPLACE) &
        FPLACE != "00000" &
        !str_starts(FPLACE, "99") &
        !is.na(place_fips) &
        FPLACE == place_fips &
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
  select(-checkable_layer)

write_sf_parquet(
  all_agreements_sf,
  "data/all_agreements_sf.parquet"
)

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
    # Preserve the source spreadsheet order, followed by derived/spatial fields.
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

# one row per agreement in the source spreadsheet
agencies_all <- arrow::read_parquet("data/agencies_all.parquet")
stopifnot(nrow(agreement_level_sf) == nrow(agencies_all))

write_sf_parquet(
  agreement_level_sf,
  "data/agreement-level-sf.parquet"
)
