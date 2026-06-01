library(tidyverse)
library(sf)
library(stringdist)
library(arrow)

source("code/functions.R")

agencies_all <- arrow::read_parquet("data/agencies_all.parquet") |>
  normalize_agencies_all()
facilities_tbl <- read_sf_parquet("data/facilities_tbl.parquet") |>
  st_drop_geometry()
hifld_facility_tbl <- arrow::read_parquet(
  "data/hifld_facility_tbl.parquet"
)
manual_points <- arrow::read_parquet("data/manual_points.parquet") |>
  mutate(
    across(
      c(
        county,
        manual_address,
        manual_city,
        manual_county,
        manual_zip,
        manual_note
      ),
      as.character
    )
  )

facility_sources_exact <- bind_rows(
  facilities_tbl,
  hifld_facility_tbl
)

# 287(g) facility-model agencies -----------------------------------------

doc_pattern <- paste(
  "department of corrections",
  "correctional services",
  "public safety & corrections",
  "division of corrections",
  "department of public safety",
  sep = "|"
)

fac_287g <- agencies_all |>
  filter(geom_class == "facility_point") |>
  transmute(
    state,
    county,
    agency,
    support_type,
    agency_level,
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
    support_clean,
    has_addendum,
    moa_pending
  ) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_place(county),
    agency_key = norm_key(agency),
    facility_guess = extract_facility_guess(agency),
    facility_guess_key = norm_key(facility_guess),
    is_doc_agency = str_detect(str_to_lower(agency), doc_pattern) &
      agency_level == "state"
  )

# match detention facilities to facility datasets ------------------------

facility_exact_matches <- fac_287g |>
  filter(!is_doc_agency) |>
  inner_join(
    facility_sources_exact,
    by = c("state_key", "county_key", "facility_guess_key" = "facility_key"),
    relationship = "many-to-many"
  ) |>
  mutate(
    match_type = "exact_state_county_facility_name",
    match_score = 1.0
  ) |>
  group_by(state, county, agency, support_clean) |>
  arrange(source_rank) |>
  slice_head(n = 1) |>
  ungroup()

facility_unmatched_after_exact <- fac_287g |>
  filter(!is_doc_agency) |>
  anti_join(facility_exact_matches, by = c("state", "county", "agency"))

# use fuzzy string matching on facility names within same county ---------

facility_fuzzy_county <-
  facility_unmatched_after_exact |>
  inner_join(
    facility_sources_exact,
    by = c("state_key", "county_key"),
    relationship = "many-to-many"
  ) |>
  mutate(
    match_dist = stringdist(
      facility_guess_key,
      facility_key,
      method = "jw",
      p = 0.1
    )
  ) |>
  filter(match_dist <= 0.22) |>
  group_by(state, county, agency) |>
  arrange(match_dist, source_rank) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_type = "fuzzy_county_facility",
    match_score = 1 - match_dist,
    needs_review = TRUE
  )

facility_unmatched_after_fuzzy_county <-
  facility_unmatched_after_exact |>
  anti_join(facility_fuzzy_county, by = c("state", "county", "agency"))

# broader state-level fuzzy matching for remaining unmatched facilities ----

facility_fuzzy_state <- facility_unmatched_after_fuzzy_county |>
  inner_join(
    facility_sources_exact,
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  mutate(
    match_dist = stringdist(
      facility_guess_key,
      facility_key,
      method = "jw",
      p = 0.1
    )
  ) |>
  filter(match_dist <= 0.15) |>
  group_by(state, county, agency) |>
  arrange(match_dist, source_rank) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_type = "fuzzy_state_facility",
    match_score = 1 - match_dist,
    needs_review = TRUE
  )

# DOC: match to all facilities in state ----------------------------------

doc_facility_pattern <- paste(
  "correctional",
  "prison",
  "penitentiary",
  "detention",
  "work camp",
  "re-?entry",
  "work release",
  "work program",
  "transition(al)? center",
  "classification",
  "diagnostic",
  "treatment facility",
  sep = "|"
)

doc_exclude_pattern <- paste(
  "county jail",
  "parish jail",
  "city jail",
  "municipal jail",
  "sheriff",
  "police",
  "courthouse",
  "regional lock-?up",
  sep = "|"
)

doc_candidates <- fac_287g |>
  filter(is_doc_agency) |>
  inner_join(
    facility_sources_exact,
    by = "state_key",
    relationship = "many-to-many"
  )

doc_matches <- doc_candidates |>
  filter(
    str_detect(str_to_lower(facility_name), doc_facility_pattern),
    !str_detect(str_to_lower(facility_name), doc_exclude_pattern)
  ) |>
  arrange(state, county, agency, source_rank) |>
  group_by(state, county, agency, facility_key) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_type = "state_doc_all_facilities",
    match_score = 1,
    needs_review = TRUE
  )

# manual matches ---------------------------------------------------------

manual_points_specific <-
  manual_points |>
  filter(!is.na(county), county != "")

manual_points_general <-
  manual_points |>
  filter(is.na(county) | county == "")

manual_matches_specific <-
  fac_287g |>
  inner_join(
    manual_points_specific,
    by = c("agency", "state", "county")
  )

manual_matches_general <-
  fac_287g |>
  inner_join(
    manual_points_general |> select(-county),
    by = c("agency", "state")
  )

manual_matches <-
  bind_rows(
    manual_matches_specific,
    manual_matches_general
  ) |>
  distinct(state, county, agency, .keep_all = TRUE) |>
  transmute(
    state,
    county,
    agency,
    support_type,
    agency_level,
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
    needs_review = TRUE,
    support_clean,
    has_addendum,
    moa_pending,
    state_key,
    county_key,
    agency_key,
    facility_guess,
    facility_guess_key,
    is_doc_agency,
    source = "manual",
    source_rank = 0L,
    detention_facility_code = NA_character_,
    facility_name = manual_facility_name,
    facility_address = manual_address,
    facility_city = manual_city,
    facility_county = coalesce(manual_county, county),
    facility_county_fips = NA_character_,
    facility_state = coalesce(manual_state, state),
    facility_state_fips = NA_character_,
    facility_zip = manual_zip,
    facility_address_full = str_squish(
      paste(manual_address, manual_city, manual_state, manual_zip, sep = ", ")
    ),
    facility_latitude = latitude,
    facility_longitude = longitude,
    facility_field_office = NA_character_,
    facility_key = norm_key(manual_facility_name),
    match_type = "manual",
    match_score = 1,
    manual_reason,
    manual_note
  )

# combine all matches ----------------------------------------------------

non_doc_matches <-
  bind_rows(
    facility_exact_matches,
    facility_fuzzy_county,
    facility_fuzzy_state
  ) |>
  group_by(state, county, agency) |>
  arrange(source_rank, desc(match_score)) |>
  slice_head(n = 1) |>
  ungroup()

auto_matches <-
  bind_rows(
    non_doc_matches,
    doc_matches
  )

facility_all_matches <-
  bind_rows(
    manual_matches,
    auto_matches |>
      anti_join(manual_matches, by = c("state", "county", "agency"))
  )

facility_unmatched_final <- fac_287g |>
  anti_join(facility_all_matches, by = c("state", "county", "agency")) |>
  mutate(
    match_type = "unmatched",
    source = NA_character_,
    source_rank = NA_integer_,
    needs_review = TRUE
  )

readr::write_csv(
  facility_unmatched_final |>
    select(state, county, agency, agency_level, support_clean, facility_guess),
  "data/facility_unmatched.csv"
)

# facility point sf layer ------------------------------------------------

facility_agreements_sf <-
  facility_all_matches |>
  filter(!is.na(facility_latitude), !is.na(facility_longitude)) |>
  st_as_sf(
    coords = c("facility_longitude", "facility_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  mutate(
    geom_class = "facility_point",
    src = source,
    state_fips = facility_state_fips,
    county_fips = facility_county_fips,
    place_fips = NA_character_,
    geoid = county_fips,
    needs_review = needs_review |
      source != "facilities" |
      match_type != "exact_state_county_facility_name" |
      has_addendum |
      moa_pending
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
    needs_review,
    has_addendum,
    moa_pending,
    facility_guess,
    facility_name,
    source,
    src,
    match_type,
    match_score,
    facility_address,
    facility_city,
    facility_county,
    facility_county_fips,
    facility_state,
    facility_state_fips,
    facility_zip,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    facility_latitude,
    facility_longitude,
    geometry
  )

# diagnostics ------------------------------------------------------------

facility_review <-
  facility_all_matches |>
  as_tibble() |>
  select(-any_of("geometry")) |>
  mutate(
    flag_missing_coordinates = is.na(facility_latitude) |
      is.na(facility_longitude),
    flag_state_level_fuzzy = match_type == "fuzzy_state_facility",
    flag_low_confidence_fuzzy = str_detect(match_type, "fuzzy") &
      match_score < 0.85,
    flag_jails_prisons_source = source == "jails_prisons",
    review_reasons = pmap_chr(
      list(
        flag_missing_coordinates,
        flag_state_level_fuzzy,
        flag_low_confidence_fuzzy,
        flag_jails_prisons_source
      ),
      \(missing_coords, state_fuzzy, low_fuzzy, jails_source) {
        reasons <- c(
          if (missing_coords) "missing_coordinates",
          if (state_fuzzy) "state_level_fuzzy",
          if (low_fuzzy) "low_confidence_fuzzy",
          if (jails_source) "jails_prisons_source"
        )

        paste(reasons, collapse = "; ")
      }
    )
  ) |>
  filter(review_reasons != "") |>
  arrange(desc(flag_missing_coordinates), match_score) |>
  select(
    review_reasons,
    state,
    county,
    agency,
    facility_guess,
    facility_name,
    source,
    match_type,
    match_score,
    facility_address,
    facility_city,
    facility_county,
    facility_latitude,
    facility_longitude
  )

readr::write_csv(
  facility_review,
  "data/facility_matches_needing_review.csv"
)

write_sf_parquet(
  facility_agreements_sf,
  "data/facility_agreements_sf.parquet"
)
