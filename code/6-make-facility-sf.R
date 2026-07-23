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

manual_facility_review <- readr::read_csv(
  "inputs/manual-facility-review.csv",
  show_col_types = FALSE
) |>
  mutate(
    state_key = norm_state(state),
    agency_key = norm_key(agency),
    facility_key = norm_key(facility_name)
  )

manual_facility_inclusions <- manual_facility_review |>
  filter(
    review_type == "doc_facility",
    decision == "include_state_doc_facility"
  ) |>
  distinct(state_key, agency_key, facility_key)

manual_facility_match_overrides <- manual_facility_review |>
  filter(
    review_type == "facility_match",
    decision == "include_facility_match"
  ) |>
  transmute(
    state,
    county,
    agency,
    state_key,
    agency_key,
    manual_facility_key = facility_key
  ) |>
  distinct()

manual_facility_exclusions <- manual_facility_review |>
  filter(
    review_type == "facility_match",
    decision == "exclude_facility_match"
  ) |>
  distinct(state_key, agency_key, facility_key)

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
    moa_pending,
    signed
  ) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_place(county),
    agency_key = norm_key(agency),
    facility_guess = extract_facility_guess(agency),
    facility_guess_key = norm_key(facility_guess),
    city_guess = extract_city_guess(agency),
    is_county_exact_agency = is_exact_county_pattern(agency, county),
    is_municipal_exact_agency = agency_level == "municipal" &
      is_exact_municipal_pattern(agency, city_guess),
    is_doc_agency = str_detect(str_to_lower(agency), doc_pattern) &
      agency_level == "state"
  )

# match detention facilities to facility datasets ------------------------

county_pattern_exact_matches <- fac_287g |>
  filter(!is_doc_agency, is_county_exact_agency) |>
  inner_join(
    facility_sources_exact,
    by = c("state_key", "county_key"),
    relationship = "many-to-many"
  ) |>
  filter(is_exact_county_pattern(facility_name, county)) |>
  anti_join(
    manual_facility_exclusions,
    by = c("state_key", "agency_key", "facility_key")
  ) |>
  mutate(
    match_type = "exact_county_pattern_all_facilities",
    match_score = 1.0
  ) |>
  group_by(state, county, agency, support_clean, facility_key) |>
  arrange(source_rank) |>
  slice_head(n = 1) |>
  ungroup()

municipal_pattern_exact_matches <- fac_287g |>
  filter(!is_doc_agency, is_municipal_exact_agency) |>
  inner_join(
    facility_sources_exact |>
      rename(facility_source_county_key = county_key),
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  filter(
    is.na(county_key) | county_key == "" |
      county_key == facility_source_county_key,
    is_exact_municipal_pattern(facility_name, city_guess)
  ) |>
  anti_join(
    manual_facility_exclusions,
    by = c("state_key", "agency_key", "facility_key")
  ) |>
  mutate(
    county_key = facility_source_county_key,
    match_type = "exact_municipal_pattern_facility",
    match_score = 1.0
  ) |>
  group_by(state, county, agency, support_clean, facility_key) |>
  arrange(source_rank) |>
  slice_head(n = 1) |>
  ungroup()

pattern_exact_matches <- bind_rows(
  county_pattern_exact_matches,
  municipal_pattern_exact_matches
)

facility_name_exact_matches <- fac_287g |>
  filter(!is_doc_agency) |>
  anti_join(pattern_exact_matches, by = c("state", "county", "agency")) |>
  inner_join(
    facility_sources_exact,
    by = c("state_key", "county_key", "facility_guess_key" = "facility_key"),
    relationship = "many-to-many"
  ) |>
  anti_join(
    manual_facility_exclusions,
    by = c(
      "state_key",
      "agency_key",
      "facility_guess_key" = "facility_key"
    )
  ) |>
  mutate(
    match_type = "exact_state_county_facility_name",
    match_score = 1.0
  ) |>
  group_by(state, county, agency, support_clean) |>
  arrange(source_rank) |>
  slice_head(n = 1) |>
  ungroup()

facility_exact_matches <- bind_rows(
  pattern_exact_matches,
  facility_name_exact_matches
)

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
  anti_join(
    manual_facility_exclusions,
    by = c("state_key", "agency_key", "facility_key")
  ) |>
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
  anti_join(
    manual_facility_exclusions,
    by = c("state_key", "agency_key", "facility_key")
  ) |>
  group_by(state, county, agency) |>
  arrange(match_dist, source_rank) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_type = "fuzzy_state_facility",
    match_score = 1 - match_dist,
    needs_review = TRUE
  )

# DOC: match to state-run prison facilities ------------------------------

doc_prison_like_pattern <- paste(
  "state prison",
  "prison",
  "penitentiary",
  "correctional institution",
  "correctional institute",
  "correctional facility",
  "correctional center",
  "correctional complex",
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

doc_ambiguous_pattern <- paste(
  "detention",
  "juvenile",
  "rehab",
  "rehabilitation",
  "treatment",
  "public safety complex",
  "justice center",
  "law enforcement center",
  sep = "|"
)

doc_local_jail_pattern <- paste(
  "county jail",
  "parish jail",
  "city jail",
  "municipal jail",
  "county detention",
  "parish detention",
  "city detention",
  "municipal detention",
  "\\bcounty\\b.*\\b(jail|detention|correctional|sheriff|law enforcement|justice|public safety)",
  "\\bparish\\b.*\\b(jail|detention|correctional|sheriff|law enforcement|justice|public safety)",
  "\\bcity\\b.*\\b(jail|detention|correctional|law enforcement|justice|public safety)",
  "\\bmunicipal\\b.*\\b(jail|detention|correctional|law enforcement|justice|public safety)",
  "sheriffs?.*\\b(jail|detention)",
  "police.*\\b(jail|detention)",
  "courthouse",
  "regional lock-?up",
  "law enforcement center",
  "justice center",
  "public safety complex",
  sep = "|"
)

doc_candidates <- fac_287g |>
  filter(is_doc_agency) |>
  inner_join(
    facility_sources_exact |>
      rename(facility_source_county_key = county_key),
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  left_join(
    manual_facility_inclusions |>
      mutate(doc_manual_include = TRUE),
    by = c("state_key", "agency_key", "facility_key")
  ) |>
  mutate(
    facility_name_clean = str_to_lower(facility_name),
    facility_source_type_clean = str_to_upper(str_squish(facility_source_type)),
    facility_operator_clean = str_to_lower(str_squish(facility_operator_name)),
    doc_manual_include = coalesce(doc_manual_include, FALSE),
    doc_is_state_prison_source = source == "hifld_prisons" &
      facility_source_type_clean == "STATE",
    doc_is_local_prison_source = source == "hifld_prisons" &
      facility_source_type_clean %in% c("COUNTY", "LOCAL"),
    doc_is_federal_prison_source = source == "hifld_prisons" &
      facility_source_type_clean == "FEDERAL",
    doc_is_uncertain_prison_source = source == "hifld_prisons" &
      facility_source_type_clean %in% c("MULTI", "NOT AVAILABLE"),
    doc_is_jails_source = source == "jails_prisons",
    doc_has_doc_operator = str_detect(
      facility_operator_clean,
      doc_pattern
    ),
    doc_has_prison_like_name = str_detect(
      facility_name_clean,
      doc_prison_like_pattern
    ),
    doc_has_ambiguous_name = str_detect(
      facility_name_clean,
      doc_ambiguous_pattern
    ),
    doc_has_local_jail_name = str_detect(
      facility_name_clean,
      doc_local_jail_pattern
    ) |
      is_exact_county_pattern(facility_name, facility_county),
    doc_is_correctional_candidate = source %in% c(
      "hifld_prisons",
      "jails_prisons"
    ) |
      doc_has_prison_like_name |
      doc_has_ambiguous_name |
      doc_has_local_jail_name,
    doc_match_tier = case_when(
      doc_manual_include ~
        "doc_manual_state_facility",
      doc_is_state_prison_source ~
        "doc_exact_state_prison_source",
      doc_is_uncertain_prison_source ~
        "doc_needs_research",
      source == "hifld" & (doc_has_prison_like_name | doc_has_ambiguous_name) ~
        "doc_needs_research",
      doc_is_jails_source & doc_has_doc_operator ~
        "doc_needs_research",
      doc_is_local_prison_source | doc_is_federal_prison_source |
        doc_is_jails_source | doc_has_local_jail_name ~
        "doc_excluded_local_jail",
      TRUE ~ "doc_not_correctional_candidate"
    ),
    doc_research_reason = case_when(
      doc_match_tier == "doc_manual_state_facility" ~
        "manual review confirmed state DOC facility",
      doc_match_tier == "doc_exact_state_prison_source" ~
        "hifld_prisons type is STATE",
      doc_match_tier == "doc_excluded_local_jail" ~
        "source type indicates local/federal jail or non-DOC facility",
      doc_match_tier == "doc_needs_research" & doc_is_uncertain_prison_source ~
        "hifld_prisons type is MULTI or NOT AVAILABLE",
      doc_match_tier == "doc_needs_research" & source == "hifld" ~
        "prison-like or ambiguous name from law-enforcement source",
      doc_match_tier == "doc_needs_research" & doc_is_jails_source ~
        "ICPSR jails source has DOC-like operator name",
      doc_match_tier == "doc_not_correctional_candidate" ~
        "source type/name do not support DOC prison match",
      TRUE ~ NA_character_
    )
  )

doc_matches <- doc_candidates |>
  filter(
    doc_match_tier %in% c(
      "doc_exact_state_prison_source",
      "doc_manual_state_facility"
    )
  ) |>
  arrange(state, county, agency, source_rank) |>
  group_by(state, county, agency, facility_key) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    county_key = facility_source_county_key,
    match_type = doc_match_tier,
    match_score = 1,
    needs_review = FALSE
  )

doc_research <- doc_candidates |>
  filter(doc_match_tier == "doc_needs_research") |>
  arrange(state, agency, facility_name, source_rank) |>
  group_by(state, agency, facility_key) |>
  slice_head(n = 1) |>
  ungroup()

doc_excluded_local <- doc_candidates |>
  filter(doc_match_tier == "doc_excluded_local_jail") |>
  arrange(state, agency, facility_name, source_rank) |>
  group_by(state, agency, facility_key) |>
  slice_head(n = 1) |>
  ungroup()

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
    signed,
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

manual_facility_matches <- fac_287g |>
  inner_join(
    manual_facility_match_overrides,
    by = c("state", "county", "agency", "state_key", "agency_key")
  ) |>
  inner_join(
    facility_sources_exact |>
      select(-county_key) |>
      rename(manual_facility_key = facility_key),
    by = c("state_key", "manual_facility_key"),
    relationship = "many-to-many"
  ) |>
  mutate(
    facility_key = manual_facility_key,
    match_type = "manual_facility_match",
    match_score = 1,
    needs_review = FALSE
  ) |>
  select(-manual_facility_key)

# combine all matches ----------------------------------------------------

non_doc_matches <-
  bind_rows(
    facility_exact_matches,
    facility_fuzzy_county,
    facility_fuzzy_state
  ) |>
  group_by(state, county, agency, facility_key) |>
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
    manual_facility_matches,
    auto_matches |>
      anti_join(
        bind_rows(
          manual_matches,
          manual_facility_matches
        ),
        by = c("state", "county", "agency")
      )
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
    is_accepted_exact_match = match_type %in% c(
      "exact_state_county_facility_name",
      "exact_county_pattern_all_facilities",
      "exact_municipal_pattern_facility",
      "manual_facility_match",
      "doc_exact_state_prison_source",
      "doc_manual_state_facility"
    ),
    needs_review = needs_review |
      (source != "facilities" & !is_accepted_exact_match) |
      !is_accepted_exact_match |
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
    signed,
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
    facility_source_type,
    facility_status,
    facility_operator_name,
    facility_source_state_fips,
    facility_source_county_fips,
    facility_source_county,
    facility_population,
    facility_hold_72,
    facility_is_regional,
    facility_is_private,
    facility_hold_lt_1yr,
    facility_hold_1yr_plus,
    facility_hold_lt_72,
    facility_function_adult,
    facility_function_work_release,
    facility_function_reception,
    facility_function_juvenile,
    facility_function_medical,
    facility_function_mental,
    facility_function_alcohol,
    facility_function_drug,
    facility_secure_level,
    facility_capacity,
    facility_naics_code,
    facility_naics_desc,
    facility_source_url,
    facility_source_date,
    facility_website,
    facility_ci_id,
    facility_csllea08id,
    facility_subtype1,
    facility_subtype2,
    facility_tribal,
    state_fips,
    county_fips,
    place_fips,
    geoid,
    facility_latitude,
    facility_longitude,
    geometry
  )

write_sf_parquet(
  facility_agreements_sf,
  "data/facility_agreements_sf.parquet"
)
