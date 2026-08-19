library(tidyverse)
library(sf)
library(tigris)
library(stringdist)

source("code/functions.R")

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)
YEAR <- 2024

agreements <- arrow::read_parquet("data/agreements.parquet")
facilities <- arrow::read_parquet("data/facilities.parquet")
jails_prisons <- arrow::read_parquet("data/jails-prisons.parquet")
manual_points <- arrow::read_parquet("data/manual-points.parquet")
manual_polygons <- arrow::read_parquet("data/manual-polygons.parquet")

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

facility_sources_exact <- bind_rows(facilities, jails_prisons)

# 287(g) facility-model agencies -----------------------------------------

doc_pattern <- paste(
  "department of corrections",
  "correctional services",
  "public safety & corrections",
  "division of corrections",
  "department of public safety",
  sep = "|"
)

# agreements manually re-routed to a polygon layer leave the facility
# matcher entirely
fac_287g <- agreements |>
  filter(geom_class == "facility_point") |>
  anti_join(manual_polygons, by = c("agency", "state", "county")) |>
  transmute(
    agreement_id,
    state,
    county,
    agency,
    agency_level,
    needs_review,
    moa,
    addendum
  ) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_ori_county(county),
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

# the dedup grain is (agreement, facility): a county agreement deliberately
# fans out to every facility fitting the county pattern, and slice_head only
# collapses duplicate source rows for the same facility
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
  group_by(agreement_id, facility_key) |>
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
  group_by(agreement_id, facility_key) |>
  arrange(source_rank) |>
  slice_head(n = 1) |>
  ungroup()

pattern_exact_matches <- bind_rows(
  county_pattern_exact_matches,
  municipal_pattern_exact_matches
)

# the jails census names each jail's operating agency, recovering jails whose
# own name carries no county tie ("Sheriff Al Cannon Detention Center" is
# operated by the Charleston County Sheriff's Office)
operator_exact_matches <- fac_287g |>
  filter(!is_doc_agency) |>
  mutate(agency_operator_key = norm_ori_agency(agency)) |>
  inner_join(
    facility_sources_exact |>
      filter(source == "jails_prisons", !is.na(facility_operator_name)) |>
      rename(facility_source_county_key = county_key) |>
      mutate(operator_key = norm_ori_agency(facility_operator_name)),
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  filter(
    operator_key == agency_operator_key,
    # regional jails list several counties in one field, so test membership
    # rather than equality
    is.na(county_key) |
      county_key == "" |
      is.na(facility_source_county_key) |
      str_detect(
        facility_source_county_key,
        paste0("\\b", county_key, "\\b")
      )
  ) |>
  anti_join(
    manual_facility_exclusions,
    by = c("state_key", "agency_key", "facility_key")
  ) |>
  mutate(
    match_type = "exact_operator_facility",
    match_score = 1.0
  ) |>
  group_by(agreement_id, facility_key) |>
  arrange(source_rank) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(-agency_operator_key, -operator_key)

# only agreements with no pattern or operator match enter this tier. The join
# consumes the source's facility_key via facility_guess_key, so these rows
# carry facility_key = NA and collapse into one group per agreement below
facility_name_exact_matches <- fac_287g |>
  filter(!is_doc_agency) |>
  anti_join(
    bind_rows(pattern_exact_matches, operator_exact_matches),
    by = "agreement_id"
  ) |>
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
  group_by(agreement_id) |>
  arrange(source_rank) |>
  slice_head(n = 1) |>
  ungroup()

facility_exact_matches <- bind_rows(
  pattern_exact_matches,
  operator_exact_matches,
  facility_name_exact_matches
)

facility_unmatched_after_exact <- fac_287g |>
  filter(!is_doc_agency) |>
  anti_join(facility_exact_matches, by = "agreement_id")

# use fuzzy string matching on facility names within same county ---------

facility_fuzzy_county <- facility_unmatched_after_exact |>
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
  group_by(agreement_id) |>
  arrange(match_dist, source_rank) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_type = "fuzzy_county_facility",
    match_score = 1 - match_dist,
    needs_review = TRUE
  )

facility_unmatched_after_fuzzy_county <- facility_unmatched_after_exact |>
  anti_join(facility_fuzzy_county, by = "agreement_id")

# broader state-level fuzzy matching for remaining unmatched facilities ----

# the statewide search compensates for its breadth with a stricter cutoff than
# the within-county tier (0.15 vs 0.22)
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
  group_by(agreement_id) |>
  arrange(match_dist, source_rank) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_type = "fuzzy_state_facility",
    match_score = 1 - match_dist,
    needs_review = TRUE
  )

# DOC: match to state-run prison facilities ------------------------------

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
    type_clean = str_to_upper(str_squish(type)),
    operator_clean = str_to_lower(str_squish(facility_operator_name)),
    doc_manual_include = coalesce(doc_manual_include, FALSE),
    doc_is_state_prison_source = source == "hifld_prisons" &
      type_clean == "STATE",
    doc_is_local_prison_source = source == "hifld_prisons" &
      type_clean %in% c("COUNTY", "LOCAL"),
    doc_is_federal_prison_source = source == "hifld_prisons" &
      type_clean == "FEDERAL",
    doc_is_uncertain_prison_source = source == "hifld_prisons" &
      type_clean %in% c("MULTI", "NOT AVAILABLE"),
    doc_is_jails_source = source == "jails_prisons",
    doc_has_doc_operator = str_detect(operator_clean, doc_pattern),
    doc_has_local_jail_name = str_detect(
      facility_name_clean,
      doc_local_jail_pattern
    ) |
      is_exact_county_pattern(facility_name, facility_county),
    # first match wins: a manual inclusion beats source typing, and the
    # DOC-operator clause claims its jails-source subset before the local-jail
    # exclusion sweeps the rest
    doc_match_tier = case_when(
      doc_manual_include ~
        "doc_manual_state_facility",
      doc_is_state_prison_source ~
        "doc_exact_state_prison_source",
      doc_is_uncertain_prison_source ~
        "doc_needs_research",
      doc_is_jails_source & doc_has_doc_operator ~
        "doc_needs_research",
      doc_is_local_prison_source | doc_is_federal_prison_source |
        doc_is_jails_source | doc_has_local_jail_name ~
        "doc_excluded_local_jail",
      TRUE ~ "doc_not_correctional_candidate"
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
  group_by(agreement_id, facility_key) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    county_key = facility_source_county_key,
    match_type = doc_match_tier,
    match_score = 1,
    needs_review = FALSE
  )

# manual matches ---------------------------------------------------------

manual_points_specific <- manual_points |>
  filter(!is.na(county), county != "")

manual_points_general <- manual_points |>
  filter(is.na(county) | county == "")

manual_matches_specific <- fac_287g |>
  inner_join(
    manual_points_specific,
    by = c("agency", "state", "county")
  )

manual_matches_general <- fac_287g |>
  inner_join(
    manual_points_general |> select(-county),
    by = c("agency", "state")
  )

# county-specific rows are bound first, so distinct() keeps the specific
# placement when an agreement matches both
manual_matches <- bind_rows(
  manual_matches_specific,
  manual_matches_general
) |>
  distinct(agreement_id, .keep_all = TRUE) |>
  transmute(
    agreement_id,
    state,
    county,
    agency,
    agency_level,
    needs_review = TRUE,
    moa,
    addendum,
    state_key,
    county_key,
    agency_key,
    facility_guess,
    facility_guess_key,
    is_doc_agency,
    source = "manual",
    source_rank = 0L,
    facility_name,
    facility_state = state,
    latitude,
    longitude,
    facility_key = norm_key(facility_name),
    match_type = "manual",
    match_score = 1
  )

# no per-facility dedup here, unlike the automated tiers: every source row
# carrying the confirmed facility key survives, and overrides can hit DOC
# agencies too
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

non_doc_matches <- bind_rows(
  facility_exact_matches,
  facility_fuzzy_county,
  facility_fuzzy_state
) |>
  group_by(agreement_id, facility_key) |>
  arrange(source_rank, desc(match_score)) |>
  slice_head(n = 1) |>
  ungroup()

auto_matches <- bind_rows(non_doc_matches, doc_matches)

# manual placements fully preempt automated matches for an agreement
facility_all_matches <- bind_rows(
  manual_matches,
  manual_facility_matches,
  auto_matches |>
    anti_join(
      bind_rows(manual_matches, manual_facility_matches),
      by = "agreement_id"
    )
)

# HIFLD law-enforcement fallback -----------------------------------------

# these are police stations, not a jail census (every row is NAICS "police
# protection"), but a city or rural county jail usually operates out of the
# agency building. A last resort for agreements every source above leaves
# unmatched, never competing with the jail-census sources.
hifld_law_enforcement <- arrow::read_parquet(
  "data/hifld-law-enforcement.parquet"
) |>
  transmute(
    source = "hifld_law_enforcement",
    source_rank = 4L,
    facility_name = str_squish(name),
    facility_address = address,
    facility_city = str_to_title(city),
    facility_county = county,
    county_fips,
    facility_state = state,
    state_fips = str_sub(county_fips, 1, 2),
    facility_zip = zip,
    latitude,
    longitude,
    state_key,
    facility_source_county_key = county_key,
    facility_key = norm_key(facility_name)
  )

hifld_fallback_matches <- fac_287g |>
  filter(!is_doc_agency) |>
  anti_join(facility_all_matches, by = "agreement_id") |>
  inner_join(
    hifld_law_enforcement,
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  filter(
    # the sheet's county must agree with the station's when both are known
    is.na(county_key) |
      county_key == "" |
      is.na(facility_source_county_key) |
      county_key == facility_source_county_key,
    # a county-level agreement names its county on the sheet, so the station
    # need only fit the county pattern even when the signing agency does not
    # (Pasco's jail is run by the Board of County Commissioners)
    ((is_county_exact_agency | agency_level == "county") &
      is_exact_county_pattern(facility_name, county)) |
      (is_municipal_exact_agency &
        is_exact_municipal_pattern(facility_name, city_guess)) |
      facility_guess_key == facility_key
  ) |>
  anti_join(
    manual_facility_exclusions,
    by = c("state_key", "agency_key", "facility_key")
  ) |>
  # prefer a station named for the jail itself, then the shortest name, which
  # is the main office rather than a substation ("... - DISTRICT 1")
  group_by(agreement_id) |>
  arrange(
    desc(str_detect(
      str_to_lower(facility_name),
      "jail|detention|correction"
    )),
    str_length(facility_name),
    facility_name,
    .by_group = TRUE
  ) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    county_key = coalesce(na_if(county_key, ""), facility_source_county_key),
    match_type = "hifld_law_enforcement_location",
    match_score = 1,
    needs_review = TRUE
  ) |>
  select(-facility_source_county_key)

facility_all_matches <- bind_rows(
  facility_all_matches,
  hifld_fallback_matches
)

# facility point sf layer ------------------------------------------------

# a match without coordinates cannot be placed on the map, so it re-enters
# below as unmatched
facility_matched_sf <- facility_all_matches |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# every agreement keeps a row; unmatched ones carry an empty geometry
facility_unmatched <- fac_287g |>
  anti_join(
    facility_matched_sf |> st_drop_geometry(),
    by = "agreement_id"
  ) |>
  mutate(
    source = NA_character_,
    match_type = "unmatched_facility",
    match_score = NA_real_,
    needs_review = TRUE
  )

facility_unmatched_sf <- st_sf(
  facility_unmatched,
  geometry = st_sfc(
    rep(list(st_point()), nrow(facility_unmatched)),
    crs = st_crs(4326)
  )
)

facility_sf <- bind_rows(facility_matched_sf, facility_unmatched_sf)

# a geocoded facility whose source table carries no FIPS still sits in exactly
# one county, so fill from the containing polygon
county_containing <- facility_sf |>
  filter(
    !st_is_empty(geometry),
    is.na(state_fips) | is.na(county_fips)
  ) |>
  select(agreement_id, facility_name) |>
  st_join(
    counties(cb = TRUE, year = YEAR, class = "sf") |>
      st_transform(4326) |>
      transmute(
        containing_state_fips = STATEFP,
        containing_county_fips = GEOID,
        geometry
      )
  ) |>
  st_drop_geometry() |>
  # a point on a county boundary can hit two polygons under planar containment
  distinct(agreement_id, facility_name, .keep_all = TRUE)

facility_sf <- facility_sf |>
  left_join(county_containing, by = c("agreement_id", "facility_name")) |>
  mutate(
    state_fips = coalesce(state_fips, containing_state_fips),
    county_fips = coalesce(county_fips, containing_county_fips)
  ) |>
  select(-containing_state_fips, -containing_county_fips) |>
  mutate(
    # plain "manual" point placements are absent from this list so they always
    # stay under review; manual_facility_match confirmations are accepted
    is_accepted_exact_match = match_type %in% c(
      "exact_state_county_facility_name",
      "exact_county_pattern_all_facilities",
      "exact_municipal_pattern_facility",
      "exact_operator_facility",
      "manual_facility_match",
      "doc_exact_state_prison_source",
      "doc_manual_state_facility"
    ),
    # "pending" is the sheet's placeholder for an unposted MOA link; those
    # agreements and any carrying an addendum stay under review
    needs_review = needs_review |
      !is_accepted_exact_match |
      !is.na(addendum) |
      moa == "pending"
  ) |>
  select(
    agreement_id,
    match_name = facility_name,
    detention_facility_code,
    source,
    source_id,
    match_type,
    match_score,
    facility_address,
    facility_city,
    facility_state,
    facility_zip,
    facility_operator_name,
    latitude,
    longitude,
    state_fips,
    county_fips,
    needs_review,
    geometry
  )

write_sf_parquet(facility_sf, "data/facility-sf.parquet")
