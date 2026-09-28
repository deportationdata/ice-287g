# Matches facility-model agreements to jail/prison points -> data/intermediate/match-facility.parquet

library(tidyverse)
library(sf)
library(tigris)
library(stringdist)

source("code/functions.R")

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)
YEAR <- 2024

agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")
facilities <- arrow::read_parquet("data/intermediate/facility-list-ice-detention.parquet")
jails_prisons <- arrow::read_parquet("data/intermediate/facility-list-jails-prisons.parquet")
censuses <- arrow::read_parquet("data/intermediate/facility-list-censuses.parquet")
manual_points <- arrow::read_parquet("data/intermediate/manual-facility-points.parquet")
manual_doc_facilities <- arrow::read_parquet("data/intermediate/manual-doc-facilities.parquet")
manual_polygons <- arrow::read_parquet("data/intermediate/manual-non-facility-polygons.parquet")

manual_facility_review <- read_csv(
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

# a facility a state DOC no longer runs
manual_doc_exclusions <- manual_facility_review |>
  filter(
    review_type == "doc_facility",
    decision == "exclude_state_doc_facility"
  ) |>
  distinct(state_key, agency_key, facility_key)

# only active agreements are matched, so a facility HIFLD marks Closed is never a candidate; it stays a location
# reference for placing a DOC's listed facilities and for dating the site around it
facility_sources_exact <- bind_rows(facilities, jails_prisons |> filter(coalesce(facility_status, "") != "Closed")) |>
  # a Georgia county prison ("Hall County Correctional Institution", "Harris County Prison") holds state inmates for the
  # county government, not the sheriff: the jails census names someone other than the sheriff as the operator of every one
  filter(!(
    facility_state %in% "Georgia" &
      str_detect(facility_name, regex("\\bcounty (correctional institut(e|ion)|prison)\\b", ignore_case = TRUE)) &
      !str_detect(facility_name, regex("jail|sheriff", ignore_case = TRUE))
  ))
# every pick ends in the same tiebreak, so no result depends on row order
tiebreak <- c("facility_name", "latitude", "longitude")

doc_pattern <- paste(
  "department of corrections?",
  "correctional services",
  "public safety & corrections",
  "division of corrections",
  "department of public safety",
  sep = "|"
)

fac_287g <- agreements |>
  filter(geometry_type == "Point") |>
  anti_join(manual_polygons, by = c("agency", "state", "county")) |>
  transmute(
    agreement_id,
    state,
    county,
    agency,
    jurisdiction_level
  ) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_ori_county(county),
    # a county sheriff or jail names its own county; a statewide name search may not cross it
    county_key = coalesce(
      na_if(county_key, ""),
      if_else(
        str_detect(agency, regex("sheriff|jail|detention|correction", ignore_case = TRUE)),
        norm_ori_county(str_match(agency, regex("^(.+?) (County|Parish)\\b", ignore_case = TRUE))[, 2]),
        NA_character_
      )
    ),
    agency_key = norm_key(agency),
    facility_guess = extract_facility_guess(agency),
    facility_guess_key = norm_key(facility_guess),
    city_guess = extract_city_guess(agency),
    is_county_exact_agency = is_exact_county_pattern(agency, county),
    is_municipal_exact_agency = jurisdiction_level == "Municipal" &
      is_exact_municipal_pattern(agency, city_guess),
    # a DOC named for its state is a state agency whatever TYPE says; a county or city DOC never is
    is_doc_agency = str_detect(str_to_lower(agency), doc_pattern) &
      (jurisdiction_level == "State" |
         str_detect(str_to_lower(agency), paste0("^", str_to_lower(state), "\\b")) |
         str_detect(agency, "^[A-Z]{2} ")) &
      !str_detect(str_to_lower(agency), "\\b(county|parish|city|town|village|borough)\\b")
  )
# only an active agreement is matched to facilities, and only to ones open now; every other jail-model agreement
# keeps its row with no point
active_ids <- agreements$agreement_id[agreements$status == "Active"]
fac_all <- fac_287g
fac_287g <- fac_287g |> filter(agreement_id %in% active_ids)

# exact match tiers

# each tier keeps one candidate per source for a facility name; the choice between sources waits until each
# candidate's site has been checked open, so an old building under a stale HIFLD record gives way to the census's
# a county agreement fans out to all its facilities; HIFLD STATE and FEDERAL prisons named
# for the county are not its jail, so they stay out
county_pattern_exact_matches <- fac_287g |>
  filter(!is_doc_agency, is_county_exact_agency) |>
  inner_join(
    facility_sources_exact,
    by = c("state_key", "county_key"),
    relationship = "many-to-many"
  ) |>
  filter(
    is_exact_county_pattern(facility_name, county),
    !(source == "hifld_prisons" & str_to_upper(str_squish(coalesce(type, ""))) %in% c("STATE", "FEDERAL"))
  ) |>
  anti_join(
    manual_facility_exclusions,
    by = c("state_key", "agency_key", "facility_key")
  ) |>
  mutate(
    match_type = "exact_county_pattern_all_facilities",
    match_score = 1.0
  ) |>
  slice_best(source_rank, by = c("agreement_id", "facility_key", "source"), tiebreak = tiebreak)

municipal_pattern_exact_matches <- fac_287g |>
  filter(!is_doc_agency, is_municipal_exact_agency) |>
  inner_join(
    facility_sources_exact |>
      rename(facility_source_county_key = county_key),
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  filter(
    # a facility with no county of its own is admitted, not dropped by an NA comparison
    is.na(county_key) |
      county_key == "" |
      is.na(facility_source_county_key) |
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
  slice_best(source_rank, by = c("agreement_id", "facility_key", "source"), tiebreak = tiebreak)

pattern_exact_matches <- bind_rows(
  county_pattern_exact_matches,
  municipal_pattern_exact_matches
)

# the jails census names each jail's operating agency, catching names with no county tie
operator_exact_matches <- fac_287g |>
  filter(!is_doc_agency) |>
  mutate(agency_operator_key = norm_operator_key(agency)) |>
  inner_join(
    facility_sources_exact |>
      filter(source == "jails_prisons", !is.na(facility_operator_name)) |>
      rename(facility_source_county_key = county_key) |>
      mutate(operator_key = norm_operator_key(facility_operator_name)),
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  filter(
    operator_key == agency_operator_key,
    # regional jails list several counties in one field, so test membership, not equality
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
  slice_best(source_rank, by = c("agreement_id", "facility_key"), tiebreak = tiebreak) |>
  select(-agency_operator_key, -operator_key)

# the join consumes facility_key via facility_guess_key, so these rows group by agreement alone
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
  slice_best(source_rank, by = c("agreement_id", "source"), tiebreak = tiebreak)

facility_exact_matches <- bind_rows(
  pattern_exact_matches,
  operator_exact_matches,
  facility_name_exact_matches
)

facility_unmatched_after_exact <- fac_287g |>
  filter(!is_doc_agency) |>
  anti_join(facility_exact_matches, by = "agreement_id")

# fuzzy match tiers

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
  slice_best(match_dist, source_rank, by = "agreement_id", tiebreak = tiebreak) |>
  mutate(
    match_type = "fuzzy_county_facility",
    match_score = 1 - match_dist,
    fuzzy_match = TRUE
  )

facility_unmatched_after_fuzzy_county <- facility_unmatched_after_exact |>
  anti_join(facility_fuzzy_county, by = "agreement_id")

# statewide search needs a stricter cutoff than the within-county tier (0.15 vs 0.22)
facility_fuzzy_state <- facility_unmatched_after_fuzzy_county |>
  inner_join(
    facility_sources_exact |>
      rename(facility_source_county_key = county_key),
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  # same county guard as the exact tiers: a statewide search may not cross the sheet's county
  filter(
    is.na(county_key) |
      county_key == "" |
      is.na(facility_source_county_key) |
      county_key == facility_source_county_key
  ) |>
  mutate(
    county_key = facility_source_county_key,
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
  slice_best(match_dist, source_rank, by = "agreement_id", tiebreak = tiebreak) |>
  mutate(
    match_type = "fuzzy_state_facility",
    match_score = 1 - match_dist,
    fuzzy_match = TRUE
  )

# state DOC agreements: match to state-run prisons only

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
  left_join(
    manual_doc_exclusions |>
      mutate(doc_manual_exclude = TRUE),
    by = c("state_key", "agency_key", "facility_key")
  ) |>
  mutate(
    facility_name_clean = str_to_lower(facility_name),
    type_clean = str_to_upper(str_squish(type)),
    operator_clean = str_to_lower(str_squish(facility_operator_name)),
    doc_manual_include = coalesce(doc_manual_include, FALSE),
    doc_manual_exclude = coalesce(doc_manual_exclude, FALSE),
    # ICE's detention list can name a DOC itself as a facility
    doc_is_named_facility = source == "facilities" & facility_key == agency_key,
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
    # clause order matters: manual verdicts, then source typing, then the local-jail sweep
    doc_match_tier = case_when(
      doc_manual_exclude ~
        "doc_excluded_manual",
      doc_manual_include ~
        "doc_manual_state_facility",
      doc_is_named_facility ~
        "doc_named_facility",
      # a youth agency's facility is not the DOC's; a DOC that runs one says so in its own list
      str_detect(facility_name_clean, "\\b(youth|juvenile)\\b") ~
        "doc_excluded_youth_facility",
      doc_is_state_prison_source ~
        "doc_exact_state_prison_source",
      doc_is_uncertain_prison_source ~
        "doc_needs_research",
      doc_is_jails_source & doc_has_doc_operator ~
        "doc_needs_research",
      doc_is_local_prison_source |
        doc_is_federal_prison_source |
        doc_is_jails_source |
        doc_has_local_jail_name ~
        "doc_excluded_local_jail",
      TRUE ~ "doc_not_correctional_candidate"
    )
  )

doc_matches <- doc_candidates |>
  filter(
    doc_match_tier %in%
      c(
        "doc_exact_state_prison_source",
        "doc_manual_state_facility",
        "doc_named_facility"
      )
  ) |>
  slice_best(source_rank, by = c("agreement_id", "facility_key"), tiebreak = tiebreak) |>
  mutate(
    county_key = facility_source_county_key,
    match_type = doc_match_tier,
    match_score = 1
  )

# a DOC's own facility list decides the facilities of every agreement it has running on the day
# the list was read; its older agreements keep the matches above
doc_list_matches <- fac_287g |>
  filter(is_doc_agency) |>
  inner_join(agreements |> select(agreement_id, signed, status, removed_by), by = "agreement_id") |>
  inner_join(
    manual_doc_facilities |> select(-note),
    by = c("state", "agency"),
    relationship = "many-to-many"
  ) |>
  filter(signed <= listed_on, status == "Active" | coalesce(removed_by > listed_on, FALSE), coalesce(program_site, TRUE)) |>
  mutate(facility_key = norm_key(facility_name)) |>
  # an address the geocoder cannot place takes a facility source's point of the same name
  left_join(
    facility_sources_exact |>
      filter(!is.na(latitude), !is.na(longitude)) |>
      arrange(source_rank) |>
      distinct(state_key, facility_key, .keep_all = TRUE) |>
      select(state_key, facility_key, source_latitude = latitude, source_longitude = longitude),
    by = c("state_key", "facility_key")
  ) |>
  mutate(
    latitude = coalesce(latitude, source_latitude),
    longitude = coalesce(longitude, source_longitude),
    facility_state = state,
    source = "manual_doc_facility_list",
    source_rank = 0L,
    source_id = source_url,
    match_type = "manual_doc_facility_list",
    match_score = 1
  ) |>
  select(-signed, -status, -removed_by, -listed_on, -source_latitude, -source_longitude, -program_site, -program_site_source)

# a listed facility takes the centre of HIFLD's prison grounds within 1.5 km over its street address when both give
# the same house number: a neighbouring facility the DOC lists at its own address keeps that address
hifld_state_prisons <- jails_prisons |>
  filter(source == "hifld_prisons", str_to_upper(coalesce(type, "")) %in% c("STATE", "FEDERAL"), !is.na(latitude), !is.na(longitude))
house_number <- \(a) str_to_lower(str_squish(coalesce(a, ""))) |>
  str_remove("^#\\s*") |>
  str_replace("^one\\b", "1") |>
  str_replace("^two\\b", "2") |>
  str_extract("^\\d+")
doc_list_sf <- doc_list_matches |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  distinct(state_key, facility_name, facility_address, latitude, longitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  st_transform(5070)
hifld_state_prisons_sf <- st_as_sf(hifld_state_prisons, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(5070)
nearest_prison <- st_nearest_feature(doc_list_sf, hifld_state_prisons_sf)
nearest_snaps <- doc_list_sf |>
  st_drop_geometry() |>
  mutate(
    snap_m = as.numeric(st_distance(doc_list_sf, hifld_state_prisons_sf[nearest_prison, ], by_element = TRUE)),
    snap_id = hifld_state_prisons$source_id[nearest_prison],
    snap_address = hifld_state_prisons$facility_address[nearest_prison],
    snap_latitude = hifld_state_prisons$latitude[nearest_prison],
    snap_longitude = hifld_state_prisons$longitude[nearest_prison]
  ) |>
  filter(snap_m <= 1500, coalesce(house_number(facility_address) == house_number(snap_address), FALSE)) |>
  select(state_key, facility_name, snap_id, snap_latitude, snap_longitude)

# a rural address can geocode kilometres down the road, so HIFLD's prison of the same name wins
# within 10 km, and places a facility whose address could not be geocoded at all
name_snaps <- doc_list_matches |>
  distinct(state_key, facility_name, facility_key, latitude, longitude) |>
  inner_join(
    hifld_state_prisons |>
      filter(!duplicated(paste(state_key, facility_key)) & !duplicated(paste(state_key, facility_key), fromLast = TRUE)) |>
      select(state_key, facility_key, snap_id = source_id, snap_latitude = latitude, snap_longitude = longitude),
    by = c("state_key", "facility_key")
  ) |>
  filter(
    is.na(latitude) |
      as.numeric(st_distance(
        st_as_sf(tibble(x = coalesce(longitude, 0), y = coalesce(latitude, 0)), coords = c("x", "y"), crs = 4326) |> st_transform(5070),
        st_as_sf(tibble(x = snap_longitude, y = snap_latitude), coords = c("x", "y"), crs = 4326) |> st_transform(5070),
        by_element = TRUE
      )) <= 10000
  ) |>
  select(state_key, facility_name, snap_id, snap_latitude, snap_longitude)

# a facility pinned to a HIFLD record takes it whatever its name or address says
pinned_snaps <- doc_list_matches |>
  filter(!is.na(hifld_id)) |>
  distinct(state_key, facility_name, hifld_id) |>
  inner_join(
    jails_prisons |> filter(source == "hifld_prisons") |> select(hifld_id = source_id, snap_latitude = latitude, snap_longitude = longitude),
    by = "hifld_id"
  ) |>
  transmute(state_key, facility_name, snap_id = hifld_id, snap_latitude, snap_longitude)
stopifnot("every hifld_id in inputs/manual-doc-facilities.csv names a HIFLD prison" =
  nrow(pinned_snaps) == nrow(distinct(filter(doc_list_matches, !is.na(hifld_id)), state_key, facility_name)))

doc_list_snaps <- bind_rows(
  pinned_snaps,
  name_snaps |> anti_join(pinned_snaps, by = c("state_key", "facility_name")),
  nearest_snaps |> anti_join(bind_rows(pinned_snaps, name_snaps), by = c("state_key", "facility_name"))
)

hifld_closed_ids <- jails_prisons$source_id[jails_prisons$source == "hifld_prisons" & jails_prisons$facility_status %in% "Closed"]
doc_list_matches <- doc_list_matches |>
  left_join(doc_list_snaps, by = c("state_key", "facility_name")) |>
  mutate(
    latitude = coalesce(snap_latitude, latitude),
    longitude = coalesce(snap_longitude, longitude),
    # the list is the source; the HIFLD record it sits on is the id, unless HIFLD marks that record Closed: a closed
    # record only places the site (McRae Women's Facility sits in a closed federal prison's building)
    source_id = if_else(snap_id %in% hifld_closed_ids, source_url, coalesce(snap_id, source_url))
  ) |>
  select(-snap_id, -snap_latitude, -snap_longitude, -source_url, -hifld_id)

doc_matches <- bind_rows(
  doc_matches |> anti_join(doc_list_matches, by = "agreement_id"),
  doc_list_matches
)

# manual matches

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

# county-specific rows bind first, so distinct() keeps them over the general placement
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
    jurisdiction_level,
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
    match_score = 1,
    manual_reason = reason,
    manual_note = note
  )

# no per-facility dedup here: every source row with the confirmed facility key survives
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
    match_score = 1
  ) |>
  select(-manual_facility_key)

non_doc_matches <- bind_rows(
  facility_exact_matches,
  facility_fuzzy_county,
  facility_fuzzy_state
) |>
  slice_best(source_rank, desc(match_score), by = c("agreement_id", "facility_key", "source"), tiebreak = tiebreak) |>
  mutate(source_choice_pending = TRUE)

auto_matches <- bind_rows(non_doc_matches, doc_matches)

facility_all_matches <- bind_rows(
  manual_matches,
  manual_facility_matches,
  auto_matches |>
    anti_join(
      bind_rows(manual_matches, manual_facility_matches),
      by = "agreement_id"
    )
)

# HIFLD fallback: police stations, not a jail census, so it runs last of all
hifld_law_enforcement <- arrow::read_parquet(
  "data/intermediate/agency-roster-hifld.parquet"
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
    is.na(county_key) |
      county_key == "" |
      is.na(facility_source_county_key) |
      county_key == facility_source_county_key,
    # a county-level agreement names its county, so only the station must fit the pattern
    ((is_county_exact_agency | jurisdiction_level == "County") &
      is_exact_county_pattern(facility_name, county)) |
      (is_municipal_exact_agency &
        is_exact_municipal_pattern(facility_name, city_guess)) |
      facility_guess_key == facility_key
  ) |>
  anti_join(
    manual_facility_exclusions,
    by = c("state_key", "agency_key", "facility_key")
  ) |>
  # the shortest name is the main office, not a substation ("... - DISTRICT 1")
  slice_best(desc(str_detect(str_to_lower(facility_name), "jail|detention|correction")),
             str_length(facility_name), by = "agreement_id", tiebreak = tiebreak) |>
  mutate(
    county_key = coalesce(na_if(county_key, ""), facility_source_county_key),
    match_type = "hifld_law_enforcement_location",
    match_score = 1,
    weak_match = TRUE
  ) |>
  select(-facility_source_county_key)

facility_all_matches <- bind_rows(
  facility_all_matches,
  hifld_fallback_matches
)

# whether each matched facility is open now

# a census lists the facilities open on its reference day, taken as mid-year
jail_census <- "Census of Jails"
prison_census <- "Census of State and Federal Adult Correctional Facilities"
census_sites <- censuses |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  mutate(census_date = make_date(census_year, 6, 30), census_row = row_number())

# manual placements and a DOC's own list are verdicts already, and police stations are not in any census
matched_sites <- facility_all_matches |>
  filter(!source %in% c("manual", "manual_doc_facility_list", "hifld_law_enforcement"), !is.na(latitude), !is.na(longitude)) |>
  distinct(source, source_id, facility_name, facility_address, latitude, longitude, type, state_key, facility_status, facility_status_date) |>
  mutate(
    site = row_number(),
    census = if_else(source == "hifld_prisons" & str_to_upper(coalesce(type, "")) %in% c("STATE", "FEDERAL"), prison_census, jail_census)
  )

as_5070 <- function(x) st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |> st_transform(5070)
matched_sites_sf <- as_5070(matched_sites)
census_sites_sf <- as_5070(census_sites)

within_m <- function(near) tibble(i = rep(seq_along(near), lengths(near)), j = unlist(near))

# census rows within 1 km of the site, with both house numbers and how many rows the census has for that
# jurisdiction that round
site_rows_nearby <- within_m(st_is_within_distance(matched_sites_sf, census_sites_sf, dist = 1000)) |>
  transmute(
    site = matched_sites$site[i],
    census_row = census_sites$census_row[j],
    near = as.numeric(st_distance(matched_sites_sf[i, ], census_sites_sf[j, ], by_element = TRUE)) <= 150,
    site_number = house_number(matched_sites$facility_address[i]),
    row_number = house_number(census_sites$facility_address[j])
  ) |>
  left_join(
    census_sites |> filter(!is.na(jurisdiction_id)) |> add_count(jurisdiction_id, census_year, census, name = "n_jurisdiction_rows") |>
      select(census_row, n_jurisdiction_rows),
    by = "census_row"
  )

# census rows at the site: the same kind of facility, in the same state, within 150 m or within 1 km under the
# same house number
site_listings <- site_rows_nearby |>
  filter(near | coalesce(site_number == row_number, FALSE)) |>
  select(site, census_row) |>
  inner_join(matched_sites |> select(site, census, state_key), by = "site") |>
  inner_join(census_sites |> select(census_row, census, state_key, census_year, census_date, jurisdiction_id, year_built),
             by = c("census_row", "census", "state_key"))

# a round shows the site absent when a jurisdiction that ran it is listed there, every row of it
# placed and none of them at the site; 2013 carries no addresses, so it never shows absence
jurisdiction_rounds <- census_sites |>
  filter(census == jail_census) |>
  select(jurisdiction_id, census_year, census_date, census_row) |>
  semi_join(
    censuses |>
      filter(census == jail_census, !is.na(jurisdiction_id)) |>
      summarise(all_placed = all(!is.na(latitude)), .by = c(jurisdiction_id, census_year)) |>
      filter(all_placed),
    by = c("jurisdiction_id", "census_year")
  )

# a row within 1 km is not the site only when it gives another house number and is its jurisdiction's one jail that
# round: a county that built its new jail down the road lists another number, while a county that lists several
# buildings of one downtown complex lists each under its own
site_rounds_nearby <- site_rows_nearby |>
  filter(is.na(site_number) | is.na(row_number) | site_number == row_number | coalesce(n_jurisdiction_rows, 2L) > 1) |>
  select(site, census_row)

site_absences <- site_listings |>
  filter(!is.na(jurisdiction_id)) |>
  distinct(site, jurisdiction_id) |>
  inner_join(jurisdiction_rounds, by = "jurisdiction_id", relationship = "many-to-many") |>
  anti_join(site_rounds_nearby, by = c("site", "census_row")) |>
  anti_join(site_listings, by = c("site", "census_year")) |>
  # every row of the jurisdiction that round must be elsewhere, not just one of them
  group_by(site, jurisdiction_id, census_year, census_date) |>
  summarise(n_far = n(), .groups = "drop") |>
  inner_join(
    jurisdiction_rounds |> count(jurisdiction_id, census_year, name = "n_rows"),
    by = c("jurisdiction_id", "census_year")
  ) |>
  filter(n_far == n_rows) |>
  distinct(site, census_year, census_date)

# HIFLD marks a closed facility; a jail census point takes the status of HIFLD's point at the same site
hifld_sites <- jails_prisons |>
  filter(source == "hifld_prisons", !is.na(latitude), !is.na(longitude), !is.na(facility_status))
hifld_status_nearby <- within_m(st_is_within_distance(matched_sites_sf, as_5070(hifld_sites), dist = 150)) |>
  transmute(site = matched_sites$site[i], nearby_status = hifld_sites$facility_status[j], nearby_status_date = hifld_sites$facility_status_date[j]) |>
  summarise(
    nearby_closed = any(nearby_status == "Closed") & !any(nearby_status == "Open"),
    nearby_status_date = max(nearby_status_date),
    .by = site
  )

site_windows <- matched_sites |>
  select(site, facility_status, facility_status_date) |>
  left_join(
    site_listings |>
      summarise(
        listed_first = min(census_date),
        listed_last = max(census_date),
        census_years = paste(sort(unique(census_year)), collapse = "; "),
        year_built = suppressWarnings(min(year_built, na.rm = TRUE)),
        .by = site
      ),
    by = "site"
  ) |>
  left_join(hifld_status_nearby, by = "site") |>
  left_join(
    site_absences |>
      left_join(site_listings |> summarise(listed_first = min(census_date), listed_last = max(census_date), .by = site), by = "site") |>
      summarise(
        absent_after = suppressWarnings(min(census_date[is.na(listed_last) | census_date > listed_last])),
        absent_before = suppressWarnings(max(census_date[!is.na(listed_first) & census_date < listed_first])),
        .by = site
      ),
    by = "site"
  ) |>
  mutate(
    year_built = if_else(is.finite(year_built), year_built, NA_integer_),
    absent_after = if_else(is.finite(absent_after), absent_after, as.Date(NA)),
    absent_before = if_else(is.finite(absent_before), absent_before, as.Date(NA)),
    hifld_closed_by = case_when(
      facility_status == "Closed" ~ facility_status_date,
      is.na(facility_status) & coalesce(nearby_closed, FALSE) ~ nearby_status_date
    ),
    # closed by the earliest round or snapshot that no longer has it, open from the latest that did not yet; HIFLD
    # checking a site open does not outweigh a round that left it out, since HIFLD re-stamps records it never visited
    closed_by = pmin(absent_after, hifld_closed_by, na.rm = TRUE),
    open_from = pmax(absent_before, make_date(year_built, 1, 1), na.rm = TRUE)
  ) |>
  select(site, census_years, listed_last, closed_by, open_from)

facility_all_matches <- facility_all_matches |>
  left_join(
    matched_sites |> select(source, source_id, facility_name, facility_address, latitude, longitude, site),
    by = c("source", "source_id", "facility_name", "facility_address", "latitude", "longitude")
  ) |>
  left_join(site_windows, by = "site") |>
  # every agreement matched here is active, so a facility any round or HIFLD shows closed leaves it, even its only one
  filter(is.na(closed_by)) |>
  mutate(facility_census_years = census_years) |>
  select(-site, -census_years, -listed_last, -closed_by, -open_from)

# copies of one jail are grouped by hand in inputs/manual-facility-duplicates.csv, with the copy that places it marked keep
facility_duplicates <- read_csv(
  "inputs/manual-facility-duplicates.csv",
  col_types = cols(.default = col_character(), keep = col_logical())
) |>
  transmute(jail, source, source_id, facility_name, keep = coalesce(keep, FALSE))

# of the open candidates for one facility name, the copy marked keep places it, else the best-ranked source
facility_all_matches <- bind_rows(
  facility_all_matches |> filter(!coalesce(source_choice_pending, FALSE)),
  facility_all_matches |>
    filter(coalesce(source_choice_pending, FALSE)) |>
    left_join(facility_duplicates |> filter(keep) |> distinct(source, source_id, facility_name, marked_keep = keep),
              by = c("source", "source_id", "facility_name")) |>
    slice_best(desc(coalesce(marked_keep, FALSE)), source_rank, desc(match_score), by = c("agreement_id", "facility_key"), tiebreak = tiebreak) |>
    select(-marked_keep)
) |>
  select(-source_choice_pending)

# two sources can name one jail differently ("Tulsa County Jail", "David L. Moss Criminal Justice Center"): a candidate
# within 150 m of a better-ranked one from another source, under the same house number, is that jail again. One
# source's own records stay apart, and a DOC's list names each unit once, so its rows stay as listed
merge_pool <- facility_all_matches |>
  filter(source != "manual_doc_facility_list", !is.na(latitude), !is.na(longitude)) |>
  arrange(source_rank, desc(match_score), across(all_of(tiebreak)))
merge_numbers <- house_number(merge_pool$facility_address)
same_jail <- within_m(st_is_within_distance(as_5070(merge_pool), as_5070(merge_pool), dist = 150)) |>
  filter(
    i < j,
    merge_pool$agreement_id[i] == merge_pool$agreement_id[j],
    merge_pool$source[i] != merge_pool$source[j],
    coalesce(merge_numbers[i] == merge_numbers[j], FALSE)
  ) |>
  arrange(j, i)
# in rank order, a candidate goes when it repeats one that stays
repeats <- logical(nrow(merge_pool))
for (p in seq_len(nrow(same_jail))) {
  if (!repeats[same_jail$i[p]]) repeats[same_jail$j[p]] <- TRUE
}
facility_all_matches <- facility_all_matches |>
  anti_join(
    merge_pool[repeats, ] |> select(agreement_id, source, source_id, facility_name, latitude, longitude),
    by = c("agreement_id", "source", "source_id", "facility_name", "latitude", "longitude")
  )

# copies of one jail the merge leaves apart (house numbers differ, points over 150 m apart, or one source lists it
# twice): an agreement keeps the copy marked keep, else its best-ranked copy of that jail
duplicate_copies <- facility_all_matches |>
  inner_join(facility_duplicates, by = c("source", "source_id", "facility_name")) |>
  arrange(desc(keep), source_rank, across(all_of(tiebreak))) |>
  filter(row_number() > 1, .by = c(agreement_id, jail))
facility_all_matches <- facility_all_matches |>
  anti_join(duplicate_copies, by = c("agreement_id", "source", "source_id", "facility_name", "latitude", "longitude"))

# facility point layer

# a match without coordinates cannot be placed, so it re-enters below as unmatched
facility_matched_sf <- facility_all_matches |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# every agreement keeps a row with an empty geometry when it has no open facility or is not active
facility_unmatched <- fac_all |>
  anti_join(
    facility_matched_sf |> st_drop_geometry(),
    by = "agreement_id"
  ) |>
  mutate(
    source = NA_character_,
    match_type = if_else(agreement_id %in% active_ids, "unmatched_facility", "not_active"),
    match_score = NA_real_
  )

facility_unmatched_sf <- st_sf(
  facility_unmatched,
  geometry = st_sfc(
    rep(list(st_point()), nrow(facility_unmatched)),
    crs = st_crs(4326)
  )
)

facility_sf <- bind_rows(facility_matched_sf, facility_unmatched_sf)

# some sources carry no FIPS, so fill it from the containing county polygon
county_containing <- facility_sf |>
  filter(
    !st_is_empty(geometry),
    is.na(state_fips) | is.na(county_fips)
  ) |>
  select(agreement_id, facility_name) |>
  st_join(
    counties_reference(YEAR) |>
      transmute(
        containing_state_fips = statefp,
        containing_county_fips = geoid,
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
    geometry_vintage = NA_integer_,
    # a point left empty because the agreement is not active is intended, not a failed match
    geometry_unmatched = st_is_empty(geometry) & match_type != "not_active",
    fuzzy_match = coalesce(fuzzy_match, FALSE),
    weak_match = coalesce(weak_match, FALSE),
    # HIFLD tags some records "(OLD)" without marking them Closed, and some are the county's current jail (Franklin
    # KS); where the tag is right (Osage KS), inputs/manual-facility-review.csv removes the record, so it would only mislead
    facility_name = str_squish(str_remove(facility_name, regex("\\(old\\)\\s*$", ignore_case = TRUE)))
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
    geometry_vintage,
    geometry_unmatched,
    fuzzy_match,
    weak_match,
    facility_census_years,
    manual_reason,
    manual_note,
    geometry
  )

sf::st_write(facility_sf, dsn = "data/intermediate/match-facility.parquet", driver = "Parquet", layer_options = "USE_PARQUET_GEO_TYPES=ONLY", delete_dsn = file.exists("data/intermediate/match-facility.parquet"), quiet = TRUE)
