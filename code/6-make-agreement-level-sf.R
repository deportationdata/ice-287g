# Assemble the per-feature layer (intermediate/match-all-features) and the
# published agreement-level-sf
library(tidyverse)
library(sf)

source("code/functions.R")

features_sf <- st_read("data/intermediate/match-features.parquet", quiet = TRUE)

agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")

agreement_identifiers <-
  arrow::read_parquet("data/intermediate/match-agency-identifiers.parquet") |>
  select(
    agreement_id,
    ORI9,
    ori_source,
    ori_conflict,
    ori_ambiguous,
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

state_codes <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")

# the review vocabulary: every flag a layer or roster stage sets, and the one
# wording it gets; a flag outside this table cannot reach the published files
review_vocabulary <- tribble(
  ~flag                         , ~reason                                          ,
  "geometry_type_unknown"       , "agreement type cannot be placed"                ,
  "geometry_unmatched"          , "no geometry matched"                            ,
  "fuzzy_match"                 , "fuzzy name match"                               ,
  "weak_match"                  , "placed by a police-station fallback"            ,
  "ambiguous_candidates"        , "several candidate geometries"                   ,
  "type_mismatch"               , "municipality type differs from the sheet"       ,
  "university_address_mismatch" , "campus address county differs from the polygon" ,
  "university_county_mismatch"  , "sheet county differs from the campus polygon"   ,
  "roster_county_disagrees"     , "a roster places the agency in another county"   ,
  "ori_conflict"                , "rosters disagree on the ORI"                    ,
  "ori_ambiguous"               , "several ORIs fit the agency"                    ,
  "duplicate_sheet_row"         , "printed more than once on the sheet"
)
compose_review_reason <- function(flags) {
  stopifnot(
    "every review flag is in the vocabulary" = setequal(
      names(flags),
      review_vocabulary$flag
    )
  )
  flags <- as_tibble(flags)[review_vocabulary$flag]
  pmap_chr(flags, \(...) {
    hit <- review_vocabulary$reason[c(...)]
    if (length(hit)) paste(hit, collapse = "; ") else NA_character_
  })
}

all_agreements_sf <- features_sf |>
  left_join(
    agreements |>
      select(
        agreement_id,
        agency_id,
        status,
        latest_sheet_row,
        latest_sheet,
        latest_sheet_url,
        n_sheet_rows,
        state,
        ice_county = county,
        agency,
        ice_type,
        jurisdiction_level,
        jurisdiction_level_source,
        support_type,
        ice_support_type,
        signed,
        moa,
        addendum,
        first_appeared,
        first_appeared_source,
        last_appeared,
        removed_by,
        removed_by_source,
        removal_flag,
        geometry_type
      ),
    by = "agreement_id"
  ) |>
  left_join(agreement_identifiers, by = "agreement_id") |>
  mutate(
    # only county/municipal matches are checked against roster counties; a roster agrees
    # when its county is among the feature's
    checkable_layer = match_layer %in% c("county", "municipal"),
    leaic_fips_mismatch = case_when(
      match_layer == "county" ~ coalesce(
        !is.na(leaic_county_fips) &
          !is.na(county_fips) &
          !among(leaic_county_fips, county_fips),
        FALSE
      ),
      match_layer == "municipal" ~ coalesce(
        (!is.na(leaic_county_fips) &
          !is.na(county_fips) &
          !among(leaic_county_fips, county_fips)) |
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
          !among(lear_county_fips, county_fips),
        FALSE
      ),
      NA
    ),
    crime_fips_mismatch = if_else(
      checkable_layer,
      coalesce(
        !is.na(crime_county_fips) &
          !is.na(county_fips) &
          !map2_lgl(
            coalesce(crime_county_fips, ""),
            coalesce(county_fips, ""),
            \(codes, listed) any(among(str_split_1(codes, ";"), listed))
          ),
        FALSE
      ),
      NA
    ),
    hifld_fips_mismatch = if_else(
      checkable_layer,
      coalesce(
        !is.na(hifld_county_fips) &
          !is.na(county_fips) &
          !among(hifld_county_fips, county_fips),
        FALSE
      ),
      NA
    ),
    # "00000" and "99xxx" are LEAIC place sentinels, not real census places
    leaic_place_confirmed = coalesce(
      match_layer == "municipal" &
        !is.na(leaic_place_fips) &
        leaic_place_fips != "00000" &
        !str_starts(leaic_place_fips, "99") &
        !is.na(place_fips) &
        leaic_place_fips == place_fips &
        roster_key_unique,
      FALSE
    )
  ) |>
  # a roster disagreement is judged per agreement: one feature the roster agrees
  # with (a regional department's own member) clears it
  mutate(
    across(
      c(
        leaic_fips_mismatch,
        lear_fips_mismatch,
        crime_fips_mismatch,
        hifld_fips_mismatch
      ),
      \(x) if (all(is.na(x))) NA else all(x, na.rm = TRUE)
    ),
    .by = agreement_id
  ) |>
  mutate(
    geometry_type_unknown = is.na(geometry_type),
    duplicate_sheet_row = coalesce(n_sheet_rows > 1, FALSE),
    ambiguous_candidates = coalesce(ambiguous_candidates, FALSE) &
      !leaic_place_confirmed,
    type_mismatch = coalesce(type_mismatch, FALSE) & !leaic_place_confirmed,
    roster_county_disagrees = coalesce(leaic_fips_mismatch, FALSE) |
      coalesce(lear_fips_mismatch, FALSE) |
      coalesce(crime_fips_mismatch, FALSE) |
      coalesce(hifld_fips_mismatch, FALSE),
    across(
      c(
        geometry_unmatched,
        fuzzy_match,
        weak_match,
        university_address_mismatch,
        university_county_mismatch,
        ori_conflict,
        ori_ambiguous
      ),
      \(x) coalesce(x, FALSE)
    ),
    moa_pending = coalesce(moa == "pending", FALSE),
    has_addendum = !is.na(addendum),
    ice_state_fips = state_codes$state_fips[match(state, state_codes$state_full)]
  )
stopifnot(
  "every ICE state resolves to a census state code" = !any(
    is.na(all_agreements_sf$ice_state_fips)
  ),
  "every feature with a state code lies in ICE's state" = !any(
    !is.na(all_agreements_sf$state_fips) &
      all_agreements_sf$state_fips != all_agreements_sf$ice_state_fips
  )
)

all_agreements_sf <- all_agreements_sf |>
  mutate(
    state_fips = ice_state_fips,
    review_reason = compose_review_reason(st_drop_geometry(all_agreements_sf)[
      review_vocabulary$flag
    ]),
    needs_review = !is.na(review_reason),
    match_quality = case_when(
      geometry_unmatched ~ "unmatched",
      str_detect(coalesce(match_type, ""), "^manual") |
        coalesce(
          match_type %in%
            c(
              "regional_member_municipality",
              "judicial_district_member_county"
            ),
          FALSE
        ) ~ "manual",
      fuzzy_match ~ "fuzzy",
      weak_match ~ "weak",
      needs_review ~ "exact_flagged",
      TRUE ~ "exact"
    )
  ) |>
  select(
    agreement_id,
    agency_id,
    status,
    latest_sheet_row,
    latest_sheet,
    latest_sheet_url,
    state,
    ice_county,
    agency,
    ice_type,
    jurisdiction_level,
    jurisdiction_level_source,
    support_type,
    ice_support_type,
    signed,
    moa,
    addendum,
    first_appeared,
    first_appeared_source,
    last_appeared,
    removed_by,
    removed_by_source,
    removal_flag,
    geometry_type,
    match_layer,
    match_name,
    match_type,
    match_score,
    match_quality,
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
    county,
    layer_county_fips,
    place_fips,
    place_geoid,
    place,
    place_type,
    geoid,
    geoid_type,
    vtd_code,
    geometry_vintage,
    ORI9,
    ori_source,
    ori_conflict,
    ori_ambiguous,
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
    all_of(review_vocabulary$flag),
    moa_pending,
    has_addendum,
    manual_reason,
    manual_note,
    review_reason,
    needs_review,
    geometry
  )

sf::st_write(
  all_agreements_sf,
  dsn = "data/intermediate/match-all-features.parquet",
  driver = "Parquet",
  layer_options = "USE_PARQUET_GEO_TYPES=ONLY",
  delete_dsn = file.exists("data/intermediate/match-all-features.parquet"),
  quiet = TRUE
)

# an agreement can span several features, so a unit code is kept only when unique and
# its counties are listed
single_or_na <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 1) ux else NA_character_
}
union_codes <- function(lists) {
  cs <- unique(unlist(str_split(lists[!is.na(lists)], ";")))
  if (length(cs)) paste(cs, collapse = ";") else NA_character_
}
quality_order <- c(
  "unmatched",
  "weak",
  "fuzzy",
  "exact_flagged",
  "manual",
  "exact"
)

# ICE's county as a census code, for the agreements with no geometry
reference_counties <- arrow::read_parquet("data/intermediate/reference-counties.parquet")
ice_county_codes <- reference_counties |>
  distinct(state, county_key, .keep_all = TRUE) |>
  select(state, county_key, ice_county_fips = county_fips)
county_names <- reference_counties |> distinct(county_fips, county) |> deframe()

agreement_level_sf <- all_agreements_sf |>
  group_by(
    agreement_id,
    agency_id,
    status,
    latest_sheet_row,
    latest_sheet,
    latest_sheet_url,
    agency,
    state,
    state_fips,
    ice_county,
    signed,
    moa,
    addendum,
    first_appeared,
    first_appeared_source,
    last_appeared,
    removed_by,
    removed_by_source,
    removal_flag,
    ORI9,
    support_type,
    ice_support_type,
    ice_type,
    jurisdiction_level,
    jurisdiction_level_source,
    geometry_type
  ) |>
  summarize(
    match_layer = paste(sort(unique(match_layer)), collapse = "+"),
    county_fips = union_codes(county_fips),
    place_geoid = single_or_na(place_geoid),
    place = single_or_na(place),
    place_type = single_or_na(place_type),
    geoid = single_or_na(geoid),
    geoid_type = single_or_na(geoid_type),
    geometry_vintage = as.integer(single_or_na(as.character(geometry_vintage))),
    across(all_of(review_vocabulary$flag), any),
    match_quality = quality_order[min(match(match_quality, quality_order))],
    geometry = st_union(geometry),
    .groups = "drop"
  )
agreement_level_sf <- agreement_level_sf |>
  mutate(
    review_reason = compose_review_reason(st_drop_geometry(agreement_level_sf)[
      review_vocabulary$flag
    ]),
    needs_review = !is.na(review_reason),
    has_addendum = !is.na(addendum),
    moa_pending = coalesce(moa == "pending", FALSE),
    # a type goes with its code
    geoid_type = if_else(is.na(geoid), NA_character_, geoid_type),
    place_type = if_else(is.na(place_geoid), NA_character_, place_type),
    county_key = norm_county(ice_county)
  ) |>
  left_join(ice_county_codes, by = c("state", "county_key")) |>
  # an agreement without boundaries is still in ICE's county
  mutate(
    county_fips = if_else(st_is_empty(geometry) & is.na(county_fips), ice_county_fips, county_fips),
    county = name_codes(county_fips, county_names)
  ) |>
  select(-county_key, -ice_county_fips) |>
  arrange(desc(last_appeared), latest_sheet_row, agreement_id) |>
  select(
    # the agency and where it is, the agreement's history, its geography, then ICE's values as printed
    agreement_id,
    agency_id,
    agency,
    ORI9,
    place,
    place_type,
    place_geoid,
    county,
    county_fips,
    state,
    state_fips,
    status,
    support_type,
    signed,
    moa,
    moa_pending,
    addendum,
    has_addendum,
    first_appeared,
    first_appeared_source,
    last_appeared,
    removed_by,
    removed_by_source,
    removal_flag,
    jurisdiction_level,
    jurisdiction_level_source,
    geometry_type,
    geoid,
    geoid_type,
    geometry_vintage,
    ice_county,
    ice_type,
    ice_support_type,
    latest_sheet_row,
    latest_sheet,
    latest_sheet_url,
    match_quality,
    review_reason,
    needs_review,
    geometry
  )

stopifnot(
  "agreement-level dataset must hold every agreement exactly once" = setequal(
    agreement_level_sf$agreement_id,
    agreements$agreement_id
  ) &&
    !anyDuplicated(agreement_level_sf$agreement_id),
  "a county name travels with its code" = all(
    is.na(agreement_level_sf$county) == is.na(agreement_level_sf$county_fips)
  ),
  "a place name travels with its code" = all(
    is.na(agreement_level_sf$place) == is.na(agreement_level_sf$place_geoid)
  ),
  "a place type never outlives its place" = !any(
    is.na(agreement_level_sf$place) & !is.na(agreement_level_sf$place_type)
  )
)

# the review columns stay in match-all-features.parquet for the QA report and the PR diff
sf::st_write(
  agreement_level_sf |> select(-match_quality, -review_reason, -needs_review),
  dsn = "data/agreements-sf.parquet",
  driver = "Parquet",
  layer_options = "USE_PARQUET_GEO_TYPES=ONLY",
  delete_dsn = file.exists("data/agreements-sf.parquet"),
  quiet = TRUE
)
