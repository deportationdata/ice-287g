# Assemble the per-feature layer (intermediate/all_agreements_sf) and the
# published agreement-level-sf
library(tidyverse)
library(sf)

source("code/functions.R")

non_facility_sf <- st_read(
  "data/intermediate/match-non-facility.parquet",
  quiet = TRUE
)
facility_sf <- st_read("data/intermediate/match-facility.parquet", quiet = TRUE)

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

stopifnot(
  "non-facility layer file must arrive in EPSG:4326" = st_crs(
    non_facility_sf
  ) ==
    st_crs(4326),
  "facility layer file must arrive in EPSG:4326" = st_crs(facility_sf) ==
    st_crs(4326)
)

# the layers partition the agreements: every agreement sits in exactly one layer, and only a
# facility agreement, a regional department or a judicial district fans out
non_facility_ids <- non_facility_sf |>
  st_drop_geometry() |>
  filter(coalesce(
    !match_type %in%
      c("regional_member_municipality", "judicial_district_member_county"),
    TRUE
  )) |>
  pull(agreement_id)
stopifnot(
  "every agreement is in exactly one layer" = setequal(
    c(non_facility_sf$agreement_id, facility_sf$agreement_id),
    agreements$agreement_id
  ) &&
    !any(facility_sf$agreement_id %in% non_facility_sf$agreement_id),
  "only regional departments and judicial districts fan out in the non-facility layers" = !anyDuplicated(
    non_facility_ids
  ),
  "the facility layer holds exactly the facility-point agreements" = setequal(
    facility_sf$agreement_id,
    agreements$agreement_id[agreements$geom_class == "facility_point"]
  )
)

# the review vocabulary: every flag a layer or roster stage sets, and the one
# wording it gets; a flag outside this table cannot reach the published files
review_vocabulary <- tribble(
  ~flag                         , ~reason                                          ,
  "geom_class_unknown"          , "agreement type cannot be placed"                ,
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

# unmatched agreements ride along with empty geometries; nothing is dropped
all_agreements_sf <-
  bind_rows(
    non_facility_sf,
    facility_sf |>
      mutate(match_layer = "facility")
  ) |>
  st_make_valid() |>
  left_join(
    agreements |>
      select(
        agreement_id,
        agency_id,
        status,
        sheet_row,
        n_sheet_rows,
        state,
        county,
        agency,
        ice_type,
        jurisdiction_level,
        jurisdiction_level_source,
        support_type,
        signed,
        moa,
        addendum,
        first_appeared,
        first_appeared_source,
        last_appeared,
        removed_by,
        removed_by_source,
        removal_flag,
        geom_class
      ),
    by = "agreement_id"
  ) |>
  left_join(agreement_identifiers, by = "agreement_id") |>
  mutate(
    # facility rows have no polygon; the published geoid contract is county fips
    geoid = if_else(match_layer == "facility", county_fips, geoid),
    # only county/municipal matches can be cross-checked against roster counties
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
    # CDE can list several counties ("01081;01087"), so test membership
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
    geom_class_unknown = geom_class == "unknown",
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
    has_addendum = !is.na(addendum)
  )
all_agreements_sf <- all_agreements_sf |>
  mutate(
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
    sheet_row,
    state,
    county,
    agency,
    ice_type,
    jurisdiction_level,
    jurisdiction_level_source,
    support_type,
    signed,
    moa,
    addendum,
    first_appeared,
    first_appeared_source,
    last_appeared,
    removed_by,
    removed_by_source,
    removal_flag,
    geom_class,
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
    place_fips,
    geoid,
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

# an agreement can span several features, so a code is kept only when unique
single_or_na <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 1) ux else NA_character_
}
quality_order <- c(
  "unmatched",
  "weak",
  "fuzzy",
  "exact_flagged",
  "manual",
  "exact"
)

agreement_level_sf <- all_agreements_sf |>
  group_by(
    agreement_id,
    agency_id,
    status,
    sheet_row,
    agency,
    state,
    county,
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
    ice_type,
    jurisdiction_level,
    jurisdiction_level_source,
    geom_class,
    state_fips
  ) |>
  summarize(
    match_layer = paste(sort(unique(match_layer)), collapse = "+"),
    county_fips = single_or_na(county_fips),
    place_fips = single_or_na(place_fips),
    geoid = single_or_na(geoid),
    vtd_code = single_or_na(vtd_code),
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
    moa_pending = coalesce(moa == "pending", FALSE)
  ) |>
  arrange(sheet_row, agreement_id) |>
  select(
    # the sheet's own order first, then derived and spatial fields
    agreement_id,
    agency_id,
    status,
    sheet_row,
    state,
    agency,
    ice_type,
    jurisdiction_level,
    jurisdiction_level_source,
    county,
    support_type,
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
    state_fips,
    county_fips,
    place_fips,
    geoid,
    vtd_code,
    geom_class,
    geometry_vintage,
    has_addendum,
    moa_pending,
    match_layer,
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
    !anyDuplicated(agreement_level_sf$agreement_id)
)

sf::st_write(
  agreement_level_sf,
  dsn = "data/agreements-sf.parquet",
  driver = "Parquet",
  layer_options = "USE_PARQUET_GEO_TYPES=ONLY",
  delete_dsn = file.exists("data/agreements-sf.parquet"),
  quiet = TRUE
)
