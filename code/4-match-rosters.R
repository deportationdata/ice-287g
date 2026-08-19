library(tidyverse)

source("code/functions.R")

# ORIs and roster FIPS codes are annotations on an agreement, not inputs to any
# geometry match, so they are resolved here at the end and written to their own
# table for 5-format-agreements-dataset.R to join by agreement_id

# every roster matches the same way: exact state + county + agency name,
# preferring an identical full name when the aggressive agency_key collides
# (Mahanoy City PD and Mahanoy Township PD both key as "mahanoy"), then falling
# back to a statewide name match when the sheet's county is missing, a typo or
# in disagreement — accepted only when the name identifies one ORI in the state
match_agency_source <- function(
  agreements,
  lookup,
  ori_col,
  match_type_col,
  fallback = TRUE
) {
  value_cols <- setdiff(
    names(lookup),
    c("state_key", "county_key", "agency_key", "source_fullname_key")
  )

  exact <- agreements |>
    left_join(
      lookup,
      by = c("state_key", "county_key", "agency_key"),
      relationship = "many-to-many"
    ) |>
    group_by(agreement_id) |>
    arrange(
      desc(source_fullname_key == fullname_key),
      .data[[ori_col]],
      .by_group = TRUE
    ) |>
    slice_head(n = 1) |>
    ungroup() |>
    mutate(
      "{match_type_col}" := if_else(
        is.na(.data[[ori_col]]),
        NA_character_,
        "exact_state_county_agency_name"
      )
    ) |>
    select(-source_fullname_key)

  # the full name discriminates where the aggressive key cannot: "Melbourne
  # Police Department" is one agency statewide even though agency_key
  # "melbourne" also covers Melbourne Village PD. Every roster gets this tier;
  # only the looser agency_key tier below is opt-in
  fullname_unique <- lookup |>
    group_by(state_key, source_fullname_key) |>
    filter(n_distinct(.data[[ori_col]]) == 1) |>
    slice_head(n = 1) |>
    ungroup()

  state_unique <- lookup |>
    group_by(state_key, agency_key) |>
    filter(n_distinct(.data[[ori_col]]) == 1) |>
    slice_head(n = 1) |>
    ungroup()

  by_fullname <- exact |>
    filter(is.na(.data[[match_type_col]])) |>
    select(-all_of(value_cols)) |>
    left_join(
      fullname_unique |>
        select(state_key, source_fullname_key, all_of(value_cols)),
      by = c("state_key", "fullname_key" = "source_fullname_key")
    ) |>
    mutate(
      "{match_type_col}" := if_else(
        is.na(.data[[ori_col]]),
        NA_character_,
        "unique_state_full_name"
      )
    )

  if (!fallback) {
    return(arrange(
      bind_rows(
        exact |> filter(!is.na(.data[[match_type_col]])),
        by_fullname
      ),
      agreement_id
    ))
  }

  bind_rows(
    exact |> filter(!is.na(.data[[match_type_col]])),
    by_fullname |> filter(!is.na(.data[[match_type_col]])),
    by_fullname |>
      filter(is.na(.data[[match_type_col]])) |>
      select(-all_of(value_cols)) |>
      left_join(
        state_unique |> select(state_key, agency_key, all_of(value_cols)),
        by = c("state_key", "agency_key")
      ) |>
      mutate(
        "{match_type_col}" := if_else(
          is.na(.data[[ori_col]]),
          NA_character_,
          "unique_state_agency_name"
        )
      )
  ) |>
    arrange(agreement_id)
}

# one candidate row per key, so a join can only multiply on the match keys
dedupe_lookup <- function(x) {
  x |>
    group_by(state_key, county_key, agency_key, source_fullname_key) |>
    slice_head(n = 1) |>
    ungroup()
}

leaic <- arrow::read_parquet("data/leaic.parquet")
lear <- arrow::read_parquet("data/lear.parquet")
crime <- arrow::read_parquet("data/crime.parquet")
hifld <- arrow::read_parquet("data/hifld-law-enforcement.parquet")

leaic_lookup <- leaic |>
  select(
    state_key,
    county_key,
    agency_key,
    source_fullname_key = fullname_key,
    leaic_ori = ori,
    leaic_name = name,
    leaic_county_fips = county_fips,
    leaic_place_fips = place_fips
  ) |>
  dedupe_lookup()

lear_lookup <- lear |>
  select(
    state_key,
    county_key,
    agency_key,
    source_fullname_key = fullname_key,
    lear_ori = ori,
    lear_name = name,
    lear_county_fips = county_fips
  ) |>
  dedupe_lookup()

crime_lookup <- crime |>
  select(
    state_key,
    county_key,
    agency_key,
    source_fullname_key = fullname_key,
    crime_ori = ori,
    crime_name = name,
    crime_county_fips = county_fips
  ) |>
  dedupe_lookup()

# HIFLD contributes no ORI, but its station addresses give an independent
# county per agency for the same cross-check the other rosters feed
hifld_lookup <- hifld |>
  filter(!is.na(county_fips)) |>
  select(
    state_key,
    county_key,
    agency_key,
    source_fullname_key = fullname_key,
    hifld_county_fips = county_fips
  ) |>
  dedupe_lookup()

# a cleaned name several real agencies share cannot identify a record, so
# 5-format's place confirmation applies only where every roster agrees the name
# names at most one agency statewide. Counted from the raw rosters because the
# deduped lookups would collapse duplicate-ORI records (Miami PD carries two
# ORIs in LEAIC) that must count as collisions
roster_key_unique_tbl <- bind_rows(
  leaic |> distinct(state_key, agency_key, id = ori),
  lear |> distinct(state_key, agency_key, id = ori),
  crime |> distinct(state_key, agency_key, id = ori),
  hifld |> distinct(state_key, agency_key, id = fullname_key),
  .id = "roster"
) |>
  count(roster, state_key, agency_key) |>
  group_by(state_key, agency_key) |>
  summarize(roster_key_unique = max(n) <= 1, .groups = "drop")

manual_agency_ori <- read_csv(
  "inputs/manual-agency-ori.csv",
  col_types = cols(.default = "c")
) |>
  filter(!is.na(ORI9), ORI9 != "") |>
  distinct(state, county, agency, .keep_all = TRUE)

agreement_identifiers <- arrow::read_parquet("data/agreements.parquet") |>
  select(agreement_id, state, county, agency) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(agency),
    fullname_key = norm_ori_fullname(agency)
  ) |>
  match_agency_source(leaic_lookup, "leaic_ori", "leaic_match_type") |>
  match_agency_source(lear_lookup, "lear_ori", "lear_match_type") |>
  match_agency_source(
    crime_lookup,
    "crime_ori",
    "crime_match_type",
    fallback = FALSE
  ) |>
  match_agency_source(hifld_lookup, "hifld_county_fips", "hifld_match_type") |>
  left_join(roster_key_unique_tbl, by = c("state_key", "agency_key")) |>
  mutate(
    # a key absent from every roster has no collision evidence either way, so
    # treat it as not-unique and it can never certify a confirmation
    roster_key_unique = coalesce(roster_key_unique, FALSE)
  ) |>
  mutate(
    # rosters naming the same agency with different ORIs means at least one
    # match is wrong; the flag rides as its own column because the geometry
    # scripts have already written their needs_review, and 5-format ORs it in
    ori_conflict = coalesce(
      (!is.na(leaic_ori) & !is.na(lear_ori) & leaic_ori != lear_ori) |
        (!is.na(leaic_ori) & !is.na(crime_ori) & leaic_ori != crime_ori) |
        (!is.na(lear_ori) & !is.na(crime_ori) & lear_ori != crime_ori),
      FALSE
    ),
    # how an ORI was found outranks which roster found it: an agency named in
    # its own county identifies a record more surely than a statewide name
    # search does, whatever the roster's vintage. Two Manor Township PDs sit in
    # Pennsylvania, and only one is in the newest roster, so a statewide hit
    # there must not displace another roster's exact county match. Rosters tie
    # by recency: CDE (2025) over LEAR (2016) over LEAIC (2012)
    crime_rank = match_tier_rank(crime_match_type),
    lear_rank = match_tier_rank(lear_match_type),
    leaic_rank = match_tier_rank(leaic_match_type),
    best_rank = pmin(crime_rank, lear_rank, leaic_rank),
    ori_source = case_when(
      !is.na(crime_ori) & crime_rank == best_rank ~ "crime_lookup",
      !is.na(lear_ori) & lear_rank == best_rank ~ "lear",
      !is.na(leaic_ori) & leaic_rank == best_rank ~ "leaic",
      TRUE ~ NA_character_
    ),
    ORI9 = case_when(
      ori_source == "crime_lookup" ~ crime_ori,
      ori_source == "lear" ~ lear_ori,
      ori_source == "leaic" ~ leaic_ori,
      TRUE ~ NA_character_
    )
  ) |>
  select(-crime_rank, -lear_rank, -leaic_rank, -best_rank) |>
  # a manual row with a county applies only to agreements naming it, while a
  # county-less row applies to that agency statewide, so the specific one wins
  left_join(
    manual_agency_ori |>
      filter(!is.na(county), county != "") |>
      select(state, county, agency, manual_ori_specific = ORI9),
    by = c("state", "county", "agency")
  ) |>
  left_join(
    manual_agency_ori |>
      filter(is.na(county) | county == "") |>
      select(state, agency, manual_ori_general = ORI9),
    by = c("state", "agency")
  ) |>
  mutate(
    manual_ori = coalesce(manual_ori_specific, manual_ori_general),
    ori_source = if_else(
      is.na(ORI9) & !is.na(manual_ori),
      "manual",
      ori_source
    ),
    ORI9 = coalesce(ORI9, manual_ori)
  ) |>
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
    leaic_match_type,
    lear_ori,
    lear_name,
    lear_county_fips,
    lear_match_type,
    crime_ori,
    crime_name,
    crime_county_fips,
    crime_match_type,
    hifld_county_fips,
    hifld_match_type
  )

# downstream joins on agreement_id would silently fan out on a duplicate
stopifnot(
  "agreement-identifiers must carry exactly one row per agreement_id" = !anyDuplicated(
    agreement_identifiers$agreement_id
  )
)

arrow::write_parquet(
  agreement_identifiers,
  "data/agreement-identifiers.parquet"
)
