# Match agreements to roster ORIs -> data/agreement-identifiers.parquet
library(tidyverse)

source("code/functions.R")

# tiers: exact state+county+agency, then statewide full name, then statewide key
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

  # dedupe here only: the uniqueness tiers must count raw rows to see collisions
  lookup_deduped <- dedupe_lookup(lookup)

  # a blank sheet county must not "exactly" match an unknown roster county
  exact <- agreements |>
    left_join(
      lookup_deduped,
      by = c("state_key", "county_key", "agency_key"),
      relationship = "many-to-many",
      na_matches = "never"
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

  # the full name discriminates where the aggressive agency_key collides
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
  )

lear_lookup <- lear |>
  select(
    state_key,
    county_key,
    agency_key,
    source_fullname_key = fullname_key,
    lear_ori = ori,
    lear_name = name,
    lear_county_fips = county_fips
  )

crime_lookup <- crime |>
  select(
    state_key,
    county_key,
    agency_key,
    source_fullname_key = fullname_key,
    crime_ori = ori,
    crime_name = name,
    crime_county_fips = county_fips
  )

# HIFLD has no ORI; it contributes an independent county for the cross-check
hifld_lookup <- hifld |>
  filter(!is.na(county_fips)) |>
  select(
    state_key,
    county_key,
    agency_key,
    source_fullname_key = fullname_key,
    hifld_county_fips = county_fips
  )

# counted from raw rosters: deduping would hide name collisions from 5-format
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
    # no roster evidence means not-unique, so it can never certify a match
    roster_key_unique = coalesce(roster_key_unique, FALSE)
  ) |>
  mutate(
    # conflicting roster ORIs mean a match is wrong; 5-format ORs this in
    ori_conflict = coalesce(
      (!is.na(leaic_ori) & !is.na(lear_ori) & leaic_ori != lear_ori) |
        (!is.na(leaic_ori) & !is.na(crime_ori) & leaic_ori != crime_ori) |
        (!is.na(lear_ori) & !is.na(crime_ori) & lear_ori != crime_ori),
      FALSE
    ),
    # tier outranks roster; ties by recency: crime 2025, lear 2016, leaic 2012
    crime_rank = match_tier_rank(crime_match_type),
    lear_rank = match_tier_rank(lear_match_type),
    leaic_rank = match_tier_rank(leaic_match_type),
    best_rank = pmin(crime_rank, lear_rank, leaic_rank),
    ori_source = case_when(
      !is.na(crime_ori) & crime_rank == best_rank ~ "crime",
      !is.na(lear_ori) & lear_rank == best_rank ~ "lear",
      !is.na(leaic_ori) & leaic_rank == best_rank ~ "leaic",
      TRUE ~ NA_character_
    ),
    ORI9 = case_when(
      ori_source == "crime" ~ crime_ori,
      ori_source == "lear" ~ lear_ori,
      ori_source == "leaic" ~ leaic_ori,
      TRUE ~ NA_character_
    )
  ) |>
  select(-crime_rank, -lear_rank, -leaic_rank, -best_rank) |>
  # a county-less manual row applies statewide; the county-specific row wins
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

stopifnot(
  "agreement-identifiers must carry exactly one row per agreement_id" = !anyDuplicated(
    agreement_identifiers$agreement_id
  )
)

arrow::write_parquet(
  agreement_identifiers,
  "data/agreement-identifiers.parquet"
)
