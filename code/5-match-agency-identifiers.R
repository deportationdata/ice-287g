# Match agreements to roster ORIs -> data/intermediate/match-agency-identifiers.parquet
library(tidyverse)

source("code/functions.R")

# tiers: exact state+county+agency, then statewide full name, then statewide key.
# Candidates fan out and the pick is a total order: the full name first, an
# agency-level ORI (…0000) over a sub-unit, then the lowest ORI; several ORIs
# tied at the top are reported as ambiguous rather than silently resolved
match_agency_source <- function(
  agreements,
  lookup,
  ori_col,
  match_type_col,
  fallback = TRUE,
  prefer_agency_level = TRUE,
  prefix = fallback,
  prefix_skip = character()
) {
  value_cols <- setdiff(
    names(lookup),
    c("state_key", "county_key", "agency_key", "source_fullname_key")
  )
  ambiguous_col <- paste0(ori_col, "_ambiguous")
  agency_level_ori <- \(x) if (prefer_agency_level) str_detect(coalesce(x, ""), "0000$") else FALSE

  # a blank sheet county must not "exactly" match an unknown roster county
  exact <- agreements |>
    left_join(
      lookup,
      by = c("state_key", "county_key", "agency_key"),
      relationship = "many-to-many",
      na_matches = "never"
    ) |>
    mutate(
      fullname_hit = coalesce(source_fullname_key == fullname_key, FALSE),
      .by_agreement_ids = n_distinct(.data[[ori_col]], na.rm = TRUE),
      .fullname_ids = n_distinct(.data[[ori_col]][fullname_hit], na.rm = TRUE),
      .by = agreement_id
    ) |>
    mutate(
      "{ambiguous_col}" := if_else(.fullname_ids > 0, .fullname_ids > 1, .by_agreement_ids > 1)
    ) |>
    arrange(desc(fullname_hit), desc(agency_level_ori(.data[[ori_col]])), .data[[ori_col]]) |>
    slice_head(n = 1, by = agreement_id) |>
    mutate(
      "{match_type_col}" := if_else(
        is.na(.data[[ori_col]]),
        NA_character_,
        "exact_state_county_agency_name"
      )
    ) |>
    select(-source_fullname_key, -fullname_hit, -.by_agreement_ids, -.fullname_ids)

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
    state_unique <- slice(state_unique, 0)
  }

  by_key <- by_fullname |>
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

  # a roster name that begins with the sheet's and adds a suffix (Kennard PD New Castle, Kinney
  # County Constable Precinct 1, Utah Department of Corrections Law Enforcement Bureau): only
  # where no roster row carries the sheet's key exactly, only when the candidates agree on one
  # ORI, the same county's first. Tested 2026-09-13: 13 hand ORIs reproduced, none changed
  # letters the roster name adds to the sheet's
  extra_col <- paste0(ori_col, "_prefix_extra")
  still <- by_key |>
    filter(is.na(.data[[match_type_col]])) |>
    select(-all_of(value_cols))
  by_prefix <- if (prefix && nrow(still)) {
    exact_keys <- lookup |> distinct(state_key, agency_key) |> mutate(has_exact = TRUE)
    still |>
      # no prefix guess for an agency an earlier roster placed
      filter(if (length(prefix_skip)) if_all(all_of(prefix_skip), is.na) else TRUE) |>
      left_join(exact_keys, by = c("state_key", "agency_key")) |>
      filter(is.na(has_exact), nchar(agency_key) >= 6) |>
      select(-has_exact) |>
      inner_join(
        lookup |> select(state_key, cand_key = agency_key, cand_county = county_key, all_of(value_cols)),
        by = "state_key",
        relationship = "many-to-many"
      ) |>
      filter(str_starts(cand_key, fixed(agency_key))) |>
      mutate(same_county = coalesce(cand_county == county_key, FALSE)) |>
      group_by(agreement_id) |>
      filter(if (any(same_county)) same_county else TRUE) |>
      filter(n_distinct(.data[[ori_col]]) == 1) |>
      mutate("{extra_col}" := nchar(cand_key) - nchar(agency_key)) |>
      slice_min(.data[[extra_col]], n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(-cand_key, -cand_county, -same_county) |>
      mutate("{match_type_col}" := "prefix_state_agency_name")
  } else {
    still |> slice(0) |> mutate("{extra_col}" := integer())
  }

  bind_rows(
    exact |> filter(!is.na(.data[[match_type_col]])),
    by_fullname |> filter(!is.na(.data[[match_type_col]])),
    by_key |> filter(!is.na(.data[[match_type_col]])),
    by_prefix,
    still |> anti_join(by_prefix, by = "agreement_id")
  ) |>
    arrange(agreement_id)
}

# keys are built here, not read from the roster parquets, so both sides share one
# normalization: a town marshal is the town's police outside Louisiana
roster_keys <- function(roster) {
  roster |>
    mutate(agency_key = norm_ori_agency(marshal_as_police(name, state_key)),
           fullname_key = norm_ori_fullname(name))
}

leaic <- arrow::read_parquet("data/intermediate/agency-roster-leaic-2012.parquet") |> roster_keys()
lear <- arrow::read_parquet("data/intermediate/agency-roster-lear-2016.parquet") |> roster_keys()
crime <- arrow::read_parquet("data/intermediate/agency-roster-cde-2025.parquet") |> roster_keys()
hifld <- arrow::read_parquet("data/intermediate/agency-roster-hifld.parquet") |> roster_keys()

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
  separate_longer_delim(county_key, ";") |>
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

agreement_identifiers <- arrow::read_parquet("data/intermediate/agreements.parquet") |>
  select(agreement_id, state, county, agency, jurisdiction_level) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(marshal_as_police(public_safety_as_police(agency, jurisdiction_level), state_key)),
    fullname_key = norm_ori_fullname(agency)
  ) |>
  match_agency_source(leaic_lookup, "leaic_ori", "leaic_match_type") |>
  match_agency_source(lear_lookup, "lear_ori", "lear_match_type") |>
  # no aggressive key: CDE lacks many agencies
  match_agency_source(
    crime_lookup,
    "crime_ori",
    "crime_match_type",
    fallback = FALSE,
    prefix = TRUE,
    prefix_skip = c("leaic_ori", "lear_ori")
  ) |>
  match_agency_source(hifld_lookup, "hifld_county_fips", "hifld_match_type", prefer_agency_level = FALSE, prefix = FALSE) |>
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
    # tier outranks roster; ties by recency: crime 2025, lear 2016, leaic 2012.
    # An exact match that could not tell two ORIs apart yields to another
    # roster's unique full-name match, but still beats the aggressive key
    # prefix ties go to fewer added letters
    crime_rank = match_tier_rank(crime_match_type) + 1.5 * (crime_ori_ambiguous & crime_match_type == "exact_state_county_agency_name") + coalesce(crime_ori_prefix_extra, 0L) / 1000,
    lear_rank = match_tier_rank(lear_match_type) + 1.5 * (lear_ori_ambiguous & lear_match_type == "exact_state_county_agency_name") + coalesce(lear_ori_prefix_extra, 0L) / 1000,
    leaic_rank = match_tier_rank(leaic_match_type) + 1.5 * (leaic_ori_ambiguous & leaic_match_type == "exact_state_county_agency_name") + coalesce(leaic_ori_prefix_extra, 0L) / 1000,
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
    ),
    # the published ORI inherits the ambiguity of the roster it came from
    ori_ambiguous = case_when(
      ori_source == "crime" ~ crime_ori_ambiguous,
      ori_source == "lear" ~ lear_ori_ambiguous,
      ori_source == "leaic" ~ leaic_ori_ambiguous,
      TRUE ~ FALSE
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
  # a manual ORI overrides the rosters; an agreeing roster keeps the credit
  mutate(
    manual_ori = coalesce(manual_ori_specific, manual_ori_general),
    ori_source = if_else(
      !is.na(manual_ori) & coalesce(ORI9 != manual_ori, TRUE),
      "manual",
      ori_source
    ),
    ORI9 = coalesce(manual_ori, ORI9)
  ) |>
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
    leaic_match_type,
    leaic_ori_ambiguous,
    lear_ori,
    lear_name,
    lear_county_fips,
    lear_match_type,
    lear_ori_ambiguous,
    crime_ori,
    crime_name,
    crime_county_fips,
    crime_match_type,
    crime_ori_ambiguous,
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
  "data/intermediate/match-agency-identifiers.parquet"
)
