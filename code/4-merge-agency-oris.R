library(tidyverse)
library(arrow)

source("code/functions.R")

# ORIs and LEAIC's FIPS codes are annotations on an agreement, not inputs to any
# geometry match, so they are merged here at the end from the parquets written
# by the 1-read-* scripts, then joined onto the shaped dataset by agreement_id
# in 5-format-agreements-dataset.R.

# every source matches the same way: exact state + county + agency name,
# preferring an identical full name when the aggressive agency_key collides
# (Mahanoy City PD vs Mahanoy Township PD both key as "mahanoy"), then falling
# back to a state-wide name match when the sheet's county is missing, a typo, or
# disagrees with the source — accepted only when the name identifies exactly one
# ORI in the state
match_agency_source <- function(
  agencies,
  lookup,
  ori_col,
  match_type_col,
  fallback = TRUE
) {
  value_cols <- setdiff(
    names(lookup),
    c("state_key", "county_key", "agency_key", "source_fullname_key")
  )

  exact <- agencies |>
    select(-any_of(c(value_cols, match_type_col))) |>
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

  if (!fallback) {
    return(arrange(exact, agreement_id))
  }

  state_unique <- lookup |>
    group_by(state_key, agency_key) |>
    filter(n_distinct(.data[[ori_col]]) == 1) |>
    slice_head(n = 1) |>
    ungroup()

  bind_rows(
    exact |> filter(!is.na(.data[[match_type_col]])),
    exact |>
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

# one candidate row per key, so a join can only ever multiply on the keys we
# deliberately match on
dedupe_lookup <- function(x) {
  x |>
    group_by(state_key, county_key, agency_key, source_fullname_key) |>
    slice_head(n = 1) |>
    ungroup()
}

leaic_lookup <- arrow::read_parquet("data/leaic.parquet") |>
  select(
    state_key,
    county_key,
    agency_key,
    source_fullname_key = leaic_fullname_key,
    ORI9,
    FSTATE,
    FCOUNTY,
    FPLACE,
    leaic_name,
    leaic_agency_type,
    leaic_subtype1,
    leaic_subtype2,
    leaic_comment
  ) |>
  dedupe_lookup()

lear_lookup <- arrow::read_parquet("data/lear.parquet") |>
  filter(!is.na(ORI9)) |>
  select(
    state_key,
    county_key,
    agency_key,
    source_fullname_key = lear_fullname_key,
    lear_ori = ORI9,
    lear_name,
    lear_county_fips
  ) |>
  dedupe_lookup()

crime_lookup <- arrow::read_parquet("data/crime.parquet") |>
  select(
    state_key,
    county_key,
    agency_key,
    source_fullname_key = crime_fullname_key,
    crime_ori,
    crime_agency_name,
    crime_county_fips
  ) |>
  dedupe_lookup()

# HIFLD contributes no ORI, but its station addresses give an independent
# county per agency for the same cross-check the other sources feed
hifld_lookup <- arrow::read_parquet("data/hifld_law_enforcement.parquet") |>
  filter(!is.na(county_fips)) |>
  select(
    state_key,
    county_key,
    agency_key,
    source_fullname_key = fullname_key,
    hifld_county_fips = county_fips
  ) |>
  dedupe_lookup()

# a cleaned agency name that several real agencies share cannot be trusted to
# identify a record, so the FPLACE confirmation in 5-format only applies when
# every roster agrees the name identifies at most one agency statewide. Counted
# from the raw rosters: the deduped lookups would collapse duplicate-ORI
# records (Miami PD carries two ORIs in LEAIC) that must count as collisions
roster_key_unique_tbl <- bind_rows(
  arrow::read_parquet("data/leaic.parquet") |>
    distinct(state_key, agency_key, id = ORI9),
  arrow::read_parquet("data/lear.parquet") |>
    filter(!is.na(ORI9)) |>
    distinct(state_key, agency_key, id = ORI9),
  arrow::read_parquet("data/crime.parquet") |>
    distinct(state_key, agency_key, id = crime_ori),
  arrow::read_parquet("data/hifld_law_enforcement.parquet") |>
    distinct(state_key, agency_key, id = fullname_key),
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

agencies_all <- arrow::read_parquet("data/agencies_all.parquet") |>
  normalize_agencies_all() |>
  select(-any_of("ori_source")) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(agency),
    fullname_key = norm_ori_fullname(agency)
  ) |>
  match_agency_source(leaic_lookup, "ORI9", "leaic_match_type") |>
  match_agency_source(lear_lookup, "lear_ori", "lear_match_type") |>
  match_agency_source(
    crime_lookup,
    "crime_ori",
    "crime_match_type",
    fallback = FALSE
  ) |>
  match_agency_source(
    hifld_lookup,
    "hifld_county_fips",
    "hifld_match_type"
  ) |>
  select(-any_of("roster_key_unique")) |>
  left_join(roster_key_unique_tbl, by = c("state_key", "agency_key")) |>
  mutate(
    # a key absent from every roster has no collision evidence either way;
    # treat it as not-unique so it can never certify a confirmation
    roster_key_unique = coalesce(roster_key_unique, FALSE)
  ) |>
  mutate(
    # sources naming the same agency with different ORIs means at least one
    # match is wrong. This runs after the geometry scripts have already written
    # their own needs_review, so the flag is kept as its own column and OR'd
    # back in by 5-format-agreements-dataset.R.
    ori_conflict = coalesce(
      (!is.na(ORI9) & !is.na(lear_ori) & ORI9 != lear_ori) |
        (!is.na(ORI9) & !is.na(crime_ori) & ORI9 != crime_ori) |
        (!is.na(lear_ori) & !is.na(crime_ori) & lear_ori != crime_ori),
      FALSE
    ),
    needs_review = needs_review | ori_conflict,
    # the shipped value comes from the most recently maintained source:
    # CDE roster (2025) over LEAR (2016) over LEAIC (2012)
    ori_source = case_when(
      !is.na(crime_ori) ~ "crime_lookup",
      !is.na(lear_ori) ~ "lear",
      !is.na(ORI9) ~ "leaic",
      TRUE ~ NA_character_
    ),
    ORI9 = coalesce(crime_ori, lear_ori, ORI9)
  ) |>
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
    -state_key,
    -county_key,
    -agency_key,
    -fullname_key,
    -manual_ori_specific,
    -manual_ori_general,
    -manual_ori
  )

arrow::write_parquet(agencies_all, "data/agencies_all.parquet")
