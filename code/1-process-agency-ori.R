library(tidyverse)
library(arrow)

source("code/functions.R")

fuzzy_max_dist <- 0.12

agencies_all <- arrow::read_parquet("data/agencies_all.parquet") |>
  normalize_agencies_all()

# LEAIC crosswalk (ICPSR 35158): authoritative ORI source -----------------

load("inputs/35158-0001-Data.rda")

# partof_rank/report_rank order duplicate-name rows so the parent agency
# (headquarters) ORI wins over troop/substation ORIs that share the name
leaic_lookup <-
  da35158.0001 |>
  as_tibble() |>
  transmute(
    leaic_name = str_squish(as.character(NAME)),
    ORI9 = str_squish(as.character(ORI9)),
    FSTATE,
    FCOUNTY,
    FPLACE,
    leaic_county = str_squish(as.character(COUNTYNAME)),
    state_key = norm_state(str_squish(as.character(STATENAME))),
    county_key = norm_ori_county(leaic_county),
    agency_key = norm_ori_agency(leaic_name),
    fullname_key = norm_ori_fullname(leaic_name),
    partof_rank = if_else(str_detect(as.character(PARTOF), "^\\(0"), 0L, 1L),
    report_rank = if_else(
      str_detect(as.character(REPORT_FLAG), "^\\(1"),
      0L,
      1L
    )
  ) |>
  # "-1" is LEAIC's missing-ORI code
  filter(
    !is.na(ORI9),
    !ORI9 %in% c("", "-1"),
    !is.na(agency_key),
    agency_key != ""
  ) |>
  distinct()

# LEAIC often abbreviates the state inside agency names ("FL FISH AND
# WILDLIFE CONSERVATION COMMISSION", "MO DEPT OF CORRECTIONS"); add a second
# lookup row keyed on the expanded spelling so those join the ICE sheets'
# full state names. Keep original key because a leading abbreviation
# is not always the state ("LA SALLE PARISH" is not "LOUISIANA
# SALLE")
state_abbrev <- tibble(
  state_key = norm_state(c(state.name, "District of Columbia")),
  state_full = str_to_upper(c(state.name, "District of Columbia")),
  state_abb = c(state.abb, "DC")
)

leaic_lookup <- bind_rows(
  leaic_lookup,
  leaic_lookup |>
    inner_join(state_abbrev, by = "state_key") |>
    filter(str_detect(leaic_name, paste0("^", state_abb, "\\b"))) |>
    mutate(
      expanded_name = str_replace(
        leaic_name,
        paste0("^", state_abb, "\\b"),
        state_full
      ),
      agency_key = norm_ori_agency(expanded_name),
      fullname_key = norm_ori_fullname(expanded_name)
    ) |>
    select(-state_full, -state_abb, -expanded_name)
) |>
  distinct()

# one typo ("Kenawha") or a dropped word ("Pearl" for Pearl River) still
# counts as the same county
county_keys_close <- function(a, b) {
  !is.na(a) &
    !is.na(b) &
    (stringdist::stringdist(a, b, method = "osa") <= 1 |
      stringdist::stringdist(a, b, method = "jw", p = 0.1) <= 0.12)
}

# state-level matcher for rows whose county is missing or misspelled
match_state_level <- function(unmatched, lookup, key_col) {
  joined <- unmatched |>
    inner_join(
      lookup,
      by = c("state_key", key_col),
      suffix = c("", "_leaic"),
      relationship = "many-to-many"
    ) |>
    mutate(county_close = county_keys_close(county_key, county_key_leaic))

  if (key_col == "fullname_key") {
    joined <- joined |> mutate(fullname_equal = TRUE)
  } else {
    # a pure suffix difference ("West Virginia State Police Department" vs
    # "WEST VIRGINIA STATE POLICE") still confirms identity; a divergence
    # mid-name ("Oak Grove Village PD" vs "OAK GROVE PD") does not
    joined <-
      joined |>
      mutate(
        fullname_equal = str_starts(fullname_key, fixed(fullname_key_leaic)) |
          str_starts(fullname_key_leaic, fixed(fullname_key))
      )
  }

  joined |>
    group_by(row_id) |>
    filter(
      (n_distinct(ORI9) == 1 &
        (all(is.na(county_key)) | any(county_close) | any(fullname_equal))) |
        any(county_close) |
        (sum(partof_rank == 0) == 1 &
          (all(is.na(county_key)) | any(fullname_equal & partof_rank == 0)))
    ) |>
    arrange(
      desc(county_close),
      desc(fullname_equal),
      partof_rank,
      report_rank,
      ORI9,
      .by_group = TRUE
    ) |>
    slice(1) |>
    ungroup()
}

# FBI crime-data ORI lookup: fallback ORI source --------------------------

crime_lookup <- arrow::read_parquet("inputs/crime-data-all-states.parquet") |>
  transmute(
    crime_ori = str_squish(ori),
    crime_agency_name = str_squish(agency_name),
    state_key = norm_state(state_name),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(agency_name)
  ) |>
  filter(
    !is.na(crime_ori),
    crime_ori != "",
    !is.na(agency_key),
    agency_key != ""
  )

crime_state <- crime_lookup |>
  group_by(state_key, agency_key) |>
  filter(n_distinct(crime_ori) == 1) |>
  slice(1) |>
  ungroup()

# match LEAIC in tiers: exact -> state-unique -> fuzzy rescue --------------

agencies_keyed <- agencies_all |>
  select(
    -any_of(c(
      "ORI9",
      "FSTATE",
      "FCOUNTY",
      "FPLACE",
      "leaic_name",
      "leaic_match_type",
      "crime_ori",
      "crime_agency_name",
      "crime_match_type",
      "ori_source"
    ))
  ) |>
  mutate(
    row_id = row_number(),
    state_key = norm_state(state),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(agency),
    fullname_key = norm_ori_fullname(agency)
  )

leaic_cols <- c("ORI9", "FSTATE", "FCOUNTY", "FPLACE", "leaic_name")

# several LEAIC rows can share a key ("MAHANOY CITY BOROUGH PD" and "MAHANOY
# TOWNSHIP POLICE" both key to "mahanoy"), so break ties by raw-name
# closeness to the ICE spelling, then parent-agency and reporting status
leaic_t1 <- agencies_keyed |>
  inner_join(
    leaic_lookup,
    by = c("state_key", "county_key", "agency_key"),
    relationship = "many-to-many"
  ) |>
  group_by(row_id) |>
  arrange(
    stringdist::stringdist(
      str_to_lower(agency),
      str_to_lower(leaic_name),
      method = "jw",
      p = 0.1
    ),
    partof_rank,
    report_rank,
    ORI9,
    .by_group = TRUE
  ) |>
  slice(1) |>
  ungroup() |>
  mutate(leaic_match_type = "exact_state_county_agency_name")

# full-name pass runs before the collapsed-key pass: "Melbourne Police
# Department" resolves on its full name even though the collapsed key
# "melbourne" collides with Melbourne Village
leaic_t2a <- agencies_keyed |>
  anti_join(leaic_t1, by = "row_id") |>
  match_state_level(leaic_lookup, "fullname_key") |>
  mutate(leaic_match_type = "exact_state_full_agency_name")

leaic_t2b <- agencies_keyed |>
  anti_join(leaic_t1, by = "row_id") |>
  anti_join(leaic_t2a, by = "row_id") |>
  match_state_level(leaic_lookup, "agency_key") |>
  mutate(leaic_match_type = "exact_state_agency_name")

fuzzy_candidates <- agencies_keyed |>
  anti_join(leaic_t1, by = "row_id") |>
  anti_join(leaic_t2a, by = "row_id") |>
  anti_join(leaic_t2b, by = "row_id") |>
  filter(!is.na(agency_key), agency_key != "") |>
  select(row_id, state, county, agency, state_key, county_key, agency_key) |>
  inner_join(
    leaic_lookup,
    by = "state_key",
    suffix = c("", "_leaic"),
    relationship = "many-to-many"
  ) |>
  mutate(
    match_dist = stringdist::stringdist(
      agency_key,
      agency_key_leaic,
      method = "jw",
      p = 0.1
    )
  ) |>
  filter(!is.na(match_dist), match_dist <= fuzzy_max_dist) |>
  mutate(
    county_close = county_keys_close(county_key, county_key_leaic),
    digits_ok = key_digits(agency_key) == key_digits(agency_key_leaic),
    key_osa = stringdist::stringdist(
      agency_key,
      agency_key_leaic,
      method = "osa"
    )
  ) |>
  group_by(row_id) |>
  mutate(
    best_unambiguous = n_distinct(ORI9[match_dist == min(match_dist)]) == 1 |
      sum(partof_rank[match_dist == min(match_dist)] == 0) == 1
  ) |>
  arrange(
    match_dist,
    desc(county_close),
    partof_rank,
    report_rank,
    ORI9,
    .by_group = TRUE
  ) |>
  distinct(ORI9, .keep_all = TRUE) |>
  slice_head(n = 3) |>
  mutate(cand_rank = row_number()) |>
  ungroup() |>
  mutate(
    dps_equiv = str_remove(agency_key, "publicsafety$") ==
      str_remove(agency_key_leaic, "publicsafety$"),
    accepted = cand_rank == 1 &
      best_unambiguous &
      digits_ok &
      ((county_close & match_dist <= 0.09 & key_osa <= 3) |
        (match_dist <= 0.02 & (county_close | is.na(county_key))) |
        (county_close & dps_equiv))
  )

leaic_t3 <- fuzzy_candidates |>
  filter(accepted) |>
  select(row_id, all_of(leaic_cols)) |>
  mutate(leaic_match_type = "fuzzy_state_agency_name")

leaic_matches <- bind_rows(
  leaic_t1 |> select(row_id, all_of(leaic_cols), leaic_match_type),
  leaic_t2a |> select(row_id, all_of(leaic_cols), leaic_match_type),
  leaic_t2b |> select(row_id, all_of(leaic_cols), leaic_match_type),
  leaic_t3
)

# match the crime lookup with the same exact tiers ------------------------

crime_cols <- c("crime_ori", "crime_agency_name")

crime_t1 <- agencies_keyed |>
  inner_join(
    crime_lookup,
    by = c("state_key", "county_key", "agency_key"),
    relationship = "many-to-many"
  ) |>
  group_by(row_id) |>
  arrange(
    stringdist::stringdist(
      str_to_lower(agency),
      str_to_lower(crime_agency_name),
      method = "jw",
      p = 0.1
    ),
    crime_ori,
    .by_group = TRUE
  ) |>
  slice(1) |>
  ungroup() |>
  mutate(crime_match_type = "exact_state_county_agency_name")

crime_t2 <- agencies_keyed |>
  anti_join(crime_t1, by = "row_id") |>
  inner_join(
    crime_state |> select(state_key, agency_key, all_of(crime_cols)),
    by = c("state_key", "agency_key")
  ) |>
  mutate(crime_match_type = "exact_state_agency_name")

crime_matches <- bind_rows(
  crime_t1 |> select(row_id, all_of(crime_cols), crime_match_type),
  crime_t2 |> select(row_id, all_of(crime_cols), crime_match_type)
)

manual_ori <- read_csv(
  "inputs/manual-agency-ori.csv",
  show_col_types = FALSE
) |>
  transmute(
    state_key = norm_state(state),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(agency),
    manual_ori = str_squish(ORI9)
  ) |>
  filter(!is.na(manual_ori), manual_ori != "") |>
  distinct(state_key, county_key, agency_key, .keep_all = TRUE)

# attach ORIs, preferring LEAIC, then the crime lookup, then manual --------

agencies_all <- agencies_keyed |>
  left_join(leaic_matches, by = "row_id") |>
  left_join(crime_matches, by = "row_id") |>
  left_join(manual_ori, by = c("state_key", "county_key", "agency_key")) |>
  mutate(
    ORI9 = coalesce(ORI9, crime_ori, manual_ori),
    ori_source = case_when(
      !is.na(leaic_match_type) ~ "leaic",
      !is.na(crime_match_type) ~ "crime_lookup",
      !is.na(manual_ori) ~ "manual",
      TRUE ~ NA_character_
    )
  ) |>
  select(
    -row_id,
    -state_key,
    -county_key,
    -agency_key,
    -fullname_key,
    -manual_ori
  )

arrow::write_parquet(agencies_all, "data/agencies_all.parquet")

fuzzy_candidates |>
  left_join(crime_matches, by = "row_id") |>
  select(
    state,
    county,
    agency,
    cand_rank,
    leaic_name,
    leaic_county,
    ORI9,
    match_dist,
    county_close,
    digits_ok,
    accepted,
    crime_ori
  ) |>
  arrange(state, agency, cand_rank) |>
  write_csv("data/agency_ori_matches_needing_review.csv")
