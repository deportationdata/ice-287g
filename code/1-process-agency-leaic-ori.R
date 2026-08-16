library(tidyverse)
library(arrow)

source("code/functions.R")

load("inputs/35158-0001-Data.rda")
LEAIC <- da35158.0001 |> as_tibble()

agencies_all <- arrow::read_parquet("data/agencies_all.parquet") |>
  normalize_agencies_all()

leaic_cols <- c(
  "ORI9",
  "FSTATE",
  "FCOUNTY",
  "FPLACE",
  "leaic_name",
  "leaic_agency_type",
  "leaic_subtype1",
  "leaic_subtype2",
  "leaic_comment"
)

leaic_lookup <- LEAIC |>
  transmute(
    leaic_state = str_to_title(str_squish(STATENAME)),
    leaic_county = str_to_title(str_squish(COUNTYNAME)),
    leaic_name = str_squish(NAME),
    ORI9 = str_squish(ORI9),
    FSTATE,
    FCOUNTY,
    FPLACE,
    leaic_agency_type = AGCYTYPE,
    leaic_subtype1 = SUBTYPE1,
    leaic_subtype2 = SUBTYPE2,
    leaic_comment = COMMENT
  ) |>
  filter(!is.na(ORI9), !ORI9 %in% c("", "-1")) |>
  mutate(
    state_key = norm_state(leaic_state),
    county_key = norm_ori_county(leaic_county),
    agency_key = norm_ori_agency(leaic_name),
    leaic_fullname_key = norm_ori_fullname(leaic_name)
  ) |>
  group_by(state_key, county_key, agency_key, leaic_fullname_key) |>
  slice_head(n = 1) |>
  ungroup()

agencies_keyed <- agencies_all |>
  select(-any_of(c(leaic_cols, "leaic_match_type"))) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_ori_county(county),
    agency_key = norm_ori_agency(agency),
    fullname_key = norm_ori_fullname(agency)
  )

# several agencies in a county can share the aggressive agency_key (Mahanoy
# City PD vs Mahanoy Township PD both key as "mahanoy"), so keep every
# candidate and prefer the one whose full name matches exactly
exact_matches <- agencies_keyed |>
  left_join(
    leaic_lookup |>
      select(
        state_key,
        county_key,
        agency_key,
        leaic_fullname_key,
        all_of(leaic_cols)
      ),
    by = c("state_key", "county_key", "agency_key"),
    relationship = "many-to-many"
  ) |>
  group_by(agreement_id) |>
  arrange(desc(leaic_fullname_key == fullname_key), ORI9, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    leaic_match_type = if_else(
      is.na(ORI9),
      NA_character_,
      "exact_state_county_agency_name"
    )
  ) |>
  select(-leaic_fullname_key)

# fall back to a state-wide name match when the sheet's county is missing,
# a typo, or disagrees with LEAIC — accepted only when the name identifies
# exactly one ORI in the state
leaic_state_unique <- leaic_lookup |>
  group_by(state_key, agency_key) |>
  filter(n_distinct(ORI9) == 1) |>
  slice_head(n = 1) |>
  ungroup()

fallback_matches <- exact_matches |>
  filter(is.na(leaic_match_type)) |>
  select(-all_of(leaic_cols)) |>
  left_join(
    leaic_state_unique |>
      select(state_key, agency_key, all_of(leaic_cols)),
    by = c("state_key", "agency_key")
  ) |>
  mutate(
    leaic_match_type = if_else(
      is.na(ORI9),
      NA_character_,
      "unique_state_agency_name"
    )
  )

agencies_all <- bind_rows(
  exact_matches |> filter(!is.na(leaic_match_type)),
  fallback_matches
) |>
  arrange(agreement_id) |>
  select(-state_key, -county_key, -agency_key, -fullname_key)

arrow::write_parquet(agencies_all, "data/agencies_all.parquet")
