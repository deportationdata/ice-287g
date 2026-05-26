library(tidyverse)
library(arrow)

source("code/functions.R")

load("data/35158-0001-Data.rda")
LEAIC <- da35158.0001 |> as_tibble()

agencies_all <- arrow::read_parquet("data/processed/agencies_all.parquet") |>
  normalize_agencies_all()

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
  mutate(
    state_key = norm_state(leaic_state),
    county_key = norm_place(leaic_county),
    agency_key = norm_key(leaic_name)
  ) |>
  filter(!is.na(ORI9), ORI9 != "") |>
  group_by(state_key, county_key, agency_key) |>
  slice_head(n = 1) |>
  ungroup()

agencies_all <- agencies_all |>
  select(
    -any_of(c(
      "ORI9",
      "FSTATE",
      "FCOUNTY",
      "FPLACE",
      "leaic_name",
      "leaic_agency_type",
      "leaic_subtype1",
      "leaic_subtype2",
      "leaic_comment",
      "leaic_match_type"
    ))
  ) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_place(county),
    agency_key = norm_key(agency)
  ) |>
  left_join(
    leaic_lookup |>
      select(
        state_key,
        county_key,
        agency_key,
        ORI9,
        FSTATE,
        FCOUNTY,
        FPLACE,
        leaic_name,
        leaic_agency_type,
        leaic_subtype1,
        leaic_subtype2,
        leaic_comment
      ),
    by = c("state_key", "county_key", "agency_key")
  ) |>
  mutate(
    leaic_match_type = if_else(
      is.na(ORI9),
      NA_character_,
      "exact_state_county_agency_name"
    )
  ) |>
  select(-state_key, -county_key, -agency_key)

arrow::write_parquet(agencies_all, "data/processed/agencies_all.parquet")
