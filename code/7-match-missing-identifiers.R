# Agreements missing an ORI or FIPS code -> data/intermediate/match-missing-identifiers.parquet
library(tidyverse)

required_fips <- function(match_layer, state_fips, county_fips, place_fips) {
  case_when(
    match_layer == "state" ~ state_fips,
    match_layer %in% c("county", "university", "facility") ~ county_fips,
    match_layer %in% c("municipal", "pa_constable") ~ place_fips,
    TRUE ~ NA_character_
  )
}

agreement_fips <- arrow::read_parquet("data/intermediate/match-all-features.parquet") |>
  as.data.frame() |>
  mutate(
    has_fips = !is.na(
      required_fips(match_layer, state_fips, county_fips, place_fips)
    )
  ) |>
  # an agreement can have several feature rows, one per matched layer
  summarize(has_fips = any(has_fips), .by = agreement_id)

agreements <- arrow::read_parquet("data/intermediate/agreements.parquet") |>
  # deliberate: the report is a to-do list for the live program, so active only
  filter(status == "active")

agreement_identifiers <- arrow::read_parquet(
  "data/intermediate/match-agency-identifiers.parquet"
)

missing_identifiers <- agreements |>
  left_join(
    agreement_identifiers |> select(agreement_id, ORI9, ori_source),
    by = "agreement_id"
  ) |>
  left_join(agreement_fips, by = "agreement_id") |>
  mutate(
    has_ori = !is.na(ORI9) & str_squish(ORI9) != "",
    # agreements with no feature rows count as missing, not dropped
    has_fips = coalesce(has_fips, FALSE),
    missing_identifier_type = case_when(
      !has_ori & !has_fips ~ "missing_both",
      !has_ori ~ "missing_ori",
      !has_fips ~ "missing_fips",
      TRUE ~ "complete"
    )
  ) |>
  filter(missing_identifier_type != "complete") |>
  select(
    agreement_id,
    state,
    county,
    agency,
    jurisdiction_level,
    support_type,
    geom_class,
    ORI9,
    ori_source,
    missing_identifier_type
  )

arrow::write_parquet(missing_identifiers, "data/intermediate/match-missing-identifiers.parquet")
