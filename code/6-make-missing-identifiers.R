library(tidyverse)
library(arrow)

# an agreement's geographic identifier comes from its matched geometry, so
# what counts as "missing FIPS" depends on the layer: a state agreement needs
# a state code, a county or facility agreement a county, a municipal or
# constable agreement a municipality, a university a county. An agreement
# whose features all lack the layer's code has no usable identifier.
required_fips <- function(match_layer, state_fips, county_fips, place_fips) {
  case_when(
    match_layer == "state" ~ state_fips,
    match_layer %in% c("county", "university", "facility") ~ county_fips,
    match_layer %in% c("municipal", "pa_constable") ~ place_fips,
    TRUE ~ NA_character_
  )
}

agreement_fips <- arrow::read_parquet("data/all_agreements_sf.parquet") |>
  as.data.frame() |>
  mutate(
    has_fips = !is.na(
      required_fips(match_layer, state_fips, county_fips, place_fips)
    )
  ) |>
  summarize(has_fips = any(has_fips), .by = agreement_id)

agencies_missing_identifiers <-
  arrow::read_parquet("data/agencies_all.parquet") |>
  left_join(agreement_fips, by = "agreement_id") |>
  mutate(
    has_ori = !is.na(ORI9) & str_squish(ORI9) != "",
    has_fips = coalesce(has_fips, FALSE),
    missing_identifier_type = case_when(
      !has_ori & !has_fips ~ "missing_both",
      !has_ori ~ "missing_ori",
      !has_fips ~ "missing_fips",
      TRUE ~ "complete"
    )
  ) |>
  filter(missing_identifier_type != "complete") |>
  select(-has_ori, -has_fips)

arrow::write_parquet(
  agencies_missing_identifiers,
  "data/agencies_missing_ori_or_fips.parquet"
)
