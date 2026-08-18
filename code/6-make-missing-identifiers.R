library(tidyverse)

# a geographic identifier comes from the matched geometry, so what counts as
# missing depends on the layer: a state agreement needs a state code, a county,
# facility or university agreement a county, a municipal or constable agreement
# a municipality
required_fips <- function(match_layer, state_fips, county_fips, place_fips) {
  case_when(
    match_layer == "state" ~ state_fips,
    match_layer %in% c("county", "university", "facility") ~ county_fips,
    match_layer %in% c("municipal", "pa_constable") ~ place_fips,
    TRUE ~ NA_character_
  )
}

# plain arrow read: the geometry blob is dead weight for this audit
agreement_fips <- arrow::read_parquet("data/all_agreements_sf.parquet") |>
  as.data.frame() |>
  mutate(
    has_fips = !is.na(
      required_fips(match_layer, state_fips, county_fips, place_fips)
    )
  ) |>
  # an agreement may match in several layers, and counts as having FIPS if any
  # one feature row carries the code its layer requires
  summarize(has_fips = any(has_fips), .by = agreement_id)

agreements <- arrow::read_parquet("data/agreements.parquet")

agreement_identifiers <- arrow::read_parquet(
  "data/agreement-identifiers.parquet"
)

missing_identifiers <- agreements |>
  left_join(
    agreement_identifiers |> select(agreement_id, ORI9, ori_source),
    by = "agreement_id"
  ) |>
  left_join(agreement_fips, by = "agreement_id") |>
  mutate(
    # whitespace-only ORIs count as missing
    has_ori = !is.na(ORI9) & str_squish(ORI9) != "",
    # agreements with no feature rows count as missing, not dropped
    has_fips = coalesce(has_fips, FALSE),
    # missing_both is tested first so the branches are mutually exclusive
    missing_identifier_type = case_when(
      !has_ori & !has_fips ~ "missing_both",
      !has_ori ~ "missing_ori",
      !has_fips ~ "missing_fips",
      TRUE ~ "complete"
    )
  ) |>
  # exceptions only: complete agreements are filtered out, not annotated
  filter(missing_identifier_type != "complete") |>
  select(
    agreement_id,
    state,
    county,
    agency,
    agency_level,
    support_type,
    geom_class,
    ORI9,
    ori_source,
    missing_identifier_type
  )

arrow::write_parquet(missing_identifiers, "data/missing-identifiers.parquet")
