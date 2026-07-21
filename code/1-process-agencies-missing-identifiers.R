library(dplyr)
library(arrow)

agencies_all <- arrow::read_parquet("data/agencies_all.parquet")

has_value <- function(x) {
  !is.na(x) & trimws(as.character(x)) != ""
}

agencies_missing_identifiers <- agencies_all |>
  mutate(
    has_ori = has_value(ORI9),
    has_fips = has_value(FSTATE) &
      has_value(FCOUNTY) &
      has_value(FPLACE),
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
