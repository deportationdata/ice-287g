library(tidyverse)
library(arrow)

source("code/functions.R")

# Law Enforcement Agency Identifiers Crosswalk (ICPSR 35158, 2012)
load("inputs/35158-0001-Data.rda")

leaic <- da35158.0001 |>
  as_tibble() |>
  transmute(
    leaic_state = str_to_title(str_squish(STATENAME)),
    leaic_county = str_to_title(str_squish(COUNTYNAME)),
    leaic_name = str_squish(NAME),
    ORI9 = str_squish(ORI9),
    # labelled values ("(01) Alabama") become zero-padded FIPS codes
    FSTATE = str_pad(str_extract(as.character(FSTATE), "[0-9]+"), 2, pad = "0"),
    FCOUNTY = str_pad(str_extract(as.character(FCOUNTY), "[0-9]+"), 3, pad = "0"),
    FPLACE = str_pad(str_extract(as.character(FPLACE), "[0-9]+"), 5, pad = "0"),
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
  )

arrow::write_parquet(leaic, "data/leaic.parquet")
