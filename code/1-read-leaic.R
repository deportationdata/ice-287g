# LEAIC agency/ORI crosswalk (ICPSR 35158, 2012) -> data/leaic.parquet
library(tidyverse)
library(arrow)

source("code/functions.R")

load("inputs/35158-0001-Data.rda")

leaic <- da35158.0001 |>
  as_tibble() |>
  transmute(
    name = str_squish(NAME),
    ori = str_squish(ORI9),
    # FSTATE/FCOUNTY/FPLACE are labelled values ("(01) Alabama"), not digits
    fstate = str_pad(str_extract(as.character(FSTATE), "[0-9]+"), 2, pad = "0"),
    fcounty = str_pad(str_extract(as.character(FCOUNTY), "[0-9]+"), 3, pad = "0"),
    county_fips = if_else(
      !is.na(fstate) & !is.na(fcounty),
      paste0(fstate, fcounty),
      NA_character_
    ),
    # place sentinels ("00000", "99xxx") pass through for 5-format to interpret
    place_fips = str_pad(str_extract(as.character(FPLACE), "[0-9]+"), 5, pad = "0"),
    state_key = norm_state(str_to_title(str_squish(STATENAME))),
    county_key = norm_ori_county(str_to_title(str_squish(COUNTYNAME))),
    agency_key = norm_ori_agency(name),
    fullname_key = norm_ori_fullname(name)
  ) |>
  # LEAIC codes a missing ORI as "-1"; that sentinel must never ship
  filter(!is.na(ori), !ori %in% c("", "-1")) |>
  select(
    state_key,
    county_key,
    agency_key,
    fullname_key,
    ori,
    name,
    county_fips,
    place_fips
  )

arrow::write_parquet(leaic, "data/leaic.parquet")
