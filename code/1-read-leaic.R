library(tidyverse)
library(arrow)

source("code/functions.R")

# Law Enforcement Agency Identifiers Crosswalk (ICPSR 35158, 2012)
load("inputs/35158-0001-Data.rda")

leaic <- da35158.0001 |>
  as_tibble() |>
  transmute(
    name = str_squish(NAME),
    ori = str_squish(ORI9),
    # FSTATE/FCOUNTY/FPLACE are labelled values ("(01) Alabama"), so extract
    # and zero-pad the digits: as.character would keep the label text
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
  # LEAIC codes missing ORIs as "-1"; drop those rows along with blank ORIs
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
