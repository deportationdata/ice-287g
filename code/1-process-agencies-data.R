library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(tigris)
library(purrr)
library(stringdist)
library(arrow)
library(tidylog)

source("code/functions.R")

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# data loading -----------------------------------------------------------

participating_agencies <-
  read_excel(
    "sheets/sheets_20260421_173735/participatingAgencies04212026am 12.15.07 AM.xlsx"
  )

load("data/35158-0001-Data.rda")
LEAIC <- da35158.0001 |> as_tibble()

hifld <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-local-law-enforcement-facilities.parquet"
)
hifld_prisons <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-prisons.parquet"
)
jails_prisons <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/jails_prisons.parquet"
)
crime_data <- arrow::read_parquet(
  "data/crime-data-all-states.parquet"
)

facilities <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/facilities-latest-sf.parquet"
)

university_boundaries <- st_read(
  "data/colleges-and-universities-campuses/CollegeUniversityCampuses.shp"
)

state_xwalk <- tibble(
  state_abbr = state.abb,
  state_full = state.name
) |>
  bind_rows(tibble(state_abbr = "DC", state_full = "District Of Columbia"))

agencies_all <- participating_agencies |>
  mutate(
    state = str_to_title(str_trim(STATE)),
    county = str_to_title(str_trim(COUNTY)),
    type_clean = str_to_lower(str_trim(TYPE)),
    support_clean = str_to_lower(str_trim(`SUPPORT TYPE`)),
    has_addendum = !(is.na(ADDENDUM) | ADDENDUM %in% c("", "NA")),
    moa_pending = str_detect(str_to_lower(str_trim(MOA)), "pending")
  ) |>
  # fix data error - Pittsburgh is in Pennsylvania, not New Hampshire
  mutate(
    state = if_else(
      `LAW ENFORCEMENT AGENCY` == "Pittsburgh Police Department" &
        state == "New Hampshire",
      "Pennsylvania",
      state
    ),
    # expand territory name to match collapsed county polygon
    state = if_else(
      state == "Northern Mariana Islands",
      "Commonwealth of the Northern Mariana Islands",
      state
    )
  ) |>
  group_by(state, `LAW ENFORCEMENT AGENCY`) |> # TODO: I don't think group_by is doing anythign here - there are no aggregation functions in the mutate - remove if so?
  mutate(
    agency_level = case_when(
      type_clean %in% c("state agency", "state") ~ "state",
      type_clean == "county" ~ "county",
      type_clean == "municipality" ~ "municipal",
      TRUE ~ "unknown"
    ),

    is_university_agency = str_detect(
      str_to_lower(`LAW ENFORCEMENT AGENCY`),
      "university|college|campus|board of trustees"
    ),

    geom_class = case_when(
      support_clean == "task force model" &
        is_university_agency ~ "university_polygon",
      support_clean == "task force model" &
        agency_level == "state" ~ "state_polygon",
      support_clean == "task force model" &
        agency_level == "county" ~ "county_polygon",
      support_clean == "task force model" &
        agency_level == "municipal" ~ "municipal_polygon",
      support_clean %in%
        c(
          "jail enforcement model",
          "warrant service officer"
        ) ~ "facility_point",
      TRUE ~ "unknown"
    ),

    needs_review = case_when(
      geom_class == "unknown" ~ TRUE,
      has_addendum ~ TRUE,
      moa_pending ~ TRUE,
      n() > 1 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  ungroup()

# source lookup tables ---------------------------------------------------

leaic_tbl <- LEAIC |>
  transmute(
    leaic_state = str_to_title(str_squish(STATENAME)),
    leaic_county = str_to_title(str_squish(COUNTYNAME)),
    leaic_name = str_squish(NAME),
    FSTATE,
    FCOUNTY,
    FPLACE,
    ORI9,
    AGCYTYPE,
    SUBTYPE1,
    SUBTYPE2,
    COMMENT
  ) |>
  mutate(
    state_key = norm_state(leaic_state),
    county_key = norm_place(leaic_county),
    agency_key_src = norm_key(leaic_name)
  )

hifld_tbl <- bind_rows(
  hifld |>
    transmute(
      src_dataset = "hifld",
      src_id = as.character(hifld_id),
      src_name = str_squish(name),
      src_address = address,
      src_city = str_squish(city),
      src_state = str_squish(state),
      src_zip = zip,
      src_type = type,
      src_status = status,
      src_population = NA_real_,
      src_hold_72 = NA,
      src_latitude = latitude,
      src_longitude = longitude,
      src_date = date
    ),
  hifld_prisons |>
    transmute(
      src_dataset = "hifld_prisons",
      src_id = as.character(hifld_id),
      src_name = str_squish(name),
      src_address = address,
      src_city = str_squish(city),
      src_state = str_squish(state),
      src_zip = zip,
      src_type = type,
      src_status = status,
      src_population = population,
      src_hold_72 = NA,
      src_latitude = latitude,
      src_longitude = longitude,
      src_date = date
    ),
  jails_prisons |>
    transmute(
      src_dataset = "jails_prisons",
      src_id = as.character(bjs_facility_ID),
      src_name = str_squish(name),
      src_address = address,
      src_city = str_squish(city),
      src_state = str_squish(state),
      src_zip = zip,
      src_type = NA_character_,
      src_status = NA_character_,
      src_population = NA_real_,
      src_hold_72 = hold_72,
      src_latitude = NA_real_,
      src_longitude = NA_real_,
      src_date = date
    )
) |>
  left_join(state_xwalk, by = c("src_state" = "state_abbr")) |>
  mutate(
    state_key = norm_state(src_state),
    county_key = norm_place(src_city),
    agency_key_src = norm_key(src_name)
  ) |>
  select(-state_full)

crime_lookup <- crime_data |>
  transmute(
    ori = str_squish(ori),
    crime_lat = latitude,
    crime_lon = longitude,
    agency_name = str_squish(agency_name),
    agency_type = agency_type_name,
    nibrs_start = nibrs_start_date,
    state_abbr = str_squish(state_abbr)
  ) |>
  distinct(ori, .keep_all = TRUE)

manual_points <- read_csv("data/manual-facility-points.csv")
