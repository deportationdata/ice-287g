library(tidyverse)
library(readxl)
library(arrow)

source("code/functions.R")

agency_files <- list.files(
  "sheets",
  pattern = "^participatingAgencies.*\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(agency_files) == 0) {
  stop("No participating agencies file found.")
}

folder_stamp <- sub(
  ".*sheets_(\\d{8}_\\d{6}).*",
  "\\1",
  agency_files
)

folder_time <- as.POSIXct(
  folder_stamp,
  format = "%Y%m%d_%H%M%S",
  tz = "UTC"
)

latest_agency_file <- agency_files[which.max(folder_time)]

participating_agencies <- read_excel(latest_agency_file)

agencies_all <- participating_agencies |>
  transmute(
    state = str_to_title(str_trim(STATE)),
    county = str_to_title(str_trim(COUNTY)),
    agency = str_squish(`LAW ENFORCEMENT AGENCY`),
    signed = as.character(SIGNED),
    support_type = str_squish(`SUPPORT TYPE`),
    type_clean = str_to_lower(str_trim(TYPE)),
    support_clean = str_to_lower(str_trim(`SUPPORT TYPE`)),
    has_addendum = !(is.na(ADDENDUM) | ADDENDUM %in% c("", "NA")),
    moa_pending = str_detect(str_to_lower(str_trim(MOA)), "pending")
  ) |>
  mutate(
    state = if_else(
      agency == "Pittsburgh Police Department" & state == "New Hampshire",
      "Pennsylvania",
      state
    ),
    state = if_else(
      state == "Northern Mariana Islands",
      "Commonwealth of the Northern Mariana Islands",
      state
    )
  ) |>
  mutate(
    agency_level = case_when(
      type_clean %in% c("state agency", "state") ~ "state",
      type_clean == "county" ~ "county",
      type_clean == "municipality" ~ "municipal",
      TRUE ~ "unknown"
    ),
    is_university_agency = str_detect(
      str_to_lower(agency),
      "university|college|campus|board of trustees"
    ),
    geom_class = case_when(
      support_clean == "task force model" & is_university_agency ~
        "university_polygon",
      support_clean == "task force model" & agency_level == "state" ~
        "state_polygon",
      support_clean == "task force model" & agency_level == "county" ~
        "county_polygon",
      support_clean == "task force model" & agency_level == "municipal" ~
        "municipal_polygon",
      support_clean %in% c("jail enforcement model", "warrant service officer") ~
        "facility_point",
      TRUE ~ "unknown"
    ),
    geom_class = if_else(
      state == "Tennessee" & str_detect(str_to_lower(agency), "\\bconstables?\\b"),
      "county_polygon",
      geom_class
    )
  ) |>
  add_count(state, agency, name = "agency_count") |>
  mutate(
    needs_review = case_when(
      geom_class == "unknown" ~ TRUE,
      has_addendum ~ TRUE,
      moa_pending ~ TRUE,
      agency_count > 1 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  select(
    state,
    county,
    agency,
    signed,
    support_type,
    agency_level,
    geom_class,
    needs_review,
    support_clean,
    has_addendum,
    moa_pending
  )

arrow::write_parquet(agencies_all, "data/agencies_all.parquet")
