library(tidyverse)
library(readxl)
library(arrow)
library(assertr)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/state_xwalk.parquet")

latest_agency_file <-
  tibble(
    path = list.files(
      "sheets",
      pattern = "^participatingAgencies.*\\.xlsx$",
      recursive = TRUE,
      full.names = TRUE
    )
  ) |>
  verify(
    length(path) > 0,
    description = "No participating agencies file found."
  ) |>
  mutate(
    folder_time = ymd_hms(str_extract(
      path,
      "sheets_(\\d{8}_\\d{6})",
      group = 1
    ))
  ) |>
  verify(
    !is.na(folder_time),
    description = "Could not parse a sheets_YYYYMMDD_HHMMSS timestamp from path"
  ) |>
  slice_max(folder_time, with_ties = FALSE) |>
  pull(path)

participating_agencies <- read_excel(latest_agency_file) |>
  add_moa_links(latest_agency_file) |>
  verify(
    str_detect(MOA, "pending") |
      (!is.na(moa_link) & str_starts(moa_link, "https://www.ice.gov")),
    description = "Some links to MOAs are missing"
  )

agencies_all <-
  participating_agencies |>
  transmute(
    state = str_to_title(str_squish(STATE)),
    county = str_to_title(str_squish(COUNTY)),
    agency = str_squish(`LAW ENFORCEMENT AGENCY`),
    support_type = str_squish(`SUPPORT TYPE`), # TODO: note I dropped support_clean, it didn't seem needed? just switched to using the uppercases throughout
    signed_date = coalesce(
      as.Date(suppressWarnings(as.numeric(SIGNED)), origin = "1899-12-30"),
      as.Date(str_replace(SIGNED, "/20026$", "/2026"), format = "%m/%d/%Y") # 6/30/20026 typo
    ),
    type_clean = str_to_lower(str_squish(TYPE)),
    has_addendum = !(is.na(ADDENDUM) | ADDENDUM %in% c("", "NA")),
    moa_pending = str_detect(str_to_lower(str_squish(MOA)), "pending"),
    moa_link
  ) |>
  verify(
    !is.na(signed_date) & between(signed_date, ymd("1996-01-01"), today() + 30),
    description = "SIGNED did not parse to a plausible date"
  ) |>
  mutate(
    state = if_else(
      agency == "Pittsburgh Police Department" & state == "New Hampshire", # TODO: this is actually the NH town of pittsburgh just mispelled -- can you drop this and fix elsewhere if needed? https://pittsburg-nh.gov/pittsburg-police-department/?
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
    state_raw = state,
    state = snap_state_name(state, state_xwalk$state_full),
    state_fuzzy_fixed = state != state_raw
  ) |>
  mutate(
    # TODO: why not call this type as they do?
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
    # TODO: should we make this readable for users, like location_type in "County", "State", "Municipality", "Facility"?
    geom_class = case_when(
      state == "Tennessee" &
        str_detect(str_to_lower(agency), "\\bconstables?\\b") ~
        "county_polygon",
      support_type == "Task Force Model" & is_university_agency ~
        "university_polygon",
      support_type == "Task Force Model" & agency_level == "state" ~
        "state_polygon",
      support_type == "Task Force Model" & agency_level == "county" ~
        "county_polygon",
      support_type == "Task Force Model" & agency_level == "municipal" ~
        "municipal_polygon",
      support_type %in%
        c("Jail Enforcement model", "Warrant Service Officer") ~
        "facility_point",
      TRUE ~ "unknown"
    ),
  ) |>
  add_count(state, agency, name = "agency_count") |>
  mutate(
    needs_review = case_when(
      geom_class == "unknown" ~ TRUE,
      has_addendum ~ TRUE,
      moa_pending ~ TRUE,
      agency_count > 1 ~ TRUE,
      state_fuzzy_fixed ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  select(
    state,
    county,
    agency,
    support_type,
    signed_date,
    agency_level,
    geom_class,
    has_addendum,
    moa_pending,
    moa_link,
    needs_review
  )

arrow::write_parquet(agencies_all, "data/agencies_all.parquet")
