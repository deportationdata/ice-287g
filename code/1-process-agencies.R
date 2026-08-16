library(tidyverse)
library(readxl)
library(arrow)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/state_xwalk.parquet")
county_name_fixes <- read_csv("inputs/county-name-fixes.csv", col_types = "ccc")

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

# the MOA / ADDENDUM urls live in embedded hyperlinks, not cell text
moa_col <- LETTERS[match("MOA", names(participating_agencies))]
addendum_col <- LETTERS[match("ADDENDUM", names(participating_agencies))]

if (is.na(moa_col) || is.na(addendum_col)) {
  stop(
    "Participating agencies sheet has no MOA/ADDENDUM column; layout changed?"
  )
}

sheet_links <- xlsx_hyperlinks(latest_agency_file)

participating_agencies <- participating_agencies |>
  # sheet rows sit one below the header row: data row i is sheet row i + 1
  mutate(excel_row = row_number() + 1L) |>
  left_join(
    sheet_links |>
      filter(col == moa_col) |>
      transmute(excel_row = row, moa_link = clean_moa_urls(url)),
    by = "excel_row"
  ) |>
  left_join(
    sheet_links |>
      filter(col == addendum_col) |>
      transmute(excel_row = row, addendum_link = clean_moa_urls(url)),
    by = "excel_row"
  ) |>
  select(-excel_row)

agencies_all <- participating_agencies |>
  transmute(
    state = str_to_title(str_trim(STATE)),
    county = str_to_title(str_trim(COUNTY)),
    county = if_else(
      str_to_lower(county) %in% c("#na", "#n/a", "na", "n/a"),
      NA_character_,
      county
    ),
    agency = str_squish(`LAW ENFORCEMENT AGENCY`),
    signed = as.Date(SIGNED),
    moa = case_when(
      !is.na(moa_link) ~ moa_link,
      str_to_lower(str_trim(MOA)) == "link pending" ~ "pending",
      TRUE ~ NA_character_
    ),
    addendum = addendum_link,
    support_type = str_squish(`SUPPORT TYPE`),
    type_clean = str_to_lower(str_trim(TYPE)),
    support_clean = str_to_lower(str_trim(`SUPPORT TYPE`))
  ) |>
  left_join(county_name_fixes, by = c("state", "county")) |>
  mutate(county = coalesce(county_fixed, county)) |>
  select(-county_fixed) |>
  mutate(
    state = snap_state_name(state, state_xwalk$state_full),
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
      support_clean %in%
        c("jail enforcement model", "warrant service officer") ~
        "facility_point",
      TRUE ~ "unknown"
    ),
    geom_class = if_else(
      state == "Tennessee" &
        str_detect(str_to_lower(agency), "\\bconstables?\\b"),
      "county_polygon",
      geom_class
    )
  ) |>
  add_count(state, agency, name = "agency_count") |>
  mutate(
    needs_review = case_when(
      geom_class == "unknown" ~ TRUE,
      !is.na(addendum) ~ TRUE,
      moa == "pending" ~ TRUE,
      agency_count > 1 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  mutate(agreement_id = row_number()) |>
  select(
    agreement_id,
    state,
    county,
    agency,
    signed,
    moa,
    addendum,
    support_type,
    agency_level,
    geom_class,
    needs_review,
    support_clean
  )

arrow::write_parquet(agencies_all, "data/agencies_all.parquet")
