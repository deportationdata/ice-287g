library(tidyverse)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/state-xwalk.parquet")
county_name_fixes <- read_csv("inputs/county-name-fixes.csv", col_types = "cccc") |>
  select(state, county, county_fixed)
signed_date_fixes <- read_csv("inputs/signed-date-fixes.csv", col_types = "cccDDc") |>
  select(state, agency, support_type, signed, signed_fixed)

agency_files <- list.files(
  "sheets",
  # ICE has shipped ParticipatingAgencies... with a capital P, so ignore case
  pattern = "^participatingAgencies.*\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
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

# built by 1-read-agreement-history.R, which must run first
if (!file.exists("data/appearance-index.parquet")) {
  stop(
    "data/appearance-index.parquet is missing; ",
    "run code/1-read-agreement-history.R first"
  )
}
appearance_index <- arrow::read_parquet("data/appearance-index.parquet")

participating_agencies <- readxl::read_excel(latest_agency_file)

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

# Removed agreements enter shaped like sheet rows so every cleaning step below
# applies to them; the page-table era carried no TYPE/COUNTY, hence NAs here.
removed_sheet_rows <- appearance_index |>
  filter(!is_current) |>
  transmute(
    STATE = raw_state_last,
    `LAW ENFORCEMENT AGENCY` = raw_agency_last,
    TYPE = raw_type_last,
    COUNTY = raw_county_last,
    `SUPPORT TYPE` = raw_support_last,
    SIGNED = as.POSIXct(signed, tz = "UTC"),
    MOA = NA_character_,
    ADDENDUM = NA_character_,
    moa_link = if_else(
      str_detect(raw_moa_last, "^https?://"),
      raw_moa_last,
      NA_character_
    ),
    addendum_link = NA_character_
  )

participating_agencies <- bind_rows(
  participating_agencies |> mutate(status = "active"),
  removed_sheet_rows |> mutate(status = "removed")
)

agreements <- participating_agencies |>
  transmute(
    status,
    # squish, not trim: the sheet embeds non-breaking spaces inside values
    state = str_to_title(str_squish(STATE)),
    # missing counties arrive as #N/A-style text, not blanks
    county = str_to_title(str_squish(COUNTY)),
    county = if_else(
      str_to_lower(county) %in% c("#na", "#n/a", "na", "n/a"),
      NA_character_,
      county
    ),
    agency = str_squish(`LAW ENFORCEMENT AGENCY`),
    signed = as.Date(SIGNED),
    # sentinel string, not NA: needs_review keys off this exact value
    moa = case_when(
      !is.na(moa_link) ~ moa_link,
      str_to_lower(str_trim(MOA)) == "link pending" ~ "pending",
      TRUE ~ NA_character_
    ),
    addendum = addendum_link,
    support_type = str_squish(`SUPPORT TYPE`),
    type_clean = str_to_lower(str_squish(TYPE)),
    support_clean = str_to_lower(str_squish(`SUPPORT TYPE`))
  ) |>
  # keys on cleaned values, so it must follow the title-casing pass above
  left_join(county_name_fixes, by = c("state", "county")) |>
  mutate(county = coalesce(county_fixed, county)) |>
  select(-county_fixed) |>
  mutate(
    state = snap_state_name(state, state_xwalk$state_full),
    state = if_else(
      state == "Northern Mariana Islands",
      "Commonwealth of the Northern Mariana Islands",
      state
    ),
    # str_to_title gives "District Of Columbia"; tigris and the xwalk use "of"
    state = if_else(
      state == "District Of Columbia",
      "District of Columbia",
      state
    )
  ) |>
  # keyed on the erroneous value, so a row no-ops once ICE fixes its sheet;
  # joins on cleaned values, so it must follow the passes above
  left_join(
    signed_date_fixes,
    by = c("state", "agency", "support_type", "signed")
  ) |>
  mutate(signed = coalesce(signed_fixed, signed)) |>
  select(-signed_fixed) |>
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
    # university outranks agency_level so campus agencies never fall through
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
    # only TN constables are county-level; PA has its own script and TX justice
    # precincts have no boundary layer, so they stay unmatched
    geom_class = case_when(
      str_detect(str_to_lower(agency), "\\bconstables?\\b") &
        state == "Tennessee" ~ "county_polygon",
      str_detect(str_to_lower(agency), "\\bconstables?\\b") &
        geom_class == "county_polygon" ~ "municipal_polygon",
      TRUE ~ geom_class
    ),
    # a sheriff or jail naming its own county is county-level whatever TYPE says
    geom_class = if_else(
      geom_class == "municipal_polygon" &
        is_exact_county_pattern(agency, county),
      "county_polygon",
      geom_class
    )
  ) |>
  # count within status, so removals never flip a current row's flag
  add_count(status, state, agency, name = "agency_count") |>
  mutate(
    needs_review = case_when(
      geom_class == "unknown" ~ TRUE,
      !is.na(addendum) ~ TRUE,
      moa == "pending" ~ TRUE,
      agency_count > 1 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  # positional over the sheet's row order; every downstream artifact joins on it
  mutate(agreement_id = row_number()) |>
  select(
    agreement_id,
    status,
    state,
    county,
    agency,
    agency_level,
    support_type,
    signed,
    moa,
    addendum,
    geom_class,
    needs_review
  ) |>
  # join on identity + signed first; signed-corrected rows fall back to
  # identity alone
  mutate(
    .key_state = appearance_norm(state),
    .key_agency = appearance_norm(agency),
    .key_support = norm_support_key(support_type)
  ) |>
  left_join(
    appearance_index |>
      select(key_state, key_agency, key_support, signed,
             first_appeared, last_appeared, removed_by, removal_flag),
    by = c(
      ".key_state" = "key_state",
      ".key_agency" = "key_agency",
      ".key_support" = "key_support",
      "signed" = "signed"
    ),
    relationship = "many-to-one"
  ) |>
  left_join(
    appearance_index |>
      group_by(key_state, key_agency, key_support) |>
      summarize(
        first_appeared_any = min(first_appeared),
        last_appeared_any = max(last_appeared),
        # a listed row keeps no removal metadata unless every variant is gone
        removed_by_any = if (any(is_current)) {
          as.POSIXct(NA)
        } else {
          max(removed_by)
        },
        removal_flag_any = if (any(is_current)) {
          NA_character_
        } else {
          removal_flag[which.max(last_appeared)]
        },
        .groups = "drop"
      ),
    by = c(
      ".key_state" = "key_state",
      ".key_agency" = "key_agency",
      ".key_support" = "key_support"
    ),
    relationship = "many-to-one"
  ) |>
  mutate(
    first_appeared = coalesce(first_appeared, first_appeared_any),
    last_appeared = coalesce(last_appeared, last_appeared_any),
    removed_by = coalesce(removed_by, removed_by_any),
    removal_flag = coalesce(removal_flag, removal_flag_any)
  ) |>
  select(
    -.key_state, -.key_agency, -.key_support,
    -first_appeared_any, -last_appeared_any,
    -removed_by_any, -removal_flag_any
  )

if (anyNA(agreements$first_appeared)) {
  warning(
    sum(is.na(agreements$first_appeared)),
    " agreement(s) missing from the appearance index; ",
    "every current row should at least match the latest sheet"
  )
}

# An unplaceable state would silently miss every state-keyed join. Warn rather
# than stop: a failure here aborts the daily workflow and drops the snapshot.
unknown_states <- setdiff(agreements$state, state_xwalk$state_full)
if (length(unknown_states) > 0) {
  warning(
    "State value(s) not in the xwalk even after snapping: ",
    paste(unknown_states, collapse = ", ")
  )
}

arrow::write_parquet(agreements, "data/agreements.parquet")
