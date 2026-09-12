# Every identity ever observed in an archived sheet, with first/last appearance
# and removal window -> data/appearance-index.parquet
# Identity is (state, agency, support type, signed), normalized exactly as
# 1-read-agreements.R cleans the sheet so both sides of the join land alike.

library(tidyverse)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/state-xwalk.parquet")

appearance_files <- list.files(
  "sheets",
  pattern = "\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
# pending lists are a different population
appearance_files <- appearance_files[
  !str_detect(basename(appearance_files), regex("^pending", ignore_case = TRUE))
]

appearance_times <- appearance_seen_at(appearance_files)
folder_timed <- str_detect(appearance_files, "sheets_\\d{8}_\\d{6}")

appearance_parsed <- purrr::pmap(
  list(appearance_files, appearance_times, folder_timed),
  function(path, seen, from_folder) {
    rows <- read_appearance_rows(path)
    if (is.null(rows) || nrow(rows) == 0 || is.na(seen)) return(NULL)
    # ICE misdates filenames around New Year, and a list cannot predate its
    # newest signing, so floor filename-derived times at max(signed)
    if (!from_folder) {
      seen <- max(seen, as.POSIXct(max(rows$signed), tz = "UTC"))
    }
    mutate(rows, seen_at = seen)
  }
)
appearance_rows <- purrr::list_rbind(appearance_parsed)

message(sprintf(
  "appearance index: %d candidate sheets, %d parsed, %d rows",
  length(appearance_files),
  sum(!purrr::map_lgl(appearance_parsed, is.null)),
  nrow(appearance_rows)
))

# state goes through the same squish/snap pipeline as the agreements build so
# typo'd states land on the same key
raw_identities <- appearance_rows |>
  group_by(raw_state, raw_agency, raw_support, raw_type, raw_county,
           raw_moa, signed) |>
  summarize(
    first_appeared = min(seen_at),
    last_appeared = max(seen_at),
    n_obs = n(),
    .groups = "drop"
  ) |>
  mutate(
    state_t = str_to_title(str_squish(raw_state)),
    state_t = snap_state_name(state_t, state_xwalk$state_full),
    state_t = if_else(
      state_t == "Northern Mariana Islands",
      "Commonwealth of the Northern Mariana Islands",
      state_t
    ),
    state_t = if_else(
      state_t == "District Of Columbia",
      "District of Columbia",
      state_t
    ),
    key_state = appearance_norm(state_t),
    key_agency = appearance_norm(raw_agency),
    key_support = norm_support_key(raw_support)
  )

# display fields keep the newest raw spelling, so removed rows read as ICE
# last printed them
appearance_index <- raw_identities |>
  arrange(last_appeared) |>
  group_by(key_state, key_agency, key_support, signed) |>
  summarize(
    first_appeared = min(first_appeared),
    last_appeared = max(last_appeared),
    n_obs = sum(n_obs),
    raw_state_last = last(raw_state),
    raw_agency_last = last(raw_agency),
    raw_support_last = last(raw_support),
    raw_type_last = last(raw_type),
    raw_county_last = last(raw_county),
    raw_moa_last = last(raw_moa),
    .groups = "drop"
  )

latest_time <- max(appearance_rows$seen_at)
snapshot_times <- sort(unique(appearance_rows$seen_at))

appearance_index <- appearance_index |>
  mutate(
    is_current = last_appeared == latest_time,
    # last seen at last_appeared, gone by the next archived snapshot
    removed_by = if_else(
      is_current,
      as.POSIXct(NA),
      snapshot_times[
        pmin(findInterval(last_appeared, snapshot_times) + 1,
             length(snapshot_times))
      ]
    )
  )

# ICE's silent edits make an identity vanish while the agreement lives on
current <- appearance_index |> filter(is_current)

rename_match <- appearance_index |>
  filter(!is_current) |>
  select(key_state, key_agency, key_support, signed) |>
  inner_join(
    current |> select(key_state, key_support, signed,
                      current_agency = key_agency),
    by = c("key_state", "key_support", "signed"),
    relationship = "many-to-many"
  ) |>
  filter(
    key_agency != current_agency,
    stringdist::stringdist(key_agency, current_agency, method = "osa") <= 3 |
      str_detect(current_agency, fixed(key_agency)) |
      str_detect(key_agency, fixed(current_agency))
  ) |>
  distinct(key_state, key_agency, key_support, signed)

resign_match <- appearance_index |>
  filter(!is_current) |>
  semi_join(current, by = c("key_state", "key_agency", "key_support"))

switch_match <- appearance_index |>
  filter(!is_current) |>
  semi_join(current, by = c("key_state", "key_agency"))

appearance_index <- appearance_index |>
  mutate(
    removal_flag = case_when(
      is_current ~ NA_character_,
      paste(key_state, key_agency, key_support, signed) %in%
        paste(rename_match$key_state, rename_match$key_agency,
              rename_match$key_support, rename_match$signed) ~
        "possible_rename",
      paste(key_state, key_agency, key_support) %in%
        paste(resign_match$key_state, resign_match$key_agency,
              resign_match$key_support) ~ "possible_resign",
      key_state %in% switch_match$key_state &
        paste(key_state, key_agency) %in%
          paste(switch_match$key_state, switch_match$key_agency) ~
        "model_switch",
      TRUE ~ NA_character_
    )
  )

message(sprintf(
  "identities: %d total, %d current, %d removed (%d flagged as look-alikes)",
  nrow(appearance_index),
  sum(appearance_index$is_current),
  sum(!appearance_index$is_current),
  sum(!is.na(appearance_index$removal_flag))
))

arrow::write_parquet(appearance_index, "data/appearance-index.parquet")
