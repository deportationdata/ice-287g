# Give every sheets/ and agreements/ snapshot folder one manifest.csv

library(tidyverse)

source("code/functions.R")

folder_stamp_iso <- function(folder) {
  m <- str_match(basename(folder), "_(\\d{8})_(\\d{6})$")
  if (is.na(m[1, 1])) {
    return(NA_character_)
  }
  format(
    as.POSIXct(paste0(m[1, 2], m[1, 3]), format = "%Y%m%d%H%M%S", tz = "UTC"),
    "%Y-%m-%dT%H:%M:%SZ"
  )
}

hash_file <- function(p) digest::digest(file = p, algo = "sha256")

convert_download_log <- function(folder) {
  log_path <- file.path(folder, "download_path_log.csv")
  log <- read_csv(log_path, col_types = cols(.default = "c"))
  stamp <- folder_stamp_iso(folder)
  rows <- tibble(
    saved_path = log$saved_path,
    file_hash = log$file_hash,
    url = coalesce(log[["url"]], log[["hyperlink"]]),
    retrieved_at = stamp,
    state = log[["state"]] %||% NA_character_,
    agency = log[["agency_name"]] %||% log[["agency"]] %||% NA_character_,
    original_filename = log[["original_filename"]],
    note = log[["document_type"]] %||% NA_character_
  )
  append_manifest(folder, rows)
  file.remove(log_path)
  cat("converted:", folder, "(", nrow(rows), "rows )\n")
}

reconstruct <- function(folder) {
  files <- list.files(folder, recursive = TRUE, full.names = TRUE)
  files <- files[basename(files) != "manifest.csv"]
  if (length(files) == 0) {
    return(invisible())
  }

  base <- basename(folder)
  rows <- tibble(
    saved_path = files,
    file_hash = vapply(files, hash_file, character(1)),
    url = NA_character_,
    retrieved_at = folder_stamp_iso(folder),
    state = NA_character_,
    agency = NA_character_,
    original_filename = basename(files),
    note = NA_character_
  )

  rel <- str_remove(files, paste0("^", folder, "/"))
  parts <- str_split(rel, "/")

  if (str_detect(base, "^agreements_\\d{8}_\\d{6}$")) {
    # dated scrape folders are laid out STATE/AGENCY/file
    rows$state <- map_chr(parts, \(p) {
      if (length(p) >= 3) p[1] else NA_character_
    })
    rows$agency <- map_chr(parts, \(p) {
      if (length(p) >= 3) str_replace_all(p[2], "_", " ") else NA_character_
    })
    rows$note <- "manifest reconstructed after the fact; this scraper run predates download logging"
  } else if (
    base %in% c("sheets_wayback_20260814", "agreements_wayback_20260814")
  ) {
    rows$retrieved_at <- "2026-08-14"
    rows$note <- "ingested by the 2026-08-14 Wayback backfill; url records where the bytes came from when known"
  } else if (base == "sheets_wayback_20260911") {
    rows$retrieved_at <- "2026-09-11"
    raw_ts <- str_match(basename(files), "^(\\d{14})\\.html$")[, 2]
    page_url <- "https://www.ice.gov/identify-and-arrest/287g"
    rows$url <- if_else(
      !is.na(raw_ts),
      sprintf("https://web.archive.org/web/%sid_/%s", raw_ts, page_url),
      NA_character_
    )
    rows$note <- case_when(
      !is.na(
        raw_ts
      ) ~ "raw Wayback capture fetched by code/0-acquire-wayback.R",
      TRUE ~ "recovered from appelson/Tracking_287g archived_data (wayback-sourced)"
    )
  } else if (base == "sheets_wayback_pre2018") {
    rows$retrieved_at <- "2026-09-12"
    raw_ts <- str_match(basename(files), "^ice_287g_(\\d{14})\\.html$")[, 2]
    # the fact sheet moved url after 2014
    era_url <- if_else(
      substr(raw_ts, 1, 4) <= "2014",
      "http://www.ice.gov/news/library/factsheets/287g.htm",
      "http://www.ice.gov/factsheets/287g"
    )
    rows$url <- if_else(
      !is.na(raw_ts),
      sprintf("https://web.archive.org/web/%sid_/%s", raw_ts, era_url),
      NA_character_
    )
    rows$note <- case_when(
      !is.na(raw_ts) ~
        "pre-2018 fact-sheet capture; see inputs/historical/SOURCES.csv",
      TRUE ~ NA_character_
    )
  } else if (base == "agreements_wayback_pre2018") {
    rows$retrieved_at <- "2026-09-12"
    rows$note <- "pre-2018 signed MOA; see inputs/historical/SOURCES.csv"
  }

  append_manifest(folder, rows)
  cat("reconstructed:", folder, "(", nrow(rows), "rows )\n")
}

snapshot_folders <- c(
  list.dirs("sheets", recursive = FALSE),
  list.dirs("agreements", recursive = FALSE)
)

for (folder in snapshot_folders) {
  has_download_log <- file.exists(file.path(folder, "download_path_log.csv"))
  has_manifest <- file.exists(file.path(folder, "manifest.csv"))
  if (has_download_log) {
    convert_download_log(folder)
  } else if (!has_manifest) {
    reconstruct(folder)
  }
}
# rows whose file dedupe removed without recording where the bytes live
n <- annotate_ghost_rows("agreements") + annotate_ghost_rows("sheets")
cat("annotated", n, "manifest rows for deleted duplicates\n")
cat("done\n")
