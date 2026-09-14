# Recover MOA PDFs the Wayback Machine archived under ICE's /doclib/287gMOA/ folder that no
# snapshot folder holds: documents ICE linked and replaced before the MOA pass saved them.
# Run by hand when the archive needs completing; the MOA pass keeps current links whole.
# -> agreements/agreements_wayback_<today>/ (created only when something is recovered)
suppressPackageStartupMessages({
  library(httr); library(stringr); library(purrr); library(dplyr); library(tidyr); library(readr); library(digest)
})
source("code/functions.R")

out_dir <- file.path("agreements", paste0("agreements_wayback_", format(Sys.Date(), "%Y%m%d")))
# ICE files non-MOA documents here too (monthly encounter reports, program applications)
not_moa <- "encounterreport|application"

# every url the CDX index holds under the folder, earliest 200 capture of each
cdx_captures <- function(prefix) {
  rows <- list()
  key <- NULL
  repeat {
    query <- list(url = prefix, matchType = "prefix", collapse = "urlkey", output = "json",
                  fl = "original,timestamp", filter = "statuscode:200", limit = "20000", showResumeKey = "true")
    if (!is.null(key)) query$resumeKey <- key
    # the CDX API 503s freely under load
    r <- RETRY("GET", "https://web.archive.org/cdx/search/cdx", query = query, timeout(300),
               times = 5, pause_base = 5, pause_cap = 60)
    stop_for_status(r)
    body <- jsonlite::fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)[-1]
    n <- length(body)
    key <- if (n >= 2 && length(body[[n - 1]]) == 0) body[[n]][[1]] else NULL
    if (!is.null(key)) body <- body[seq_len(n - 2)]
    rows <- c(rows, body)
    if (is.null(key)) break
  }
  tibble(original = map_chr(rows, 1), timestamp = map_chr(rows, 2))
}

manifests <- snapshot_manifests("agreements")
held <- unique(c(doc_key(na.omit(manifests$url)), doc_key(na.omit(manifests$original_filename)),
                 doc_key(list.files("agreements", "\\.pdf$", recursive = TRUE, ignore.case = TRUE))))

captures <- cdx_captures("www.ice.gov/doclib/287gMOA/") |>
  filter(str_detect(str_to_lower(original), "\\.pdf($|\\?)")) |>
  mutate(key = doc_key(original)) |>
  filter(!key %in% held, !str_detect(key, not_moa)) |>
  arrange(timestamp) |>
  distinct(key, .keep_all = TRUE)
message("archived MOA PDFs no snapshot folder holds: ", nrow(captures))

recovered <- list()
for (i in seq_len(nrow(captures))) {
  cap <- captures[i, ]
  src <- sprintf("https://web.archive.org/web/%sid_/%s", cap$timestamp, cap$original)
  r <- tryCatch(RETRY("GET", src, user_agent("Mozilla/5.0"), timeout(120), times = 4, pause_base = 5, pause_cap = 60),
                error = \(e) NULL)
  Sys.sleep(1)
  if (is.null(r) || status_code(r) != 200 || !body_has_magic(content(r, "raw"), "pdf")) {
    message("  skipped (no PDF body): ", cap$original)
    next
  }
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  bytes <- content(r, "raw")
  name <- cap$original |> str_remove("\\?.*$") |> basename() |> URLdecode() |> sanitize_path_component(fallback = "moa.pdf")
  path <- make_unique_file_path(out_dir, name)
  writeBin(bytes, path)
  recovered[[length(recovered) + 1]] <- tibble(
    saved_path = path, file_hash = digest(file = path, algo = "sha256"), url = src,
    retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    original_filename = basename(URLdecode(str_remove(cap$original, "\\?.*$"))),
    note = paste0("recovered from the Wayback Machine's ", substr(cap$timestamp, 1, 8), " capture of ", cap$original,
                  "; no snapshot folder held this document"),
    capture_time = cap$timestamp
  )
}

if (length(recovered)) {
  append_manifest(out_dir, list_rbind(recovered))
  message("recovered ", length(recovered), " PDF(s) into ", out_dir)
} else {
  message("nothing to recover")
}
