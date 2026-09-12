# Convert the pre-2018 fact-sheet captures in sheets/sheets_wayback_pre2018/raw
# into html_<YYYYMMDD>_<contenthash12>.xlsx snapshots in that folder.

library(tidyverse)
library(rvest)

source("code/functions.R")

src_dir <- "sheets/sheets_wayback_pre2018/raw"
out_dir <- "sheets/sheets_wayback_pre2018"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

captures <- list.files(src_dir, pattern = "^ice_287g_\\d{14}\\.html$",
                       full.names = TRUE)

seen_keys <- str_match(
  list.files(out_dir, pattern = "^html_\\d{8}_[0-9a-f]{12}\\.xlsx$"),
  "^html_(\\d{8})_([0-9a-f]{12})"
)
seen_keys <- paste(seen_keys[, 2], seen_keys[, 3])

n_written <- 0; n_skipped <- 0

for (f in sort(captures)) {
  ts <- str_match(basename(f), "ice_287g_(\\d{14})")[, 2]
  html <- read_html(f)

  # some captures split off a second, MOA-less table of agencies without
  # trained officers, so parse every table carrying the roster columns
  tables <- html_elements(html, "table")
  targets <- keep(as.list(tables), function(tb) {
    hdr <- str_squish(str_to_upper(html_text2(html_elements(tb, "th"))))
    "LAW ENFORCEMENT AGENCY" %in% hdr && any(str_detect(hdr, "SIGNED"))
  })
  if (length(targets) == 0) { cat(ts, "no roster table\n"); next }

  parse_table <- function(target) {
    hdr <- str_squish(str_to_upper(html_text2(html_elements(target, "th"))))
    rows <- html_elements(target, "tbody tr")
    if (length(rows) == 0) {
      rows <- html_elements(target, "tr")[-1]
    }
    map(rows, function(r) {
      tds <- html_elements(r, "td")
      if (length(tds) < length(hdr)) return(NULL)
      # the fact sheets append footnote markers to cells ("DELAWARE**")
      vals <- str_remove_all(str_squish(html_text2(tds)), "[*†]+$")
      names(vals) <- hdr[seq_along(vals)]
      href <- html_attr(html_element(r, "td a"), "href")
      if (!is.na(href) && str_starts(href, "/web/")) {
        href <- paste0("https://web.archive.org", href)
      }
      tibble(
        STATE = vals[["STATE"]],
        `LAW ENFORCEMENT AGENCY` = vals[["LAW ENFORCEMENT AGENCY"]],
        `SUPPORT TYPE` = vals[["SUPPORT TYPE"]],
        SIGNED = as.Date(str_extract(
          vals[[which(str_detect(names(vals), "SIGNED"))]],
          "\\d{4}-\\d{2}-\\d{2}"
        )),
        MOA = href
      )
    }) |>
      list_rbind()
  }

  parsed <- map(targets, parse_table) |>
    list_rbind() |>
    filter(!is.na(`LAW ENFORCEMENT AGENCY`), `LAW ENFORCEMENT AGENCY` != "")

  if (nrow(parsed) == 0) { cat(ts, "empty table\n"); next }

  content_hash <- substr(digest::digest(parsed), 1, 12)
  key <- paste(substr(ts, 1, 8), content_hash)
  if (key %in% seen_keys) { n_skipped <- n_skipped + 1; next }
  seen_keys <- c(seen_keys, key)

  out <- file.path(out_dir, sprintf("html_%s_%s.xlsx",
                                    substr(ts, 1, 8), content_hash))
  writexl::write_xlsx(parsed, out)
  n_written <- n_written + 1
  era_url <- if (substr(ts, 1, 4) <= "2014") {
    "http://www.ice.gov/news/library/factsheets/287g.htm"
  } else {
    "http://www.ice.gov/factsheets/287g"
  }
  append_manifest(out_dir, tibble(
    saved_path = out,
    file_hash = digest::digest(file = out, algo = "sha256"),
    url = sprintf("https://web.archive.org/web/%sid_/%s", ts, era_url),
    retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    note = sprintf(
      "table extracted from raw/ice_287g_%s.html by code/0-287g-pre2018-rosters.R",
      ts
    )
  ))
  cat(ts, "rows:", nrow(parsed), "->", basename(out), "\n")
}

cat(sprintf("done: %d written, %d skipped\n", n_written, n_skipped))
