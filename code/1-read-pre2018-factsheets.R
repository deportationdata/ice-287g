# Parse the archived ICE 287(g) fact-sheet rosters -> data/pre2018-roster-long.csv (agency x snapshot).

suppressPackageStartupMessages({ library(rvest); library(xml2); library(dplyr); library(stringr) })

ROOT   <- "."  # run from the repo root
DIR_FS <- file.path(ROOT, "sheets", "sheets_wayback_pre2018", "raw")
OUT    <- file.path(ROOT, "data"); dir.create(OUT, showWarnings = FALSE)

files <- sort(list.files(DIR_FS, pattern = "^ice_287g_\\d+\\.html$", full.names = TRUE))
if (!length(files)) stop("No fact-sheet captures found. Run code/0-287g-pre2018-download.R first.")

# ICE stopped updating the on-page caption after 2014-08; later captures use the capture date instead.
CAPTION_RELIABLE_THROUGH <- "20140910999999"

parse_one <- function(path) {
  ts   <- str_match(basename(path), "(\\d{14})")[, 2]
  doc  <- read_html(path)
  txt  <- html_text2(doc)

  cap  <- str_match(txt, "Mutually Signed Agreements\\s*\\((\\d+)\\)[^\\n]{0,40}")[, 1]
  n_cap <- suppressWarnings(as.integer(str_match(cap, "\\((\\d+)\\)")[, 2]))
  dt_cap <- str_match(cap, "(\\d{2}/\\d{2}/\\d{4})")[, 2]

  rows <- list()
  for (tb in html_elements(doc, "table")) {
    for (tr in html_elements(tb, "tr")) {
      tds <- html_elements(tr, "td")
      if (length(tds) < 4) next
      cells <- str_squish(html_text2(tds))
      if (toupper(cells[1]) %in% c("STATE", "")) next
      if (!str_detect(cells[1], "^[A-Za-z]")) next
      a    <- html_element(tr, xpath = ".//a[contains(translate(@href,'PDF','pdf'),'.pdf')]")
      href <- if (length(a) && !is.na(a)) html_attr(a, "href") else NA_character_
      rows[[length(rows) + 1]] <- tibble(
        state        = cells[1],
        agency       = cells[2],
        support_type = cells[3],
        date_signed  = cells[4],
        moa_file     = if (is.na(href)) NA_character_ else basename(href)
      )
    }
  }
  if (!length(rows)) { warning("no table rows parsed in ", basename(path)); return(NULL) }

  reliable <- ts <= CAPTION_RELIABLE_THROUGH & !is.na(dt_cap)
  bind_rows(rows) |>
    mutate(
      snapshot_ts   = ts,
      capture_date  = as.Date(ts, format = "%Y%m%d"),
      caption       = cap,
      caption_n     = n_cap,
      content_asof  = if (reliable) as.Date(dt_cap, "%m/%d/%Y") else as.Date(ts, "%Y%m%d"),
      asof_basis    = if (reliable) "on-page caption" else "capture date (caption stale)",
      .before = 1
    )
}

roster <- bind_rows(lapply(files, parse_one)) |>
  mutate(
    # DATES SIGNED is the current agreement's date, overwritten on each re-signing, not first adoption.
    date_signed = suppressWarnings(as.Date(date_signed, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))),
    support_type = case_when(
      str_detect(toupper(support_type), "JAIL")  ~ "Jail Enforcement",
      str_detect(toupper(support_type), "TASK")  ~ "Task Force",
      str_detect(toupper(support_type), "HYBRID|JOINT") ~ "Hybrid",
      TRUE ~ support_type
    )
  ) |>
  arrange(snapshot_ts, state, agency)

write.csv(roster, file.path(OUT, "pre2018-roster-long.csv"), row.names = FALSE, na = "")

cat(sprintf("Parsed %d captures -> %d agency-snapshot rows, %d distinct agencies\n",
            length(files), nrow(roster), n_distinct(paste(roster$state, roster$agency))))
print(roster |> group_by(snapshot_ts, content_asof, asof_basis, caption_n) |>
        summarise(rows = n(), moa_links = sum(!is.na(moa_file)), .groups = "drop"),
      n = 100)
cat("\nNOTE: where caption_n disagrees with `rows`, trust `rows`.\n")
