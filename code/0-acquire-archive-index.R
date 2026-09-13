# Check ICE's /287g-archive page, its index of archived 287(g) MOAs, against the dated
# snapshots in inputs/historical/. When the page's documents change, save a snapshot beside
# the earlier ones and fetch any newly listed PDF no snapshot folder holds.
#   MOA_REPORT  append a markdown summary to this file (the PR body)
suppressPackageStartupMessages({
  library(httr); library(stringr); library(purrr); library(dplyr); library(readr); library(digest); library(xml2)
})
source("code/functions.R")
invisible(Sys.setlocale("LC_TIME", "C"))

report <- Sys.getenv("MOA_REPORT", "")
page_url <- "https://www.ice.gov/287g-archive"
out_dir <- file.path("agreements", paste0("agreements_archive_", format(Sys.Date(), "%Y%m%d")))
snapshot_cols <- c("state", "agency", "date_signed", "moa_file")

r <- ice_get(page_url)
stop_for_status(r)
doc <- read_html(content(r, "text", encoding = "UTF-8"))
# one accordion per state; each document is "Agency [MODEL] (Mon. D, YYYY)"
live <- xml_find_all(doc, "//h3[contains(@class, 'accordion-title')]") |>
  map(\(h) {
    links <- xml_find_all(h, "following-sibling::div[contains(@class, 'accordion-description')][1]//li/a")
    tibble(state = str_squish(xml_text(h)), text = str_squish(xml_text(links)), href = xml_attr(links, "href"))
  }) |>
  list_rbind()
stopifnot("ICE's archive page layout changed: no MOA links found" = nrow(live) > 0)

date_re <- "\\(([A-Za-z]+\\.? \\d{1,2}, \\d{4})\\)$"
live <- live |>
  mutate(date_text = str_replace(str_replace(str_match(text, date_re)[, 2], "\\.", ""), "^Sept\\b", "Sep"),
         date_signed = coalesce(as.Date(date_text, "%b %d, %Y"), as.Date(date_text, "%B %d, %Y")),
         agency = text |> str_remove(paste0("\\s*", date_re)) |> str_remove("\\s*\\[[^\\]]+\\]$") |> str_squish(),
         moa_file = basename(href))

snapshots <- sort(list.files("inputs/historical", "^ice_live_287gMOA_index_\\d{4}-\\d{2}-\\d{2}\\.tsv$", full.names = TRUE))
latest <- read_tsv(last(snapshots), col_names = snapshot_cols, col_types = cols(.default = "c"), progress = FALSE)
added <- live |> filter(!moa_file %in% latest$moa_file)
removed <- latest |> filter(!moa_file %in% live$moa_file)
message(sprintf("ICE archive page: %d documents; %d added and %d removed since %s", nrow(live), nrow(added), nrow(removed), basename(last(snapshots))))
if (!nrow(added) && !nrow(removed)) quit(save = "no")

snapshot <- file.path("inputs/historical", sprintf("ice_live_287gMOA_index_%s.tsv", Sys.Date()))
live |> transmute(state, agency, date_signed = format(date_signed, "%Y-%m-%d"), moa_file) |>
  write_tsv(snapshot, col_names = FALSE, na = "")

held <- unique(c(doc_key(na.omit(snapshot_manifests("agreements")$url)),
                 doc_key(list.files("agreements", "\\.pdf$", recursive = TRUE, ignore.case = TRUE))))
fetched <- list()
for (k in seq_len(nrow(added))) {
  a <- added[k, ]
  if (doc_key(a$href) %in% held) next
  resp <- tryCatch(ice_get(a$href), error = \(e) NULL)
  Sys.sleep(1)
  if (is.null(resp) || status_code(resp) != 200 || !body_has_magic(content(resp, "raw"), "pdf")) next
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  path <- make_unique_file_path(out_dir, sanitize_path_component(a$moa_file, fallback = "moa.pdf"))
  writeBin(content(resp, "raw"), path)
  v <- response_validators(resp)
  fetched[[length(fetched) + 1]] <- tibble(
    saved_path = path, file_hash = digest(file = path, algo = "sha256"), url = a$href,
    retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), state = a$state, agency = a$agency,
    original_filename = a$moa_file, note = "newly listed on ICE's /287g-archive page", etag = v[["etag"]], last_modified = v[["last_modified"]])
}
if (length(fetched)) append_manifest(out_dir, list_rbind(fetched))

if (nzchar(report)) {
  cat(c(sprintf("## ICE's MOA archive page, %s", Sys.Date()), "",
        sprintf("Saved `%s`: %d documents, %d added and %d removed since `%s`; %d new PDF(s) fetched.",
                snapshot, nrow(live), nrow(added), nrow(removed), basename(last(snapshots)), length(fetched)), "",
        if (nrow(added)) paste0("- added: ", added$state, " — ", added$agency, " (", added$moa_file, ")") else character(),
        if (nrow(removed)) paste0("- removed: ", removed$state, " — ", removed$agency, " (", removed$moa_file, ")") else character(), ""),
      file = report, sep = "\n", append = TRUE)
}
