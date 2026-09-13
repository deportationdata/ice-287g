# Snapshot ICE's participating-agencies workbook into sheets/sheets_<timestamp>/
# with a manifest. The MOA pdfs it links are fetched by 0-acquire-moa-pass.R.
suppressPackageStartupMessages({
  library(httr); library(rvest); library(stringr); library(purrr); library(dplyr)
  library(readr); library(fs); library(digest)
})
source("code/functions.R")

page_url <- "https://www.ice.gov/identify-and-arrest/287g"
results <- ice_get(page_url)
stop_for_status(results)
page <- read_html(content(results, as = "text", encoding = "UTF-8"))

make_absolute_url <- function(href) {
  if (is.na(href) || is.null(href) || href == "") return(NA_character_)
  # test "//host/path" before "/path": startsWith("/") matches both
  if (startsWith(href, "//")) return(paste0("https:", href))
  if (startsWith(href, "/")) return(paste0("https://www.ice.gov", href))
  href
}

all_links <- html_elements(page, "a[href]")
links_df <- tibble(
  text = html_text(all_links, trim = TRUE),
  href = map_chr(html_attr(all_links, "href"), make_absolute_url)
)

candidate_links <- links_df |>
  filter(
    str_detect(text, regex("participating agencies|view 287\\(g\\)", ignore_case = TRUE)) |
      str_detect(href, regex("\\.xlsx($|\\?)|file-download/download/public", ignore_case = TRUE))
  ) |>
  pull(href) |>
  unique()
cat(sprintf("Found %d candidate participating agencies link(s): %s\n",
            length(candidate_links), paste(candidate_links, collapse = ", ")))

looks_like_excel <- function(candidate) {
  tryCatch({
    response <- ice_get(candidate)
    ct <- headers(response)[["content-type"]]
    cd <- headers(response)[["content-disposition"]]
    status_code(response) == 200 && (
      str_detect(candidate, regex("\\.xlsx($|\\?)|file-download/download/public", ignore_case = TRUE)) ||
        (!is.null(ct) && str_detect(ct, regex("spreadsheet|excel|octet-stream", ignore_case = TRUE))) ||
        (!is.null(cd) && str_detect(cd, regex("\\.xlsx|excel|participating", ignore_case = TRUE)))
    )
  }, error = function(e) FALSE)
}
participating <- candidate_links[map_lgl(candidate_links, looks_like_excel)]
# ICE has published a pending-agencies workbook before; keep the slot
pending <- character()

if (length(participating) == 0) {
  cat("\nERROR: No participating agencies Excel link found on the ICE page.\n")
  for (i in seq_len(nrow(links_df))) cat(sprintf("  [%s] %s\n", links_df$text[i], links_df$href[i]))
  stop("Aborting: no participating agencies Excel link found.")
}

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
results_folder <- file.path("sheets", paste0("sheets_", timestamp))
dir.create(results_folder, showWarnings = FALSE, recursive = TRUE)

download_workbook <- function(url, folder, label) {
  tryCatch({
    results <- ice_get(url)
    stop_for_status(results)
    bytes <- content(results, as = "raw")
    # a 200 that is really an HTML block page must not be archived as a workbook
    if (!body_has_magic(bytes, "zip")) stop("response is not an xlsx (no PK magic)")
    original <- response_filename(results, url, paste0(label, ".xlsx"))
    file_path <- make_unique_file_path(folder, sanitize_download_filename(original, fallback = label))
    writeBin(bytes, file_path)
    v <- response_validators(results)
    cat(sprintf("Downloaded: %s\n", file_path))
    tibble(
      saved_path = file_path,
      file_hash = digest(file = file_path, algo = "sha256"),
      url = url,
      retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      original_filename = original,
      note = label,
      etag = v[["etag"]],
      last_modified = v[["last_modified"]]
    )
  }, error = function(e) {
    cat(sprintf("Failed to download %s: %s\n", url, conditionMessage(e)))
    NULL
  })
}

downloads <- bind_rows(
  map(participating, download_workbook, folder = results_folder, label = "participating"),
  map(pending, download_workbook, folder = results_folder, label = "pending")
)
if (nrow(downloads) == 0 || !any(downloads$note == "participating")) {
  stop("ERROR: Failed to download any participating agencies file.")
}
append_manifest(results_folder, downloads)
cat(sprintf("Sheet snapshot written: %s (%d workbook(s))\n", results_folder, nrow(downloads)))
