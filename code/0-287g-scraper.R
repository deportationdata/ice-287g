library(tidyverse)
library(httr)
library(rvest)
library(readxl)

source("code/functions.R")

# polite GET with retries and backoff; 4xx statuses are terminal
ice_get <- function(url, ...) {
  RETRY(
    "GET",
    url,
    user_agent("Mozilla/5.0"),
    timeout(60),
    times = 4,
    pause_base = 2,
    pause_cap = 30,
    terminate_on = c(400, 401, 403, 404),
    ...
  )
}

# --- Download 287(g) spreadsheets ---

# base 287(g) page URL
results <- ice_get("https://www.ice.gov/identify-and-arrest/287g")
stop_for_status(results)
page <- read_html(content(results, as = "text", encoding = "UTF-8"))

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# --- Find participating agencies spreadsheet robustly ---

all_links <- html_elements(page, "a[href]")

links <- tibble(
  text = html_text(all_links, trim = TRUE),
  href = html_attr(all_links, "href")
) |>
  filter(!is.na(href), href != "") |>
  mutate(
    href = if_else(
      str_starts(href, "/"),
      paste0("https://www.ice.gov", href),
      href
    )
  )

candidates <- links |>
  filter(
    str_detect(
      text,
      regex("participating agencies|view 287\\(g\\)", ignore_case = TRUE)
    ) |
      str_detect(
        href,
        regex(
          "\\.xlsx($|\\?)|file-download/download/public",
          ignore_case = TRUE
        )
      )
  ) |>
  distinct(href)

cat(sprintf(
  "Found %d candidate participating agencies link(s): %s\n",
  nrow(candidates),
  paste(candidates$href, collapse = ", ")
))

# Verify and download in one pass: each candidate is fetched once, kept only
# if the response looks like an Excel file (by URL shape, content-type, or
# content-disposition), and written out below from the same response.
sheets_folder <- file.path("sheets", paste0("sheets_", timestamp))

downloaded_sheets <- candidates |>
  mutate(
    response = map(href, \(u) tryCatch(ice_get(u), error = \(e) NULL))
  ) |>
  filter(map_lgl(response, \(r) !is.null(r) && status_code(r) == 200)) |>
  mutate(
    content_type = map_chr(response, \(r) headers(r)[["content-type"]] %||% ""),
    content_disposition = map_chr(
      response,
      \(r) headers(r)[["content-disposition"]] %||% ""
    )
  ) |>
  filter(
    str_detect(
      href,
      regex("\\.xlsx($|\\?)|file-download/download/public", ignore_case = TRUE)
    ) |
      str_detect(
        content_type,
        regex("spreadsheet|excel|octet-stream", ignore_case = TRUE)
      ) |
      str_detect(
        content_disposition,
        regex("\\.xlsx|excel|participating", ignore_case = TRUE)
      )
  ) |>
  mutate(
    # filename from the content-disposition header, falling back to the URL
    # basename and then to a fixed label.
    file_name = if_else(
      str_detect(content_disposition, "filename="),
      content_disposition |>
        str_extract("filename=(.*)$", group = 1) |>
        str_remove(";.*$") |>
        str_trim() |>
        str_remove_all("[\"']"),
      basename(str_remove(href, "\\?.*$"))
    ),
    file_name = if_else(
      is.na(file_name) | file_name == "" | !str_detect(file_name, fixed(".")),
      "participating.xlsx",
      file_name
    ),
    path = file.path(sheets_folder, file_name)
  )

pending <- c()

if (length(pending) == 0) {
  cat("No pending agencies file found - skipping.\n")
}

# abort early with a clear message if nothing found
if (nrow(downloaded_sheets) == 0) {
  cat("\nERROR: No participating agencies Excel link found on the ICE page.\n")
  cat("Here are all links found on the page:\n")
  print(links, n = Inf)
  stop("Aborting: no participating agencies Excel link found.")
}

dir.create(sheets_folder, showWarnings = FALSE, recursive = TRUE)

walk2(downloaded_sheets$response, downloaded_sheets$path, \(r, p) {
  writeBin(content(r, as = "raw"), p)
  cat(sprintf("Downloaded: %s\n", p))
})

# --- Download agreement files ---

# trim_ws = FALSE: many STATE / LAW ENFORCEMENT AGENCY cells carry trailing space
sheet_path <- downloaded_sheets$path[1]
participating <- read_excel(sheet_path, trim_ws = FALSE)

agreement_links <- participating |>
  add_moa_links(sheet_path) |>
  transmute(
    state = STATE,
    agency = `LAW ENFORCEMENT AGENCY`,
    url = moa_link
  ) |>
  filter(
    !is.na(url),
    !is.na(state),
    state != "NA",
    !is.na(agency),
    agency != "NA"
  )

cat(sprintf("Found %d agency agreement links.\n", nrow(agreement_links)))

agreements_folder <- file.path(
  "agreements",
  paste0("agreements_", timestamp)
)
dir.create(agreements_folder, showWarnings = FALSE, recursive = TRUE)

downloads <- agreement_links |>
  mutate(
    dest = file.path(
      agreements_folder,
      str_replace_all(state, " ", "_"),
      str_replace_all(agency, "[/\\\\ ]", "_"),
      basename(str_remove(url, "\\?.*$"))
    )
  )

walk(
  unique(dirname(downloads$dest)),
  dir.create,
  showWarnings = FALSE,
  recursive = TRUE
)

downloads <- downloads |>
  mutate(
    saved = map2_lgl(url, dest, \(url, dest) {
      Sys.sleep(1)
      tryCatch(
        {
          response <- ice_get(url)
          stop_for_status(response)
          writeBin(content(response, as = "raw"), dest)
          TRUE
        },
        error = \(e) {
          cat(sprintf("Failed to download %s: %s\n", url, conditionMessage(e)))
          FALSE
        }
      )
    })
  )

# log failed downloads
failed <- downloads |>
  filter(!saved) |>
  pull(url)

if (length(failed) > 0) {
  failed_log_path <- file.path(agreements_folder, "failed_downloads.txt")
  writeLines(failed, failed_log_path)

  cat(sprintf(
    "%d failed downloads logged to %s\n",
    length(failed),
    failed_log_path
  ))
}

cat("Done.\n")
