# Scrape ICE's 287(g) workbook and linked MOAs into sheets/ and agreements/.

library(httr)
library(rvest)
library(openxlsx)
library(stringr)
library(fs)
library(digest)

sanitize_path_component <- function(x, fallback = "unnamed") {
  x <- as.character(x)
  x <- str_squish(x)

  if (is.na(x) || x == "" || x == "NA") {
    x <- fallback
  }

  x <- str_replace_all(x, "[[:space:]]+", "_")
  x <- fs::path_sanitize(x, replacement = "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_remove_all(x, "^_+|_+$")

  if (is.na(x) || x == "") {
    x <- fallback
  }

  x
}

sanitize_download_filename <- function(file_name, fallback) {
  file_name <- sanitize_path_component(file_name, fallback = fallback)

  if (!str_detect(file_name, fixed("."))) {
    file_name <- paste0(file_name, ".xlsx")
  }

  file_name
}

make_unique_file_path <- function(folder, file_name) {
  candidate <- file.path(folder, file_name)

  if (!file.exists(candidate)) {
    return(candidate)
  }

  extension <- tools::file_ext(file_name)
  stem <- if (extension == "") {
    file_name
  } else {
    str_remove(file_name, paste0("\\.", extension, "$"))
  }

  i <- 2
  repeat {
    candidate_name <- if (extension == "") {
      paste0(stem, "_", i)
    } else {
      paste0(stem, "_", i, ".", extension)
    }
    candidate <- file.path(folder, candidate_name)

    if (!file.exists(candidate)) {
      return(candidate)
    }

    i <- i + 1
  }
}

url <- "https://www.ice.gov/identify-and-arrest/287g"

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

results <- ice_get(url)
stop_for_status(results)
page <- read_html(content(results, as = "text", encoding = "UTF-8"))

# ICE publishes some MOA urls SafeLinks-wrapped; the real one is in url=
unwrap_safelink <- function(u) {
  if (is.na(u) || !str_detect(u, "safelinks\\.protection\\.outlook\\.com")) {
    return(u)
  }
  inner <- str_extract(u, "(?<=[?&]url=)[^&]+")
  if (is.na(inner)) u else URLdecode(inner)
}

make_absolute_url <- function(href) {
  if (is.na(href) || is.null(href) || href == "") {
    return(NA_character_)
  }

  # test "//host/path" before "/path": startsWith("/") matches both
  if (startsWith(href, "//")) {
    return(paste0("https:", href))
  }

  if (startsWith(href, "/")) {
    return(paste0("https://www.ice.gov", href))
  }

  return(href)
}

all_links <- page |>
  html_elements("a[href]")

links_df <- data.frame(
  text = all_links |> html_text(trim = TRUE),
  href = all_links |> html_attr("href"),
  stringsAsFactors = FALSE
)

links_df$href <- vapply(links_df$href, make_absolute_url, character(1))

candidate_links <- links_df |>
  dplyr::filter(
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
  dplyr::pull(href) |>
  unique()

cat(sprintf(
  "Found %d candidate participating agencies link(s): %s\n",
  length(candidate_links),
  paste(candidate_links, collapse = ", ")
))

verified_links <- c()

for (candidate in candidate_links) {
  test <- tryCatch(
    {
      response <- ice_get(candidate)
      status <- status_code(response)
      ct <- headers(response)[["content-type"]]
      cd <- headers(response)[["content-disposition"]]

      looks_like_excel <-
        status == 200 &&
        (str_detect(candidate, regex("\\.xlsx($|\\?)", ignore_case = TRUE)) ||
          str_detect(
            candidate,
            regex("file-download/download/public", ignore_case = TRUE)
          ) ||
          (!is.null(ct) &&
            str_detect(
              ct,
              regex("spreadsheet|excel|octet-stream", ignore_case = TRUE)
            )) ||
          (!is.null(cd) &&
            str_detect(
              cd,
              regex("\\.xlsx|excel|participating", ignore_case = TRUE)
            )))

      if (looks_like_excel) candidate else NA_character_
    },
    error = function(e) NA_character_
  )

  if (!is.na(test)) {
    verified_links <- c(verified_links, test)
  }
}

participating <- unique(verified_links)
pending <- c()

if (length(pending) == 0) {
  cat("No pending agencies file found - skipping.\n")
}

if (length(participating) == 0) {
  cat("\nERROR: No participating agencies Excel link found on the ICE page.\n")
  cat("Here are all links found on the page:\n")

  all_links <- page |>
    html_elements("a[href]")

  for (link in all_links) {
    cat(sprintf(
      "  [%s] %s\n",
      link |> html_text(trim = TRUE),
      link |> html_attr("href")
    ))
  }

  stop("Aborting: no participating agencies Excel link found.")
}

base_results_folder <- "sheets"
dir.create(base_results_folder, showWarnings = FALSE, recursive = TRUE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
results_folder <- file.path(base_results_folder, paste0("sheets_", timestamp))
dir.create(results_folder, showWarnings = FALSE, recursive = TRUE)
sheet_download_log <- data.frame(
  saved_path = character(),
  file_hash = character(),
  url = character(),
  retrieved_at = character(),
  state = character(),
  agency = character(),
  original_filename = character(),
  note = character(),
  stringsAsFactors = FALSE
)

download_excel_from_url <- function(url, folder, label = "file") {
  tryCatch(
    {
      results <- ice_get(url)
      stop_for_status(results)

      cd <- headers(results)[["content-disposition"]]

      if (!is.null(cd) && grepl("filename=", cd)) {
        file_name_only <- str_split(cd, "filename=", n = 2)[[1]][2]
        file_name_only <- str_remove(file_name_only, ";.*$")
        file_name_only <- str_trim(file_name_only)
        file_name_only <- str_remove_all(file_name_only, "^[\"']|[\"']$")
      } else {
        file_name_only <- basename(str_split(url, "\\?")[[1]][1])

        if (
          is.na(file_name_only) ||
            file_name_only == "" ||
            !grepl("\\.", file_name_only)
        ) {
          file_name_only <- paste0(label, ".xlsx")
        }
      }

      original_file_name_only <- file_name_only
      file_name_only <- sanitize_download_filename(
        file_name_only,
        fallback = label
      )

      dir.create(folder, showWarnings = FALSE, recursive = TRUE)

      file_path <- make_unique_file_path(folder, file_name_only)
      file_name_only <- basename(file_path)
      writeBin(content(results, as = "raw"), file_path)
      file_hash <- digest(file = file_path, algo = "sha256")

      sheet_download_log <<- rbind(
        sheet_download_log,
        data.frame(
          saved_path = file_path,
          file_hash = file_hash,
          url = url,
          retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          state = NA_character_,
          agency = NA_character_,
          original_filename = original_file_name_only,
          note = NA_character_,
          stringsAsFactors = FALSE
        )
      )

      cat(sprintf("Downloaded: %s\n", file_path))

      return(file_path)
    },
    error = function(e) {
      cat(sprintf("Failed to download %s: %s\n", url, conditionMessage(e)))
      return(NULL)
    }
  )
}

downloaded_participating <- c()

for (url in participating) {
  file_path <- download_excel_from_url(
    url,
    results_folder,
    label = "participating"
  )

  if (!is.null(file_path)) {
    downloaded_participating <- c(downloaded_participating, file_path)
  }
}

for (url in pending) {
  download_excel_from_url(
    url,
    results_folder,
    label = "pending"
  )
}

write.csv(
  sheet_download_log,
  file.path(results_folder, "manifest.csv"),
  row.names = FALSE,
  na = ""
)

if (length(downloaded_participating) == 0) {
  stop("ERROR: Failed to download any participating agencies file.")
}

# Agreement documents linked from the participating-agencies workbook.

file_path <- downloaded_participating[1]
wb <- loadWorkbook(file_path)
sheet_name <- names(wb)[1]
df <- readWorkbook(wb, sheet = sheet_name, colNames = FALSE)

ws <- wb$worksheets[[1]]

# readWorkbook drops empty rows/cols, which would misattribute MOA links
valued <- !is.na(ws$sheet_data$v)
valued_rows <- ws$sheet_data$rows[valued]
valued_cols <- ws$sheet_data$cols[valued]
if (
  length(valued_rows) == 0 ||
    min(valued_rows) != 1L ||
    nrow(df) != max(valued_rows) ||
    min(valued_cols) != 1L ||
    ncol(df) != max(valued_cols)
) {
  stop(
    "Sheet has empty rows or columns inside the data region; ",
    "hyperlink-to-row alignment would misattribute MOA links. ",
    "Inspect the downloaded workbook before re-running."
  )
}

hyperlink_map <- ws$hyperlinks

hyperlink_lookup <- list()

if (!is.null(hyperlink_map) && length(hyperlink_map) > 0) {
  for (h in hyperlink_map) {
    cell_ref <- h$ref
    target <- h$target

    if (
      !is.null(cell_ref) &&
        !is.null(target) &&
        !is.na(cell_ref) &&
        !is.na(target) &&
        target != ""
    ) {
      hyperlink_lookup[[cell_ref]] <- target
    }
  }
}

get_hyperlink_for_row <- function(row_idx, column) {
  column_index <- match(column, LETTERS)

  if (is.na(column_index) || column_index > ncol(df)) {
    return(NULL)
  }

  cell_ref <- paste0(column, row_idx)

  if (cell_ref %in% names(hyperlink_lookup)) {
    return(hyperlink_lookup[[cell_ref]])
  }

  return(NULL)
}

hyperlinks_list <- c()
states_list <- c()
agencies_list <- c()
document_types_list <- c()

document_columns <- c(MOA = "G", addendum = "H")

for (i in seq_len(nrow(df))) {
  row <- df[i, ]

  if (ncol(df) < 2) {
    next
  }

  state <- as.character(row[[1]])
  agency_name <- as.character(row[[2]])

  for (document_type in names(document_columns)) {
    hyperlink <- get_hyperlink_for_row(
      i,
      document_columns[[document_type]]
    )

    if (
      !is.null(hyperlink) &&
        !is.na(state) &&
        state != "NA" &&
        !is.na(agency_name) &&
        agency_name != "NA"
    ) {
      hyperlinks_list <- c(hyperlinks_list, hyperlink)
      states_list <- c(states_list, state)
      agencies_list <- c(agencies_list, agency_name)
      document_types_list <- c(document_types_list, document_type)
    }
  }
}

cat(sprintf(
  "Found %d agency MOA/addendum links.\n",
  length(hyperlinks_list)
))

documents_folder <- "agreements"
dir.create(documents_folder, showWarnings = FALSE, recursive = TRUE)

timestamp_folder <- file.path(
  documents_folder,
  paste0("agreements_", timestamp)
)
dir.create(timestamp_folder, showWarnings = FALSE, recursive = TRUE)
agreement_download_log <- data.frame(
  saved_path = character(),
  file_hash = character(),
  url = character(),
  retrieved_at = character(),
  state = character(),
  agency = character(),
  original_filename = character(),
  note = character(),
  stringsAsFactors = FALSE
)

failed <- c()

for (i in seq_along(hyperlinks_list)) {
  hyperlink <- unwrap_safelink(hyperlinks_list[i])
  state <- states_list[i]
  agency_name <- agencies_list[i]
  document_type <- document_types_list[i]

  safe_state_name <- sanitize_path_component(state, fallback = "unknown_state")
  safe_agency_name <- sanitize_path_component(
    agency_name,
    fallback = "unknown_agency"
  )

  state_folder <- file.path(timestamp_folder, safe_state_name)
  agency_folder <- file.path(state_folder, safe_agency_name)

  dir.create(agency_folder, showWarnings = FALSE, recursive = TRUE)

  tryCatch(
    {
      Sys.sleep(1)
      results <- ice_get(hyperlink)

      if (status_code(results) == 200) {
        cd <- headers(results)[["content-disposition"]]

        if (!is.null(cd) && grepl("filename=", cd)) {
          file_name_only <- str_split(cd, "filename=", n = 2)[[1]][2]
          file_name_only <- str_remove(file_name_only, ";.*$")
          file_name_only <- str_trim(file_name_only)
          file_name_only <- str_remove_all(file_name_only, "^[\"']|[\"']$")
        } else {
          file_name_only <- basename(str_split(hyperlink, "\\?")[[1]][1])

          if (
            is.na(file_name_only) ||
              file_name_only == "" ||
              !grepl("\\.", file_name_only)
          ) {
            file_name_only <- paste0(
              safe_agency_name,
              "_",
              document_type,
              "_agreement"
            )
          }
        }

        original_file_name_only <- file_name_only
        file_name_only <- sanitize_path_component(
          paste0(document_type, "_", file_name_only),
          fallback = paste0(safe_agency_name, "_", document_type)
        )

        file_name <- make_unique_file_path(agency_folder, file_name_only)
        file_name_only <- basename(file_name)
        writeBin(content(results, as = "raw"), file_name)
        file_hash <- digest(file = file_name, algo = "sha256")

        agreement_download_log <<- rbind(
          agreement_download_log,
          data.frame(
            saved_path = file_name,
            file_hash = file_hash,
            url = hyperlink,
            retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
                                  tz = "UTC"),
            state = state,
            agency = agency_name,
            original_filename = original_file_name_only,
            note = document_type,
            stringsAsFactors = FALSE
          )
        )
      } else {
        cat(sprintf(
          "HTTP %d for %s (%s)\n",
          status_code(results),
          hyperlink,
          document_type
        ))
        failed <- c(failed, paste(document_type, hyperlink, sep = ": "))
      }
    },
    error = function(e) {
      cat(sprintf(
        "Exception for %s (%s): %s\n",
        hyperlink,
        document_type,
        conditionMessage(e)
      ))
      failed <<- c(failed, paste(document_type, hyperlink, sep = ": "))
    }
  )
}

if (length(failed) > 0) {
  failed_log_path <- file.path(timestamp_folder, "failed_downloads.txt")
  writeLines(failed, failed_log_path)

  cat(sprintf(
    "%d failed downloads logged to %s\n",
    length(failed),
    failed_log_path
  ))
}

write.csv(
  agreement_download_log,
  file.path(timestamp_folder, "manifest.csv"),
  row.names = FALSE,
  na = ""
)

cat("Done.\n")
