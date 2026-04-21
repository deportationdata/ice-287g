library(rvest)
library(httr)
library(openxlsx)
library(stringr)

# download files ---------------------------------------------------------

# base 287g page URL
URL <- "https://www.ice.gov/identify-and-arrest/287g"

# getting page content
response <- GET(URL, user_agent("Mozilla/5.0"))
soup <- read_html(content(response, as = "text", encoding = "UTF-8"))

# match links by anchor text
find_links_by_text <- function(page, keyword) {
  all_links <- page |>
    html_elements("a[href]")

  results <- c()
  for (link in all_links) {
    text <- html_text(link, trim = TRUE)
    href <- html_attr(link, "href")
    if (grepl(keyword, text, ignore.case = TRUE)) {
      # ensure absolute URL
      if (startsWith(href, "/")) {
        href <- paste0("https://www.ice.gov", href)
      }
      results <- c(results, href)
    }
  }
  return(results)
}

participating <- find_links_by_text(soup, "participating agencies")
pending <- find_links_by_text(soup, "pending agencies")

# log what was found
cat(sprintf(
  "Found %d participating link(s): %s\n",
  length(participating),
  paste(participating, collapse = ", ")
))
cat(sprintf(
  "Found %d pending link(s): %s\n",
  length(pending),
  paste(pending, collapse = ", ")
))

# abort early with a clear message if nothing found
if (length(participating) == 0) {
  cat("\nERROR: No participating agencies link found on the ICE page.\n")
  cat("Here are all links found on the page:\n")
  all_links <- soup |> html_elements("a[href]")
  for (link in all_links) {
    cat(sprintf(
      "  [%s] %s\n",
      html_text(link, trim = TRUE),
      html_attr(link, "href")
    ))
  }
  stop("Aborting: no participating agencies link found.")
}

# create a "sheets" folder
base_results_folder <- "sheets"
dir.create(base_results_folder, showWarnings = FALSE, recursive = TRUE)

# create a timestamped subfolder
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
results_folder <- file.path(base_results_folder, paste0("sheets_", timestamp))
dir.create(results_folder, showWarnings = FALSE, recursive = TRUE)

# function to download and save excel files
download_excel_from_url <- function(url, folder, label = "file") {
  tryCatch(
    {
      r <- GET(url, user_agent("Mozilla/5.0"))
      stop_for_status(r)

      # try to get filename from content-disposition header first
      cd <- headers(r)[["content-disposition"]]
      if (!is.null(cd) && grepl("filename=", cd)) {
        file_name_only <- str_trim(str_remove(
          str_split(cd, "filename=")[[1]][2],
          '["\']'
        ))
      } else {
        # fall back to last segment of URL
        file_name_only <- basename(str_split(url, "\\?")[[1]][1])
        if (
          is.na(file_name_only) ||
            file_name_only == "" ||
            !grepl("\\.", file_name_only)
        ) {
          file_name_only <- paste0(label, ".xlsx")
        }
      }

      dir.create(folder, showWarnings = FALSE, recursive = TRUE)
      file_path <- file.path(folder, file_name_only)
      writeBin(content(r, as = "raw"), file_path)
      cat(sprintf("Downloaded: %s\n", file_path))
      return(file_path)
    },
    error = function(e) {
      cat(sprintf("Failed to download %s: %s\n", url, conditionMessage(e)))
      return(NULL)
    }
  )
}

# download participating and pending files
downloaded_participating <- c()
for (url in participating) {
  path <- download_excel_from_url(url, results_folder, label = "participating")
  if (!is.null(path)) {
    downloaded_participating <- c(downloaded_participating, path)
  }
}

for (url in pending) {
  download_excel_from_url(url, results_folder, label = "pending")
}

if (length(downloaded_participating) == 0) {
  stop("ERROR: Failed to download any participating agencies file.")
}

# download agreements ----------------------------------------------------

# reading the excel document as a workbook
file_path <- downloaded_participating[1]
wb <- loadWorkbook(file_path)
sheet_name <- names(wb)[1]
df <- readWorkbook(wb, sheet = sheet_name, colNames = FALSE)

# extract hyperlinks from column 7 using the workbook object
ws <- wb$worksheets[[1]]

# pull hyperlink targets from the sheet XML
hyperlink_map <- ws$hyperlinks # named list: cell ref -> target URL

# build a lookup: row number -> hyperlink target
get_hyperlink_for_row <- function(row_idx) {
  cell_ref <- paste0("G", row_idx)
  if (!is.null(hyperlink_map) && cell_ref %in% names(hyperlink_map)) {
    return(hyperlink_map[[cell_ref]]$ref) # target URL
  }
  return(NULL)
}

# collect hyperlink, state, and agency triples
hyperlinks_list <- c()
states_list <- c()
agencies_list <- c()

# rows in df correspond to spreadsheet rows starting at 1 (adjust if there's a header)
for (i in seq_len(nrow(df))) {
  row <- df[i, ]
  # need at least 7 columns
  if (ncol(df) < 7) {
    next
  }

  state <- as.character(row[[1]])
  agency_name <- as.character(row[[2]])
  hyperlink <- get_hyperlink_for_row(i)

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
  }
}

cat(sprintf("Found %d agency agreement links.\n", length(hyperlinks_list)))

# create an "agreements" folder
documents_folder <- "agreements"
dir.create(documents_folder, showWarnings = FALSE, recursive = TRUE)

# create a timestamped subfolder
timestamp_folder <- file.path(
  documents_folder,
  paste0("agreements_", timestamp)
)
dir.create(timestamp_folder, showWarnings = FALSE, recursive = TRUE)

# track failed downloads
failed <- c()

# looping through hyperlink, state, agency combinations
for (i in seq_along(hyperlinks_list)) {
  hyperlink <- hyperlinks_list[i]
  state <- states_list[i]
  agency_name <- agencies_list[i]

  # clean state and agency names for use as folder names
  safe_state_name <- gsub(" ", "_", state)
  safe_agency_name <- gsub("[/\\\\ ]", "_", agency_name)

  # create state and agency folders
  state_folder <- file.path(timestamp_folder, safe_state_name)
  agency_folder <- file.path(state_folder, safe_agency_name)
  dir.create(agency_folder, showWarnings = FALSE, recursive = TRUE)

  # download agreement file
  tryCatch(
    {
      Sys.sleep(1)
      r <- GET(hyperlink, user_agent("Mozilla/5.0"))

      if (status_code(r) == 200) {
        # prefer content-disposition filename, fall back to URL basename
        cd <- headers(r)[["content-disposition"]]
        if (!is.null(cd) && grepl("filename=", cd)) {
          fname <- str_trim(str_remove(
            str_split(cd, "filename=")[[1]][2],
            '["\']'
          ))
        } else {
          fname <- basename(str_split(hyperlink, "\\?")[[1]][1])
          if (is.na(fname) || fname == "" || !grepl("\\.", fname)) {
            fname <- paste0(safe_agency_name, "_agreement")
          }
        }

        file_name <- file.path(agency_folder, fname)
        writeBin(content(r, as = "raw"), file_name)
      } else {
        cat(sprintf("HTTP %d for %s\n", status_code(r), hyperlink))
        failed <- c(failed, hyperlink)
      }
    },
    error = function(e) {
      cat(sprintf("Exception for %s: %s\n", hyperlink, conditionMessage(e)))
      failed <<- c(failed, hyperlink)
    }
  )
}

# log failed downloads
if (length(failed) > 0) {
  failed_log_path <- file.path(timestamp_folder, "failed_downloads.txt")
  writeLines(failed, failed_log_path)
  cat(sprintf(
    "%d failed downloads logged to %s\n",
    length(failed),
    failed_log_path
  ))
}

cat("Done.\n")
