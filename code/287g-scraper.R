library(httr)
library(rvest)
library(openxlsx)
library(stringr)

# --- Download 287(g) spreadsheets ---

# base 287(g) page URL
url <- "https://www.ice.gov/identify-and-arrest/287g"

# get page content
results <- GET(url, user_agent("Mozilla/5.0"))
page <- read_html(content(results, as = "text", encoding = "UTF-8"))

# function to normalize ICE URLs
make_absolute_url <- function(href) {
  if (is.na(href) || is.null(href) || href == "") {
    return(NA_character_)
  }

  if (startsWith(href, "/")) {
    return(paste0("https://www.ice.gov", href))
  }

  return(href)
}

# function to find candidate links by broad text / href patterns
find_candidate_links <- function(page, patterns) {
  all_links <- page |>
    html_elements("a[href]")

  results <- c()

  for (link in all_links) {
    text <- link |>
      html_text(trim = TRUE)

    href <- link |>
      html_attr("href")

    href <- make_absolute_url(href)
    combined <- paste(text, href)

    if (any(str_detect(combined, regex(patterns, ignore_case = TRUE)))) {
      results <- c(results, href)
    }
  }

  return(unique(results[!is.na(results)]))
}

# function to extract direct excel links from a page
find_excel_links <- function(page) {
  links <- page |>
    html_elements("a[href]") |>
    html_attr("href")

  links <- vapply(links, make_absolute_url, character(1))
  links <- links[str_detect(links, regex("\\.xlsx($|\\?)", ignore_case = TRUE))]

  return(unique(links[!is.na(links)]))
}

# function to resolve candidate links to direct excel files
resolve_to_excel_links <- function(candidate_links) {
  resolved_links <- c()

  for (candidate_url in candidate_links) {
    if (
      str_detect(candidate_url, regex("\\.xlsx($|\\?)", ignore_case = TRUE))
    ) {
      resolved_links <- c(resolved_links, candidate_url)
      next
    }

    tryCatch(
      {
        Sys.sleep(1)
        sub_results <- GET(candidate_url, user_agent("Mozilla/5.0"))
        stop_for_status(sub_results)

        content_type <- headers(sub_results)[["content-type"]]

        # if the candidate itself is already an excel file
        if (
          !is.null(content_type) &&
            str_detect(
              content_type,
              regex(
                "spreadsheet|excel|application/vnd.openxmlformats-officedocument",
                ignore_case = TRUE
              )
            )
        ) {
          resolved_links <- c(resolved_links, candidate_url)
        } else {
          sub_page <- read_html(
            content(sub_results, as = "text", encoding = "UTF-8")
          )

          excel_links <- find_excel_links(sub_page)

          if (length(excel_links) > 0) {
            resolved_links <- c(resolved_links, excel_links)
          }
        }
      },
      error = function(e) {
        cat(sprintf(
          "Failed to resolve candidate link %s: %s\n",
          candidate_url,
          conditionMessage(e)
        ))
      }
    )
  }

  return(unique(resolved_links))
}

# find candidate participating and pending links
participating_candidates <- find_candidate_links(
  page,
  c(
    "view\\s*287\\(g\\)\\s*participating",
    "287\\(g\\).*participating",
    "participatingAgencies",
    "participating.*xlsx"
  )
)

pending_candidates <- find_candidate_links(
  page,
  c(
    "pending\\s*agencies",
    "287\\(g\\).*pending",
    "pendingAgencies",
    "pending.*xlsx"
  )
)

participating <- resolve_to_excel_links(participating_candidates)
pending <- resolve_to_excel_links(pending_candidates)

# log what was found
cat(sprintf(
  "Found %d participating candidate link(s): %s\n",
  length(participating_candidates),
  paste(participating_candidates, collapse = ", ")
))
cat(sprintf(
  "Resolved %d participating Excel link(s): %s\n",
  length(participating),
  paste(participating, collapse = ", ")
))
cat(sprintf(
  "Found %d pending candidate link(s): %s\n",
  length(pending_candidates),
  paste(pending_candidates, collapse = ", ")
))
cat(sprintf(
  "Resolved %d pending Excel link(s): %s\n",
  length(pending),
  paste(pending, collapse = ", ")
))

# abort early with a clear message if nothing found
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

# create results folders
base_results_folder <- "sheets"
dir.create(base_results_folder, showWarnings = FALSE, recursive = TRUE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
results_folder <- file.path(base_results_folder, paste0("sheets_", timestamp))
dir.create(results_folder, showWarnings = FALSE, recursive = TRUE)

# function to download and save excel files
download_excel_from_url <- function(url, folder, label = "file") {
  tryCatch(
    {
      results <- GET(url, user_agent("Mozilla/5.0"))
      stop_for_status(results)

      # try to get filename from content-disposition header first
      cd <- headers(results)[["content-disposition"]]

      if (!is.null(cd) && grepl("filename=", cd)) {
        file_name_only <- str_trim(
          str_remove(
            str_split(cd, "filename=")[[1]][2],
            '["\']'
          )
        )
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
      writeBin(content(results, as = "raw"), file_path)

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

if (length(downloaded_participating) == 0) {
  stop("ERROR: Failed to download any participating agencies file.")
}

# --- Download agreement files ---

# read the excel document as a workbook
file_path <- downloaded_participating[1]
wb <- loadWorkbook(file_path)
sheet_name <- names(wb)[1]
df <- readWorkbook(wb, sheet = sheet_name, colNames = FALSE)

# extract hyperlinks from column 7 using the workbook object
ws <- wb$worksheets[[1]]

# pull hyperlink targets from the sheet XML
hyperlink_map <- ws$hyperlinks

# helper function: row number -> hyperlink target
get_hyperlink_for_row <- function(row_idx) {
  cell_ref <- paste0("G", row_idx)

  if (!is.null(hyperlink_map) && cell_ref %in% names(hyperlink_map)) {
    return(hyperlink_map[[cell_ref]]$ref)
  } else {
    return(NULL)
  }
}

# collect hyperlink, state, and agency values
hyperlinks_list <- c()
states_list <- c()
agencies_list <- c()

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

# create agreements folders
documents_folder <- "agreements"
dir.create(documents_folder, showWarnings = FALSE, recursive = TRUE)

timestamp_folder <- file.path(
  documents_folder,
  paste0("agreements_", timestamp)
)
dir.create(timestamp_folder, showWarnings = FALSE, recursive = TRUE)

# track failed downloads
failed <- c()

# loop through hyperlink, state, agency combinations
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
      results <- GET(hyperlink, user_agent("Mozilla/5.0"))

      if (status_code(results) == 200) {
        # prefer content-disposition filename, fall back to URL basename
        cd <- headers(results)[["content-disposition"]]

        if (!is.null(cd) && grepl("filename=", cd)) {
          file_name_only <- str_trim(
            str_remove(
              str_split(cd, "filename=")[[1]][2],
              '["\']'
            )
          )
        } else {
          file_name_only <- basename(str_split(hyperlink, "\\?")[[1]][1])

          if (
            is.na(file_name_only) ||
              file_name_only == "" ||
              !grepl("\\.", file_name_only)
          ) {
            file_name_only <- paste0(safe_agency_name, "_agreement")
          }
        }

        file_name <- file.path(agency_folder, file_name_only)
        writeBin(content(results, as = "raw"), file_name)
      } else {
        cat(sprintf("HTTP %d for %s\n", status_code(results), hyperlink))
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
