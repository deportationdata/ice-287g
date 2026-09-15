# Delete duplicate downloads and empty folders under agreements/ and sheets/.

library(digest)
suppressPackageStartupMessages({ library(stringr); library(purrr); library(dplyr); library(readr) })
source("code/functions.R")

deleted_in <- character()

get_file_hash <- function(filepath) {
  file_hash <- digest(file = filepath, algo = "sha256")

  return(file_hash)
}

# ICE links byte-identical files under several agencies, so scope by agency
dedupe_scope_key <- function(base_path, file_path, file_hash) {
  rel_path <- substring(file_path, nchar(base_path) + 2)
  parts <- strsplit(rel_path, "/", fixed = TRUE)[[1]]
  # parts[1] is the snapshot folder; scope is STATE/AGENCY for agreements
  scope <- parts[-c(1, length(parts))]
  # a sheet's scope is the day it was observed: the scrape folder's date, ICE's
  # filename date (pending lists carry it in the same forms as participating
  # ones), or the Wayback capture timestamp that names an archived page (the
  # digits 1-read-sheets.R dates a capture by). Identical content on different
  # days is two observations, not a duplicate; a sheet with no date of its own
  # is never a duplicate of another file
  if (basename(base_path) == "sheets") {
    name <- basename(file_path)
    day <- coalesce(
      as.Date(str_match(file_path, "sheets_(\\d{8})_\\d{6}")[, 2], format = "%Y%m%d"),
      ice_filename_date(str_replace(name, regex("^pendingAgencies", ignore_case = TRUE), "participatingAgencies")),
      as.Date(str_sub(str_match(name, "^(?:ice_287g_)?(?:pre\\d{4}_)?(\\d{14})")[, 2], 1, 8), format = "%Y%m%d")
    )
    scope <- if (is.na(day)) rel_path else format(day, "%Y-%m-%d")
  }
  paste(c(scope, file_hash), collapse = "/")
}

# walk wayback folders first so a re-download is deleted, not the archived copy
order_wayback_first <- function(paths) {
  paths <- sort(paths)
  is_wayback <- grepl("wayback", paths, fixed = TRUE)
  c(paths[is_wayback], paths[!is_wayback])
}

remove_duplicate_files_recursive <- function(base_path) {
  seen_files <- list()

  all_files <- list.files(base_path, recursive = TRUE, full.names = TRUE)
  all_files <- order_wayback_first(all_files)

  for (file_path in all_files) {
    if (!file.exists(file_path) || file.info(file_path)$isdir) {
      next
    }

    file_hash <- tryCatch(
      get_file_hash(file_path),
      error = function(e) NULL
    )

    if (is.null(file_hash)) {
      next
    }

    file_key <- dedupe_scope_key(base_path, file_path, file_hash)

    if (is.null(seen_files[[file_key]])) {
      seen_files[[file_key]] <- file_path
    } else {
      cat(sprintf("Deleting: %s\n", file_path))
      file.remove(file_path)
      deleted_in <<- union(deleted_in, dirname(file_path))
    }
  }
}

remove_duplicate_files_one_level <- function(base_path) {
  seen_files <- list()

  subfolders <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)
  subfolders <- order_wayback_first(subfolders)

  for (subfolder in subfolders) {
    if (!file.info(subfolder)$isdir) {
      next
    }

    files <- list.files(subfolder, recursive = FALSE, full.names = TRUE)
    files <- sort(files)

    for (file_path in files) {
      if (!file.exists(file_path) || file.info(file_path)$isdir) {
        next
      }

      file_hash <- tryCatch(
        get_file_hash(file_path),
        error = function(e) NULL
      )

      if (is.null(file_hash)) {
        next
      }

      file_key <- dedupe_scope_key(base_path, file_path, file_hash)

      if (is.null(seen_files[[file_key]])) {
        seen_files[[file_key]] <- file_path
      } else {
        cat(sprintf("Deleting: %s\n", file_path))
        file.remove(file_path)
        deleted_in <<- union(deleted_in, dirname(file_path))
      }
    }
  }
}

delete_empty_dirs <- function(base_path) {
  # list.dirs returns parents before children; reverse for bottom-up
  all_dirs <- list.dirs(base_path, recursive = TRUE, full.names = TRUE)
  all_dirs <- rev(all_dirs)

  for (dir_path in all_dirs) {
    if (normalizePath(dir_path) == normalizePath(base_path)) {
      next
    }

    contents <- list.files(dir_path, all.files = TRUE, no.. = TRUE)

    if (length(contents) == 0) {
      # unlink needs recursive = TRUE for a dir, and returns 0 on success
      if (unlink(dir_path, recursive = TRUE) == 0) {
        cat(sprintf("Deleted: %s\n", dir_path))
      } else {
        cat(sprintf("Error deleting: %s\n", dir_path))
      }
    }
  }
}

delete_path_log_only_dirs <- function(base_path) {
  all_dirs <- list.dirs(base_path, recursive = TRUE, full.names = TRUE)
  all_dirs <- rev(all_dirs)

  for (dir_path in all_dirs) {
    if (normalizePath(dir_path) == normalizePath(base_path)) {
      next
    }

    contents <- list.files(dir_path, all.files = TRUE, no.. = TRUE)

    # a folder whose only file is a manifest with rows is provenance (revalidated
    # validators, or rows for files retained elsewhere) and stays; only a
    # download_path_log.csv or a rowless manifest marks a folder as empty
    manifest <- file.path(dir_path, "manifest.csv")
    manifest_has_rows <- file.exists(manifest) && length(readLines(manifest, warn = FALSE)) > 1
    if (length(contents) > 0 && !manifest_has_rows &&
          all(contents %in% c("manifest.csv", "download_path_log.csv"))) {
      unlink(file.path(dir_path, contents))
      if (unlink(dir_path, recursive = TRUE) == 0) {
        cat(sprintf("Deleted path-log-only folder: %s\n", dir_path))
      } else {
        cat(sprintf("Error deleting: %s\n", dir_path))
      }
    }
  }
}

remove_duplicate_files_recursive("agreements")
delete_empty_dirs("agreements")
delete_path_log_only_dirs("agreements")
delete_empty_dirs("agreements")

remove_duplicate_files_one_level("sheets")
delete_empty_dirs("sheets")
delete_path_log_only_dirs("sheets")
delete_empty_dirs("sheets")

# the manifests of folders that lost files must say where the bytes now live
snapshot_folders <- function(paths, root) {
  unique(str_extract(paths[startsWith(paths, paste0(root, "/"))], sprintf("^%s/[^/]+", root)))
}
n <- annotate_ghost_rows("agreements", snapshot_folders(deleted_in, "agreements")) +
  annotate_ghost_rows("sheets", snapshot_folders(deleted_in, "sheets"))
cat(sprintf("Annotated %d manifest rows for deleted duplicates.\n", n))

cat("Deduplication complete.\n")
