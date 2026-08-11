library(digest)

# --- Helper function to get file hash ---

get_file_hash <- function(filepath) {
  file_hash <- digest(file = filepath, algo = "sha256")

  return(file_hash)
}

# --- Remove duplicate files recursively ---

remove_duplicate_files_recursive <- function(base_path) {
  seen_files <- list()

  # walk all files depth-first (sorted for determinism)
  all_files <- list.files(base_path, recursive = TRUE, full.names = TRUE)
  all_files <- sort(all_files)

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

    if (is.null(seen_files[[file_hash]])) {
      seen_files[[file_hash]] <- file_path
    } else {
      cat(sprintf("Deleting: %s\n", file_path))
      file.remove(file_path)
    }
  }
}

# --- Remove duplicate files one level deep ---

remove_duplicate_files_one_level <- function(base_path) {
  seen_files <- list()

  # only look one level of subfolders deep
  subfolders <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)
  subfolders <- sort(subfolders)

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

      if (is.null(seen_files[[file_hash]])) {
        seen_files[[file_hash]] <- file_path
      } else {
        cat(sprintf("Deleting: %s\n", file_path))
        file.remove(file_path)
      }
    }
  }
}

# --- Delete empty directories ---

delete_empty_dirs <- function(base_path) {
  # list.dirs returns parents before children; reverse for bottom-up
  all_dirs <- list.dirs(base_path, recursive = TRUE, full.names = TRUE)
  all_dirs <- rev(all_dirs)

  for (dir_path in all_dirs) {
    # skip the root itself
    if (normalizePath(dir_path) == normalizePath(base_path)) {
      next
    }

    contents <- list.files(dir_path, all.files = TRUE, no.. = TRUE)

    if (length(contents) == 0) {
      tryCatch(
        {
          unlink(dir_path, recursive = FALSE)
          cat(sprintf("Deleted: %s\n", dir_path))
        },
        error = function(e) {
          cat(sprintf("Error deleting: %s\n", dir_path))
        }
      )
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

    if (identical(contents, "download_path_log.csv")) {
      unlink(file.path(dir_path, "download_path_log.csv"))
      unlink(dir_path, recursive = FALSE)
      cat(sprintf("Deleted path-log-only folder: %s\n", dir_path))
    }
  }
}

# --- Run deduplication pipeline ---

remove_duplicate_files_recursive("agreements")
delete_empty_dirs("agreements")
delete_path_log_only_dirs("agreements")
delete_empty_dirs("agreements")

remove_duplicate_files_one_level("sheets")
delete_empty_dirs("sheets")
delete_path_log_only_dirs("sheets")
delete_empty_dirs("sheets")

cat("Deduplication complete.\n")
