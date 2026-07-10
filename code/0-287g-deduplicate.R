library(digest)

# --- Helper function to get file hash ---

get_file_hash <- function(filepath) {
  digest(file = filepath, algo = "sha256")
}

# --- Remove duplicate files ---

# Removes files whose content duplicates an earlier file (sorted order, so the
# oldest snapshot wins) within the same dedup group. `group_fn` maps a file
# path to its scope: files in different groups are never compared, so two
# different agencies that receive byte-identical agreement files each keep
# their copy.
remove_duplicate_files <- function(base_path, group_fn = function(path) "") {
  seen_files <- list()

  all_files <- sort(list.files(base_path, recursive = TRUE, full.names = TRUE))

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

    key <- paste(group_fn(file_path), file_hash, sep = "::")

    if (is.null(seen_files[[key]])) {
      seen_files[[key]] <- file_path
    } else {
      cat(sprintf("Deleting: %s\n", file_path))
      file.remove(file_path)
    }
  }
}

# agreements/<agreements_TIMESTAMP>/<state>/<agency>/<file>: dedup within the
# same state/agency across snapshots, never across agencies
agreement_group <- function(path) {
  dirname(sub("^agreements/[^/]+/", "", path))
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
          # unlink() needs recursive = TRUE to remove a directory at all;
          # the empty check above ensures nothing else is deleted
          unlink(dir_path, recursive = TRUE)
          cat(sprintf("Deleted: %s\n", dir_path))
        },
        error = function(e) {
          cat(sprintf("Error deleting: %s\n", dir_path))
        }
      )
    }
  }
}

# --- Run deduplication pipeline ---

remove_duplicate_files("agreements", agreement_group)
delete_empty_dirs("agreements")

remove_duplicate_files("sheets")
delete_empty_dirs("sheets")

cat("Deduplication complete.\n")
