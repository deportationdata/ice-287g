library(tidyverse)
library(digest)

trees <- c("agreements", "sheets")

# Delete files whose content duplicates a same-group file from earlier snapshot
tibble(path = list.files(trees, recursive = TRUE, full.names = TRUE)) |>
  mutate(
    snapshot = ymd_hms(str_extract(path, "[0-9]{8}_[0-9]{6}")),
    group = str_remove(dirname(path), "/[^/]+"),
    hash = map_chr(path, function(p) {
      tryCatch(digest(file = p, algo = "sha256"), error = function(e) {
        NA_character_
      })
    })
  ) |>
  filter(!is.na(hash)) |>
  arrange(snapshot, path) |>
  group_by(group, hash) |>
  filter(row_number() > 1) |> # the earliest-snapshot file per group+hash survives
  ungroup() |>
  pull(path) |>
  walk(function(p) {
    cat(sprintf("Deleting: %s\n", p))
    file.remove(p)
  })

# Prune empty directories
list.dirs(trees, recursive = TRUE, full.names = TRUE) |>
  setdiff(trees) |>
  rev() |>
  walk(function(d) {
    if (length(list.files(d, all.files = TRUE, no.. = TRUE)) == 0) {
      # unlink() needs recursive = TRUE to remove a directory at all;
      # the empty check above ensures nothing else is deleted
      unlink(d, recursive = TRUE)
      cat(sprintf("Deleted: %s\n", d))
    }
  })

cat("Deduplication complete.\n")
