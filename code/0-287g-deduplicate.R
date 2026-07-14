library(tidyverse)
library(digest)

trees <- c("agreements", "sheets")

# Delete files whose content duplicates a same-group file from an earlier
# snapshot. Both trees are laid out <tree>/<tree>_YYYYMMDD_HHMMSS/..., so a
# file's group is its directory with the snapshot component removed:
# agreements/<state>/<agency> for agreements (two agencies with byte-identical
# files each keep their copy), the tree root for sheets. Files in different
# groups are never compared. The snapshot timestamp is parsed and ordered on
# explicitly, so the oldest copy wins by date rather than by path sort order
# (a file with no parseable timestamp sorts last and loses to any dated copy).
tibble(path = list.files(trees, recursive = TRUE, full.names = TRUE)) |>
  mutate(
    snapshot = ymd_hms(str_extract(path, "[0-9]{8}_[0-9]{6}")),
    group = str_remove(dirname(path), "/[^/]+"),
    hash = map_chr(path, function(p) {
      tryCatch(digest(file = p, algo = "sha256"), error = function(e) NA_character_)
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

# Prune empty directories. The rev() is load-bearing: list.dirs returns every
# parent before its descendants, so walking in reverse visits each directory
# only after all of its children. Combined with checking emptiness at deletion
# time (not up front), a parent whose only child was just pruned goes too.
# This re-scans the filesystem rather than deriving dirs from the deletions
# above, because it must also catch dirs that were already empty (e.g. an
# agency folder whose download failed).
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
