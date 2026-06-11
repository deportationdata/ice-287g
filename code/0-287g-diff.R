suppressPackageStartupMessages({
  library(digest)
})

get_file_hash <- function(path) {
  digest(file = path, algo = "sha256")
}

get_snapshot <- function(base_path) {
  if (!dir.exists(base_path)) {
    return(data.frame(
      rel_path = character(),
      full_path = character(),
      file_hash = character(),
      stringsAsFactors = FALSE
    ))
  }

  files <- list.files(
    base_path,
    recursive = TRUE,
    full.names = TRUE
  )

  files <- files[!file.info(files)$isdir]
  rel_paths <- substring(files, nchar(base_path) + 2)

  data.frame(
    rel_path = rel_paths,
    full_path = files,
    file_hash = vapply(files, get_file_hash, character(1)),
    stringsAsFactors = FALSE
  )
}

get_snapshot_or_manifest <- function(base_path, manifest_path) {
  if (file.exists(manifest_path)) {
    manifest <- readRDS(manifest_path)
    manifest$full_path <- file.path(base_path, manifest$rel_path)
    return(manifest[, c("rel_path", "full_path", "file_hash")])
  }

  get_snapshot(base_path)
}

old_agreements <- get_snapshot_or_manifest(
  "/tmp/old-287g/agreements",
  "/tmp/old-287g/agreements.rds"
)
new_agreements <- get_snapshot("agreements")

old_sheets <- get_snapshot_or_manifest(
  "/tmp/old-287g/sheets",
  "/tmp/old-287g/sheets.rds"
)
new_sheets <- get_snapshot("sheets")

with_category <- function(snapshot, category) {
  if (nrow(snapshot) > 0) {
    snapshot$category <- category
    return(snapshot)
  }

  data.frame(
    rel_path = character(),
    full_path = character(),
    file_hash = character(),
    category = character(),
    stringsAsFactors = FALSE
  )
}

old_agreements <- with_category(old_agreements, "agreements")
new_agreements <- with_category(new_agreements, "agreements")
old_sheets <- with_category(old_sheets, "sheets")
new_sheets <- with_category(new_sheets, "sheets")

old <- rbind(old_agreements, old_sheets)
new <- rbind(new_agreements, new_sheets)

old_keys <- paste(old$category, old$rel_path, sep = "::")
new_keys <- paste(new$category, new$rel_path, sep = "::")

added_keys <- setdiff(new_keys, old_keys)
removed_keys <- setdiff(old_keys, new_keys)
common_keys <- intersect(new_keys, old_keys)

added <- new[
  match(added_keys, new_keys),
  c("category", "rel_path"),
  drop = FALSE
]
removed <- old[
  match(removed_keys, old_keys),
  c("category", "rel_path"),
  drop = FALSE
]

modified_lines <- unlist(lapply(common_keys, function(key) {
  old_row <- old[old_keys == key, , drop = FALSE][1, ]
  new_row <- new[new_keys == key, , drop = FALSE][1, ]

  if (identical(old_row$file_hash, new_row$file_hash)) {
    return(NULL)
  }

  c(
    paste0("- **", new_row$category, "/", new_row$rel_path, "**"),
    paste0("  - content changed")
  )
}))

lines <- character(0)

if (nrow(added) > 0) {
  lines <- c(lines, paste0("**", nrow(added), " file(s) added:**"))
  lines <- c(
    lines,
    paste0("- ", added$category, "/", added$rel_path)
  )
  lines <- c(lines, "")
}

if (nrow(removed) > 0) {
  lines <- c(lines, paste0("**", nrow(removed), " file(s) removed:**"))
  lines <- c(
    lines,
    paste0("- ", removed$category, "/", removed$rel_path)
  )
  lines <- c(lines, "")
}

if (length(modified_lines) > 0) {
  n_modified <- sum(startsWith(modified_lines, "- **"))
  lines <- c(
    lines,
    paste0("**", n_modified, " file(s) with modified contents:**")
  )
  lines <- c(lines, modified_lines)
  lines <- c(lines, "")
}

if (length(lines) == 0) {
  writeLines("NO_REAL_CHANGES", "/tmp/287g-changes.txt")
} else {
  writeLines(lines, "/tmp/287g-changes.txt")
}
