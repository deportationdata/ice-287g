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
  rel_paths <- list.files(
    base_path,
    recursive = TRUE,
    full.names = FALSE
  )

  data.frame(
    rel_path = rel_paths,
    full_path = files,
    file_hash = vapply(files, get_file_hash, character(1)),
    stringsAsFactors = FALSE
  )
}

old_agreements <- get_snapshot("/tmp/old-287g/agreements")
new_agreements <- get_snapshot("agreements")

old_sheets <- get_snapshot("/tmp/old-287g/sheets")
new_sheets <- get_snapshot("sheets")

# add category column, handling empty data frames
if (nrow(old_agreements) > 0) {
  old_agreements$category <- "agreements"
} else {
  old_agreements <- data.frame(
    rel_path = character(),
    full_path = character(),
    file_hash = character(),
    category = character(),
    stringsAsFactors = FALSE
  )
}

if (nrow(old_sheets) > 0) {
  old_sheets$category <- "sheets"
} else {
  old_sheets <- data.frame(
    rel_path = character(),
    full_path = character(),
    file_hash = character(),
    category = character(),
    stringsAsFactors = FALSE
  )
}

if (nrow(new_agreements) > 0) {
  new_agreements$category <- "agreements"
} else {
  new_agreements <- data.frame(
    rel_path = character(),
    full_path = character(),
    file_hash = character(),
    category = character(),
    stringsAsFactors = FALSE
  )
}

if (nrow(new_sheets) > 0) {
  new_sheets$category <- "sheets"
} else {
  new_sheets <- data.frame(
    rel_path = character(),
    full_path = character(),
    file_hash = character(),
    category = character(),
    stringsAsFactors = FALSE
  )
}

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
