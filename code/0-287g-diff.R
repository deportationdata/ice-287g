library(tidyverse)
library(digest)

trees <- c("agreements", "sheets")

old <- tibble(category = trees) |>
  mutate(
    files = map(category, function(tree) {
      manifest_path <- file.path("/tmp/old-287g", paste0(tree, ".rds"))
      base_path <- file.path("/tmp/old-287g", tree)

      if (file.exists(manifest_path)) {
        readRDS(manifest_path) |>
          as_tibble() |>
          select(rel_path, file_hash)
      } else {
        tibble(
          path = list.files(base_path, recursive = TRUE, full.names = TRUE)
        ) |>
          mutate(
            rel_path = str_remove(path, fixed(paste0(base_path, "/"))),
            file_hash = map_chr(path, function(p) {
              digest(file = p, algo = "md5")
            })
          ) |>
          select(rel_path, file_hash)
      }
    })
  ) |>
  unnest(files)

new <- tibble(
  path = list.files(trees, recursive = TRUE, full.names = TRUE)
) |>
  mutate(
    category = str_extract(path, "^[^/]+"),
    rel_path = str_remove(path, "^[^/]+/"),
    file_hash = map_chr(path, function(p) digest(file = p, algo = "md5"))
  ) |>
  select(category, rel_path, file_hash)

# One markdown section per change type, in a fixed order, each file listed
# once as a bullet (path-sorted) with modified files carrying a sub-bullet.
sections <- full_join(
  old,
  new,
  by = c("category", "rel_path"),
  suffix = c("_old", "_new")
) |>
  mutate(
    status = case_when(
      is.na(file_hash_old) ~ "added",
      is.na(file_hash_new) ~ "removed",
      file_hash_old != file_hash_new ~ "modified"
    )
  ) |>
  filter(!is.na(status)) |>
  mutate(
    status = factor(status, levels = c("added", "removed", "modified")),
    label = if_else(
      status == "modified",
      "with modified contents",
      as.character(status)
    ),
    bullet = if_else(
      status == "modified",
      paste0("- **", category, "/", rel_path, "**\n  - content changed"),
      paste0("- ", category, "/", rel_path)
    )
  ) |>
  arrange(status, category, rel_path) |>
  summarise(
    section = paste0(
      "**",
      n(),
      " file(s) ",
      first(label),
      ":**\n",
      paste(bullet, collapse = "\n"),
      "\n"
    ),
    .by = status
  ) |>
  pull(section)

if (length(sections) == 0) {
  writeLines("NO_REAL_CHANGES", "/tmp/287g-changes.txt")
} else {
  writeLines(sections, "/tmp/287g-changes.txt")
}
