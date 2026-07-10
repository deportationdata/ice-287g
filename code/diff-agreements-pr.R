#!/usr/bin/env Rscript
# Diff two agreements parquet files (main vs PR) and emit a markdown summary
# suitable for a PR comment. Geometry is dropped so location changes surface
# through the non-geometry columns that feed the output. The caller supplies
# the sticky-comment marker so one comment can hold several diffs.
#
# Usage: diff-agreements-pr.R <main_path> <pr_path> [label]

suppressMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)
main_path <- args[1]
pr_path <- args[2]
label <- if (length(args) >= 3) args[3] else "all_agreements_sf.parquet"
MAX_ROWS <- 100

cat(sprintf("## `%s` diff vs `main`\n\n", label))

if (!file.exists(main_path) || file.size(main_path) == 0) {
  cat(sprintf("_No `%s` on `main` - skipping diff._\n", label))
  quit(status = 0)
}
if (!file.exists(pr_path) || file.size(pr_path) == 0) {
  cat(sprintf("_No `%s` on this branch - nothing to diff._\n", label))
  quit(status = 0)
}

read_clean <- function(path) {
  read_parquet(path) |>
    as_tibble() |>
    select(-any_of("geometry"))
}

main_df <- read_clean(main_path)
pr_df <- read_clean(pr_path)

make_key <- function(df) {
  base_key_cols <- intersect(
    c(
      "state",
      "county",
      "agency",
      "support_type",
      "agency_level",
      "geom_class",
      "match_layer",
      "facility_name",
      "facility_city",
      "facility_state",
      "county_match",
      "municipality_match",
      "university_name"
    ),
    names(df)
  )
  sort_cols <- setdiff(names(df), "geometry")

  df |>
    mutate(.base_key = do.call(paste, c(across(all_of(base_key_cols)), sep = " | "))) |>
    arrange(across(all_of(sort_cols))) |>
    group_by(.base_key) |>
    mutate(.dup_id = row_number(), .key = paste(.base_key, .dup_id, sep = " | ")) |>
    ungroup() |>
    select(-.base_key, -.dup_id)
}

main_df <- make_key(main_df)
pr_df <- make_key(pr_df)

added <- pr_df |> anti_join(main_df, by = ".key")
removed <- main_df |> anti_join(pr_df, by = ".key")
common <- intersect(main_df$.key, pr_df$.key)

data_cols <- setdiff(intersect(names(main_df), names(pr_df)), ".key")

to_char_long <- function(df) {
  df |>
    filter(.key %in% common) |>
    select(.key, all_of(data_cols)) |>
    mutate(across(-.key, as.character)) |>
    pivot_longer(-.key, names_to = "column", values_to = "value")
}

changes <- inner_join(
  to_char_long(main_df) |> rename(main = value),
  to_char_long(pr_df) |> rename(pr = value),
  by = c(".key", "column")
) |>
  filter(
    (is.na(main) != is.na(pr)) |
      (!is.na(main) & !is.na(pr) & main != pr)
  ) |>
  left_join(
    pr_df |> distinct(.key, agency, state, county),
    by = ".key"
  )

md_escape <- function(x) {
  x <- replace_na(as.character(x), "")
  x <- str_replace_all(x, "\\|", "\\\\|")
  str_replace_all(x, "\\n", " ")
}

md_table <- function(df, cols, max_rows = MAX_ROWS) {
  if (nrow(df) == 0) return("_(none)_\n")

  cols <- intersect(cols, names(df))
  df_show <- df |> slice_head(n = max_rows) |> select(all_of(cols))
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  separator <- paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  rows <- vapply(seq_len(nrow(df_show)), function(i) {
    paste0("| ", paste(md_escape(unlist(df_show[i, ])), collapse = " | "), " |")
  }, character(1))
  out <- paste(c(header, separator, rows), collapse = "\n")

  if (nrow(df) > max_rows) {
    out <- paste0(out, "\n\n_Showing first ", max_rows, " of ", nrow(df), " rows._")
  }

  paste0(out, "\n")
}

cat(sprintf("- **Added:** %d agreement row(s)\n", nrow(added)))
cat(sprintf("- **Removed:** %d agreement row(s)\n", nrow(removed)))
cat(sprintf(
  "- **Modified:** %d cell change(s) across %d agreement row(s)\n\n",
  nrow(changes),
  n_distinct(changes$.key)
))

summary_cols <- c("state", "county", "agency", "support_type", "agency_level", "match_layer")

cat("### Added\n\n")
cat(md_table(added, summary_cols))
cat("\n### Removed\n\n")
cat(md_table(removed, summary_cols))
cat("\n### Modified\n\n")
cat(md_table(
  changes |> arrange(state, county, agency, column),
  c("state", "county", "agency", "column", "main", "pr")
))
