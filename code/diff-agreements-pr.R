#!/usr/bin/env Rscript
# Diff two match-all-features.parquet files (main vs PR) into a markdown comment

suppressMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)
main_path <- args[1]
pr_path <- args[2]
MARKER <- "<!-- pr-diff-bot -->"
MAX_ROWS <- 100

cat(MARKER, "\n", sep = "")
cat("## `match-all-features.parquet` diff vs `main`\n\n")

if (!file.exists(main_path) || file.size(main_path) == 0) {
  cat("_No `match-all-features.parquet` on `main` - skipping diff._\n")
  quit(status = 0)
}
if (!file.exists(pr_path) || file.size(pr_path) == 0) {
  cat("_No `match-all-features.parquet` on this branch - nothing to diff._\n")
  quit(status = 0)
}

read_clean <- function(path) {
  read_parquet(path) |>
    as_tibble() |>
    select(-any_of("geometry"))
}

main_df <- read_clean(main_path)
pr_df <- read_clean(pr_path)

id_cols <- c("agreement_id", "agency_id", "agreement_lineage_id", "succeeded_by", "sheet_row")

make_key <- function(df) {
  base_key_cols <- intersect(
    c(
      "state",
      "county",
      "agency",
      "support_type",
      "jurisdiction_level",
      "geom_class",
      "match_layer",
      "match_name",
      "facility_city",
      "facility_state",
      # either branch's schema is accepted; intersect() drops absent columns
      "facility_name",
      "county_match",
      "municipality_match",
      "university_name"
    ),
    names(df)
  )
  # ids must not drive pairing (a new id would read as add+remove) and must not
  # be diffed as cells (one id change would report on every row)
  sort_cols <- setdiff(names(df), c("geometry", id_cols))

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

data_cols <- setdiff(intersect(names(main_df), names(pr_df)), c(".key", id_cols))

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
    pr_df |>
      distinct(across(any_of(c(".key", "agency", "state", "county")))),
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

# ids are excluded from pairing and cells above, so an id migration reads as one line
churn <- inner_join(main_df |> select(.key, any_of("agreement_id")),
                    pr_df |> select(.key, any_of("agreement_id")),
                    by = ".key", suffix = c("_main", "_pr"))
if (all(c("agreement_id_main", "agreement_id_pr") %in% names(churn))) {
  cat(sprintf("- **Identity churn:** %d row(s) changed `agreement_id`\n\n",
              sum(as.character(churn$agreement_id_main) != as.character(churn$agreement_id_pr), na.rm = TRUE)))
}

summary_cols <- c("state", "county", "agency", "support_type", "jurisdiction_level", "match_layer")

cat("### Added\n\n")
cat(md_table(added, summary_cols))
cat("\n### Removed\n\n")
cat(md_table(removed, summary_cols))
cat("\n### Modified\n\n")
cat(md_table(
  changes |> arrange(state, county, agency, column),
  c("state", "county", "agency", "column", "main", "pr")
))
