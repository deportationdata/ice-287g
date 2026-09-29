#!/usr/bin/env Rscript
# Diff two agreements-sf.parquet files (main vs PR) into a markdown comment

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
cat("## `agreements-sf.parquet` diff vs `main`\n\n")

if (!file.exists(main_path) || file.size(main_path) == 0) {
  cat("_No `agreements-sf.parquet` on `main` - skipping diff._\n")
  quit(status = 0)
}
if (!file.exists(pr_path) || file.size(pr_path) == 0) {
  cat("_No `agreements-sf.parquet` on this branch - nothing to diff._\n")
  quit(status = 0)
}

read_clean <- function(path) {
  read_parquet(path) |>
    as_tibble() |>
    select(-any_of("geometry")) |>
    # dates were published without the _date suffix until 2026-09-29
    rename(any_of(c(signed_date = "signed", first_appeared_date = "first_appeared", last_appeared_date = "last_appeared",
                    removed_by_date = "removed_by", addendum_signed_date = "addendum_signed")))
}

main_df <- read_clean(main_path)
pr_df <- read_clean(pr_path)

id_cols <- c("agreement_id", "agency_id", "agreement_lineage_id", "succeeded_by", "latest_sheet_row", "latest_sheet", "latest_sheet_url")
# the roster end date moves on every active row with each new sheet, so it is reported as one line
roster_cols <- c("last_appeared_date")

make_key <- function(df) {
  base_key_cols <- intersect(
    c(
      "state",
      "ice_county",
      "agency",
      "support_type",
      "jurisdiction_level",
      "geometry_type",
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
  # ids stay out of the sort, so rows sharing a key are numbered by their data
  sort_cols <- setdiff(names(df), c("geometry", id_cols))

  df |>
    mutate(.base_key = do.call(paste, c(across(all_of(base_key_cols)), sep = " | "))) |>
    arrange(across(all_of(sort_cols))) |>
    group_by(.base_key) |>
    mutate(.dup_id = row_number(), .key = paste(.base_key, .dup_id, sep = " | ")) |>
    ungroup() |>
    select(-.base_key, -.dup_id)
}

# rows pair in three passes, each over the rows the one before left unpaired:
# 1. the same agreement_id, which is built from state, agency, model and signing date
# 2. make_key's descriptive key, so a corrected signing date pairs and counts as churn
# 3. state, ICE's county, model and signing date where exactly one unpaired row on each side
#    has them, so a renamed agency, whose name and id change together, pairs and counts as churn
shared_ids <- intersect(main_df$agreement_id, pr_df$agreement_id)
shared_ids <- shared_ids[!shared_ids %in% c(main_df$agreement_id[duplicated(main_df$agreement_id)],
                                            pr_df$agreement_id[duplicated(pr_df$agreement_id)])]
main_df <- bind_rows(
  main_df |> filter(agreement_id %in% shared_ids) |> mutate(.key = paste("id", agreement_id, sep = " | ")),
  main_df |> filter(!agreement_id %in% shared_ids) |> make_key()
)
pr_df <- bind_rows(
  pr_df |> filter(agreement_id %in% shared_ids) |> mutate(.key = paste("id", agreement_id, sep = " | ")),
  pr_df |> filter(!agreement_id %in% shared_ids) |> make_key()
)

rename_cols <- intersect(c("state", "ice_county", "support_type", "signed_date"), intersect(names(main_df), names(pr_df)))
main_df <- main_df |>
  mutate(.rename = if_else(!.key %in% pr_df$.key & !is.na(signed_date),
                           do.call(paste, c("renamed", across(all_of(rename_cols)), sep = " | ")), NA))
pr_df <- pr_df |>
  mutate(.rename = if_else(!.key %in% main_df$.key & !is.na(signed_date),
                           do.call(paste, c("renamed", across(all_of(rename_cols)), sep = " | ")), NA))
renamed <- intersect(
  main_df |> count(.rename) |> filter(n == 1, !is.na(.rename)) |> pull(.rename),
  pr_df |> count(.rename) |> filter(n == 1, !is.na(.rename)) |> pull(.rename)
)
main_df <- main_df |> mutate(.key = if_else(.rename %in% renamed, .rename, .key), .rename = NULL)
pr_df <- pr_df |> mutate(.key = if_else(.rename %in% renamed, .rename, .key), .rename = NULL)

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

roster <- changes |> filter(column %in% roster_cols)
changes <- changes |> filter(!column %in% roster_cols)

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

if (nrow(roster) > 0) {
  moves <- roster |>
    count(column, main, pr, name = "n") |>
    arrange(desc(n)) |>
    mutate(text = sprintf("%s to %s on %d row(s)", replace_na(main, "blank"), replace_na(pr, "blank"), n))
  cat(sprintf("- **Roster end date:** `%s` moved %s\n\n",
              paste(unique(moves$column), collapse = "`, `"),
              paste(head(moves$text, 5), collapse = "; ")))
}

# rows the second and third passes paired have different ids; ids stay out of the cells, so they are counted in one line
churn <- inner_join(main_df |> select(.key, any_of("agreement_id")),
                    pr_df |> select(.key, any_of("agreement_id")),
                    by = ".key", suffix = c("_main", "_pr"))
if (all(c("agreement_id_main", "agreement_id_pr") %in% names(churn))) {
  cat(sprintf("- **Identity churn:** %d row(s) changed `agreement_id`\n\n",
              sum(as.character(churn$agreement_id_main) != as.character(churn$agreement_id_pr), na.rm = TRUE)))
}

summary_cols <- c("state", "ice_county", "county", "agency", "support_type", "jurisdiction_level", "match_layer")

cat("### Added\n\n")
cat(md_table(added, summary_cols))
cat("\n### Removed\n\n")
cat(md_table(removed, summary_cols))
cat("\n### Modified\n\n")
cat(md_table(
  changes |> arrange(state, county, agency, column),
  c("state", "county", "agency", "column", "main", "pr")
))
