#!/usr/bin/env Rscript
# Diff agreements-sf.parquet on main vs a PR into three PR comments in the output directory:
# rows.md (row changes), merge.md (ICE's row beside the pipeline's match), stats.md (topline counts, QA)

suppressMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)
main_path <- args[1]
pr_path <- args[2]
# review/agreement-review.csv on each side; main has none before the first build that writes it
main_review_path <- args[3]
pr_review_path <- args[4]
# review/qa-summary.csv on each side
main_qa_path <- args[5]
pr_qa_path <- args[6]
out_dir <- args[7]
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

markers <- c(rows = "<!-- pr-diff-rows -->", merge = "<!-- pr-diff-merge -->", stats = "<!-- pr-diff-stats -->")
table_rows <- 150
review_rows <- 80
# the workflow passes the commit the comments describe
head_sha <- Sys.getenv("PR_HEAD_SHA", "")
stamp <- sprintf("_Updated %s UTC%s._", format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC"),
                 if (head_sha != "") paste0(" for ", substr(head_sha, 1, 7)) else "")

write_comment <- function(which, lines) {
  writeLines(c(markers[[which]], lines, "", stamp), file.path(out_dir, paste0(which, ".md")))
}

if (!file.exists(main_path) || file.size(main_path) == 0) {
  for (w in names(markers)) write_comment(w, "_No `agreements-sf.parquet` on `main` - skipping diff._")
  quit(status = 0)
}
if (!file.exists(pr_path) || file.size(pr_path) == 0) {
  for (w in names(markers)) write_comment(w, "_No `agreements-sf.parquet` on this branch - nothing to diff._")
  quit(status = 0)
}

read_clean <- function(path) {
  read_parquet(path) |>
    as_tibble() |>
    select(-any_of("geometry")) |>
    # older files lack the _date suffix
    rename(any_of(c(signed_date = "signed", first_appeared_date = "first_appeared", last_appeared_date = "last_appeared",
                    removed_by_date = "removed_by", addendum_signed_date = "addendum_signed")))
}
# the review file carries the match detail the published file leaves out, one row per agreement
read_review <- function(path) {
  if (is.na(path) || !file.exists(path) || file.size(path) == 0) return(NULL)
  read_csv_arrow(path) |>
    as_tibble() |>
    # both sides type alike: a count and an all-blank column read as character
    mutate(across(everything(), as.character))
}
read_qa <- function(path) {
  if (is.na(path) || !file.exists(path) || file.size(path) == 0) return(NULL)
  read_csv_arrow(path) |> as_tibble() |> mutate(across(everything(), as.character))
}

main_pub <- read_clean(main_path)
pr_pub <- read_clean(pr_path)
published_cols <- intersect(names(main_pub), names(pr_pub))
main_review <- read_review(main_review_path)
pr_review <- read_review(pr_review_path)
main_df <- if (is.null(main_review)) main_pub else main_pub |> left_join(main_review, by = "agreement_id")
pr_df <- if (is.null(pr_review)) pr_pub else pr_pub |> left_join(pr_review, by = "agreement_id")

id_cols <- c("agreement_id", "agency_id", "agreement_lineage_id", "succeeded_by", "latest_sheet_row", "latest_sheet", "latest_sheet_url")
# the roster end date moves on every active row with each new sheet, so it is reported as one line
roster_cols <- c("last_appeared_date")
# the columns the merge comment covers: ICE's row, the agency match and the jurisdiction; a change
# in one of them puts the agreement in the re-matched table
review_cols <- c(
  "agency", "state", "ice_county", "ice_type", "ORI9",
  "jurisdiction", "jurisdiction_facilities", "jurisdiction_level", "jurisdiction_level_source",
  "place", "place_type", "place_geoid", "county", "county_fips", "geoid", "geoid_type",
  "geometry_type", "geometry_vintage",
  "match_layer", "match_name", "match_type", "match_quality", "n_features", "facility_city",
  "review_reason", "ori_source", "ori_roster_name", "ori_roster_county", "ori_disagreements",
  "manual_reason", "manual_note"
)
status_cols <- c("status", "removed_by_date", "removed_by_source", "removal_flag")
link_cols <- c("moa", "addendum", "addendum_second")

make_key <- function(df) {
  # the match columns stay out: a re-matched agreement is a modification, not a removal and an addition
  base_key_cols <- intersect(
    c("state", "ice_county", "agency", "support_type", "jurisdiction_level", "geometry_type"),
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

# rows pair in three passes over the rows still unpaired: same agreement_id, then make_key's key
# (a corrected signing date), then a unique state/county/model/date match (a renamed agency)
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

# every cell that differs between the paired rows, published and review columns alike
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
      distinct(across(any_of(c(".key", "agency", "state", "ice_county", "county", "support_type", "signed_date", "signed_source")))),
    by = ".key"
  )

roster <- changes |> filter(column %in% roster_cols)
changes <- changes |> filter(!column %in% roster_cols)
# modified agreements whose match or jurisdiction moved go into the merge comment
recheck_keys <- unique(changes$.key[changes$column %in% review_cols])
recheck <- pr_df |> filter(.key %in% recheck_keys)

# rows paired by the second and third passes have different ids, counted in one line
churn <- inner_join(main_df |> select(.key, any_of("agreement_id")),
                    pr_df |> select(.key, any_of("agreement_id")),
                    by = ".key", suffix = c("_main", "_pr"))
n_churn <- if (all(c("agreement_id_main", "agreement_id_pr") %in% names(churn))) {
  sum(as.character(churn$agreement_id_main) != as.character(churn$agreement_id_pr), na.rm = TRUE)
} else NA_integer_

newest_sheet <- function(df) {
  if (!"latest_sheet" %in% names(df)) return(NA_character_)
  df |> filter(!is.na(latest_sheet)) |> count(latest_sheet, sort = TRUE) |> slice_head(n = 1) |> pull(latest_sheet)
}
sheet_line <- sprintf("ICE's newest sheet: `%s` on this branch, `%s` on `main`.", newest_sheet(pr_df), newest_sheet(main_df))

md_escape <- function(x) {
  x <- replace_na(as.character(x), "")
  x <- str_replace_all(x, "\\|", "\\\\|")
  str_replace_all(x, "\\n", " ")
}

md_table <- function(df, cols = names(df), max_rows = table_rows, more = "") {
  if (nrow(df) == 0) return("_(none)_")

  cols <- intersect(cols, names(df))
  # a date would print as its day number once a row is unlisted
  df_show <- df |> slice_head(n = max_rows) |> select(all_of(cols)) |> mutate(across(everything(), as.character))
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  separator <- paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  rows <- vapply(seq_len(nrow(df_show)), function(i) {
    paste0("| ", paste(md_escape(unlist(df_show[i, ])), collapse = " | "), " |")
  }, character(1))
  out <- c(header, separator, rows)

  if (nrow(df) > max_rows) {
    out <- c(out, "", sprintf("_Showing first %d of %d rows%s._", max_rows, nrow(df), more))
  }
  out
}

# "x (y)" when y is filled, x alone otherwise
with_paren <- function(x, y) if_else(is.na(y) | y == "", x, paste0(coalesce(x, ""), " (", y, ")"))
blank_na <- function(x) replace_na(as.character(x), "")
# pieces joined per row, empties dropped
join_pieces <- function(sep, ...) {
  m <- cbind(...)
  apply(m, 1, \(r) paste(r[!is.na(r) & r != ""], collapse = sep))
}
br_join <- function(...) join_pieces("<br>", ...)
dot_join <- function(...) join_pieces(" · ", ...)
# a document url as a short link; "pending" and blanks stay as they are
doc_link <- function(url) {
  if_else(!is.na(url) & str_detect(url, "^https?://"), paste0("[pdf](", url, ")"), blank_na(url))
}
signed_text <- function(df) with_paren(as.character(df$signed_date), df$signed_source)
# a county name reduced to what both ICE and the census spell the same way
county_key <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\bparish\\b", "county") |>
    str_remove_all("\\bcounty\\b|[^a-z0-9 ;]") |>
    str_squish()
}
plural <- function(n, noun) sprintf("%d %s%s", n, noun, if_else(n == 1, "", "s"))

status_changes <- changes |> filter(column == "status")
status_keys <- status_changes$.key
marked_removed <- status_changes |> filter(main == "Active", pr == "Removed")
other_status <- status_changes |> anti_join(marked_removed, by = ".key")
# a document url arriving where there was none, or "pending"; a changed url is an ordinary cell change
linked <- changes |>
  filter(column %in% link_cols, (is.na(main) | main == "pending"), !is.na(pr), pr != "pending")
other_changes <- changes |>
  filter(column %in% published_cols) |>
  anti_join(linked, by = c(".key", "column")) |>
  filter(!(.key %in% status_keys & column %in% status_cols))

ice_cols <- c(state = "state", `ICE county` = "ice_county", agency = "agency", `ICE type` = "ice_type", model = "support_type")
ice_row <- function(df) {
  df |>
    mutate(signed = signed_text(df), `first appeared` = with_paren(as.character(first_appeared_date), first_appeared_source),
           MOA = doc_link(moa)) |>
    select(all_of(ice_cols), signed, `first appeared`, MOA) |>
    arrange(state, `ICE county`, agency)
}
status_row <- function(df) {
  df |>
    left_join(status_changes |> select(.key, `status on main` = main, `status on branch` = pr), by = ".key") |>
    mutate(signed = signed_text(df), `removed by` = with_paren(as.character(removed_by_date), removed_by_source), flag = removal_flag) |>
    select(all_of(ice_cols), signed, `status on main`, `status on branch`, `removed by`, flag) |>
    arrange(state, `ICE county`, agency)
}

rows_md <- c(
  "## Row changes in `agreements-sf.parquet` vs `main`",
  "",
  paste0("_", sheet_line, " Published columns only; how each new or re-matched row was matched is in the ICE-vs-pipeline comment._"),
  "",
  sprintf("- **Added:** %s", plural(nrow(added), "agreement row")),
  sprintf("- **Marked removed by ICE:** %s (`status` Active to Removed)", plural(nrow(marked_removed), "row")),
  sprintf("- **Other status changes:** %s", plural(nrow(other_status), "row")),
  sprintf("- **Dropped from the file:** %s", plural(nrow(removed), "row")),
  sprintf("- **Documents linked:** %s (an MOA or addendum url where there was none)", plural(nrow(linked), "row")),
  sprintf("- **Other cell changes:** %d across %s", nrow(other_changes), plural(n_distinct(other_changes$.key), "row")),
  if (nrow(roster) > 0) {
    moves <- roster |>
      count(column, main, pr, name = "n") |>
      arrange(desc(n)) |>
      mutate(text = sprintf("%s to %s on %s", replace_na(main, "blank"), replace_na(pr, "blank"), plural(n, "row")))
    sprintf("- **Roster end date:** `%s` moved %s", paste(unique(moves$column), collapse = "`, `"), paste(head(moves$text, 5), collapse = "; "))
  },
  if (!is.na(n_churn)) sprintf("- **Identity churn:** %s changed `agreement_id`", plural(n_churn, "row")),
  "",
  "### Added",
  "",
  md_table(ice_row(added)),
  "",
  "### Marked removed by ICE",
  "",
  md_table(status_row(pr_df |> filter(.key %in% marked_removed$.key))),
  "",
  "### Other status changes",
  "",
  md_table(status_row(pr_df |> filter(.key %in% other_status$.key))),
  "",
  "### Dropped from the file",
  "",
  md_table(removed |> mutate(signed = signed_text(removed)) |> select(all_of(ice_cols), signed, status) |> arrange(state, `ICE county`, agency)),
  "",
  "### Documents linked",
  "",
  md_table(
    linked |>
      mutate(signed = signed_text(linked), link = doc_link(pr)) |>
      arrange(state, ice_county, agency, column) |>
      select(state, `ICE county` = ice_county, agency, model = support_type, signed, column, link)
  ),
  "",
  "### Other cell changes",
  "",
  md_table(
    other_changes |>
      mutate(signed = signed_text(other_changes)) |>
      arrange(state, ice_county, agency, column) |>
      # a matched facility list runs to thousands of characters
      mutate(across(c(main, pr), \(x) str_trunc(x, 120))) |>
      select(state, `ICE county` = ice_county, agency, model = support_type, signed, column, main, branch = pr)
  )
)
write_comment("rows", rows_md)

# one row per agreement with ICE's row, the pipeline's reading and the match beside each other;
# rows with a review reason or a note sort first
merge_rows <- function(df) {
  # columns an older schema lacks read as blank
  absent <- setdiff(review_cols, names(df))
  if (length(absent)) df[absent] <- NA_character_
  df <- df |>
    mutate(across(all_of(review_cols), as.character)) |>
    mutate(
      n_names = str_count(coalesce(match_name, ""), "; ") + 1,
      first_names = str_split_fixed(coalesce(match_name, ""), "; ", 4)[, 1:3, drop = FALSE] |>
        apply(1, \(r) paste(r[r != ""], collapse = "; ")),
      matched_to = case_when(
        is.na(match_layer) ~ NA_character_,
        is.na(match_name) ~ match_layer,
        n_names > 3 ~ sprintf("%s: %s (+%d more)", match_layer, first_names, n_names - 3),
        TRUE ~ paste0(match_layer, ": ", first_names)
      ),
      roster = case_when(
        is.na(ori_source) ~ NA_character_,
        is.na(ori_roster_name) ~ ori_source,
        TRUE ~ paste0(ori_source, ": ", with_paren(ori_roster_name, ori_roster_county))
      ),
      county_differs = !is.na(ice_county) & !is.na(county) &
        !str_detect(paste0("; ", county_key(county), "; "), fixed(paste0("; ", county_key(ice_county), "; "))),
      note_county = case_when(
        is.na(ice_county) ~ "no ICE county",
        county_differs ~ "ICE county differs"
      ),
      # the level ICE's TYPE alone would give, as 2-make-agreements.R reads it
      ice_type_level = case_when(
        str_to_lower(ice_type) %in% c("state agency", "state") ~ "State",
        str_to_lower(ice_type) == "county" ~ "County",
        str_to_lower(ice_type) == "municipality" ~ "Municipal"
      ),
      # a level that overrides a printed TYPE is worth a look; one that agrees with it is not
      note_level = case_when(
        jurisdiction_level_source == "Name, no ICE type" ~ "no ICE type",
        is.na(ice_type) | coalesce(ice_type_level == jurisdiction_level, FALSE) ~ NA_character_,
        jurisdiction_level_source == "Name pattern" ~ "level from name pattern",
        jurisdiction_level_source == "Manual list" ~ "level from manual list"
      ),
      note_level = if_else(is.na(note_level), NA_character_, with_paren(note_level, paste("ICE type:", ice_type))),
      note_geometry = if_else(is.na(geometry_type) & status == "Active", "no geometry", NA_character_),
      note_ori = if_else(is.na(ORI9) & status == "Active", "no ORI", NA_character_),
      note_manual = if_else(is.na(manual_note), NA_character_, paste0("manual: ", manual_note))
    ) |>
    unite(notes, note_county, note_level, note_geometry, note_ori, note_manual, sep = "; ", na.rm = TRUE) |>
    mutate(
      `ICE says` = br_join(
        paste0("**", blank_na(agency), "**"),
        dot_join(ice_county, state),
        dot_join(ice_type, support_type, signed_text(pick(everything())))
      ),
      `we say` = br_join(
        paste0("**", blank_na(jurisdiction), "** · ", with_paren(jurisdiction_level, jurisdiction_level_source)),
        dot_join(county, with_paren(place, place_type), paste("geoid", with_paren(geoid, geoid_type)),
                 replace_na(geometry_type, "no geometry")),
        if_else(is.na(ORI9), "no ORI9", paste0("ORI9 ", ORI9, if_else(is.na(roster), "", paste0(" from ", roster))))
      ),
      `matched via` = br_join(matched_to, dot_join(match_type, match_quality)),
      check = dot_join(review_reason, notes),
      .flagged = check != ""
    ) |>
    arrange(desc(.flagged), state, agency)
  df
}
merge_table <- function(df, changed = NULL) {
  more <- "; every agreement's detail is in `review/agreement-review.csv` on this branch"
  if (nrow(df) == 0) return("_(none)_")
  rows <- merge_rows(df)
  if (!is.null(changed)) {
    # what moved, so the reader sees the re-match rather than hunting for it
    moved <- changed |>
      mutate(text = sprintf("%s: %s to %s", column, str_trunc(replace_na(main, "blank"), 60), str_trunc(replace_na(pr, "blank"), 60))) |>
      group_by(.key) |>
      summarise(changed = paste(text, collapse = "; "), .groups = "drop")
    rows <- rows |>
      left_join(moved, by = ".key") |>
      mutate(check = br_join(paste0("changed ", changed), check))
  }
  md_table(rows |> select(`ICE says`, `we say`, `matched via`, check), max_rows = review_rows, more = more)
}
quality_line <- function(df) {
  if (nrow(df) == 0) return("match quality n/a")
  if (!"match_quality" %in% names(df)) return("match quality unknown without a review file")
  q <- df |> count(quality = replace_na(as.character(match_quality), "none"), sort = TRUE)
  paste("match quality", paste(sprintf("%s %d", q$quality, q$n), collapse = ", "))
}
n_flagged <- function(df) if (nrow(df) == 0) 0L else sum(merge_rows(df)$.flagged)

merge_md <- c(
  "## ICE's row beside the pipeline's match",
  "",
  "_For every agreement this PR adds or re-matches: what ICE's sheet says on the left, what the pipeline made of it on the right. Did it pick the right agency, jurisdiction, geometry and ORI? Rows with a review reason or a note sort first._",
  "",
  sprintf("- **Added:** %s; %s; %s", plural(nrow(added), "agreement"), quality_line(added), plural(n_flagged(added), "flagged row")),
  sprintf("- **Re-matched:** %s whose match or jurisdiction columns changed; %s",
          plural(nrow(recheck), "agreement"), plural(n_flagged(recheck), "flagged row")),
  if (is.null(main_review) && !is.null(pr_review)) {
    "- _`review/agreement-review.csv` is not on `main` yet, so re-matching is judged on the published columns only._"
  },
  "",
  "### Added",
  "",
  merge_table(added),
  "",
  "### Re-matched",
  "",
  merge_table(recheck, changes |> filter(.key %in% recheck_keys, column %in% review_cols))
)
write_comment("merge", merge_md)

fmt_change <- function(main, pr) {
  d <- pr - main
  case_when(d > 0 ~ paste0("+", d), d < 0 ~ as.character(d), TRUE ~ "")
}
# one row per metric: a named list of functions over a data frame
metric_table <- function(metrics) {
  tibble(metric = names(metrics),
         main = vapply(metrics, \(f) f(main_pub), numeric(1)),
         branch = vapply(metrics, \(f) f(pr_pub), numeric(1))) |>
    mutate(change = fmt_change(main, branch), across(c(main, branch), \(x) format(x, big.mark = ",", trim = TRUE)))
}
# one row per value of a column, counted on each side
count_table <- function(col, label = col, active_only = TRUE, only_changed = FALSE, fill_na = "none") {
  side <- function(df, name) {
    if (!col %in% names(df)) return(tibble(value = character(), !!name := integer()))
    if (active_only) df <- df |> filter(status == "Active")
    df |> count(value = replace_na(as.character(.data[[col]]), fill_na), name = name)
  }
  out <- full_join(side(main_pub, "main"), side(pr_pub, "branch"), by = "value") |>
    mutate(across(c(main, branch), \(x) replace_na(x, 0L)), change = fmt_change(main, branch)) |>
    arrange(desc(branch), value)
  n_all <- nrow(out)
  if (only_changed) out <- out |> filter(change != "")
  out <- out |> rename(!!label := value) |> mutate(across(c(main, branch), \(x) format(x, big.mark = ",", trim = TRUE)))
  list(table = out, n_all = n_all)
}
n_active <- \(df) sum(df$status == "Active")
has_col <- \(df, col) col %in% names(df)

overview <- metric_table(list(
  "Agreement rows" = nrow,
  "Active" = n_active,
  "Removed" = \(df) sum(df$status == "Removed"),
  "Superseded" = \(df) sum(df$status == "Superseded"),
  "Agencies (ever)" = \(df) n_distinct(df$agency_id),
  "Agencies with an active agreement" = \(df) n_distinct(df$agency_id[df$status == "Active"]),
  "States with an active agreement" = \(df) n_distinct(df$state[df$status == "Active"]),
  "Rows on ICE's newest sheet" = \(df) if (has_col(df, "latest_sheet_row")) max(df$latest_sheet_row, na.rm = TRUE) - 1 else NA_real_
))
coverage <- metric_table(list(
  "Active with a polygon" = \(df) sum(df$status == "Active" & coalesce(df$geometry_type == "Polygon", FALSE)),
  "Active with facility points" = \(df) sum(df$status == "Active" & coalesce(df$geometry_type == "Point", FALSE)),
  "Active with no geometry" = \(df) sum(df$status == "Active" & is.na(df$geometry_type)),
  "Active with an ORI9" = \(df) sum(df$status == "Active" & !is.na(df$ORI9)),
  "Active with no ORI9" = \(df) sum(df$status == "Active" & is.na(df$ORI9)),
  "Active with an MOA linked" = \(df) sum(df$status == "Active" & coalesce(str_detect(df$moa, "^https?://"), FALSE)),
  "Active with the MOA pending" = \(df) sum(df$status == "Active" & coalesce(df$moa == "pending", FALSE)),
  "Active with an addendum" = \(df) if (has_col(df, "has_addendum")) sum(df$status == "Active" & coalesce(df$has_addendum, FALSE)) else NA_real_
))
by_model <- count_table("support_type", "model")
by_level <- count_table("jurisdiction_level", "jurisdiction level")
by_state <- count_table("state", "state", only_changed = TRUE)
by_signed_year <- {
  side <- function(df, name) df |> filter(status == "Active") |> count(value = replace_na(as.character(format(df$signed_date[df$status == "Active"], "%Y")), "undated"), name = name)
  full_join(side(main_pub, "main"), side(pr_pub, "branch"), by = "value") |>
    mutate(across(c(main, branch), \(x) replace_na(x, 0L)), change = fmt_change(main, branch)) |>
    arrange(desc(value)) |> filter(change != "") |> rename(`signed in` = value) |>
    mutate(across(c(main, branch), \(x) format(x, big.mark = ",", trim = TRUE)))
}

main_qa <- read_qa(main_qa_path)
pr_qa <- read_qa(pr_qa_path)
qa_md <- if (is.null(pr_qa)) {
  "_No `review/qa-summary.csv` on this branch._"
} else if (is.null(main_qa)) {
  c("_No `review/qa-summary.csv` on `main` yet; every check on the branch:_", "",
    md_table(pr_qa |> select(check, scope, branch = value, expected, status)))
} else {
  qa <- full_join(main_qa |> select(check, scope, main = value),
                  pr_qa |> select(check, scope, branch = value, expected, status),
                  by = c("check", "scope")) |>
    mutate(moved = coalesce(main != branch, TRUE), failing = !coalesce(status %in% c("pass", "info"), FALSE))
  shown <- qa |> filter(moved | failing) |> arrange(desc(failing), check, scope)
  c(sprintf("_%s moved or fail; %d unchanged._", plural(nrow(shown), "check"), sum(!qa$moved & !qa$failing)), "",
    md_table(shown |> mutate(across(c(main, branch, expected, status), blank_na)) |> select(check, scope, main, branch, expected, status)))
}

stats_md <- c(
  "## Topline numbers: `main` vs this branch",
  "",
  paste0("_", sheet_line, "_"),
  "",
  "### Agreements",
  "",
  md_table(overview),
  "",
  "### Active agreements by model",
  "",
  md_table(by_model$table),
  "",
  "### Active agreements by jurisdiction level",
  "",
  md_table(by_level$table),
  "",
  sprintf("### Active agreements by state (%d of %d states changed)", nrow(by_state$table), by_state$n_all),
  "",
  md_table(by_state$table),
  "",
  "### Active agreements by signing year (changed years only)",
  "",
  md_table(by_signed_year),
  "",
  "### Coverage of active agreements",
  "",
  md_table(coverage),
  "",
  "### QA checks",
  "",
  qa_md
)
write_comment("stats", stats_md)
