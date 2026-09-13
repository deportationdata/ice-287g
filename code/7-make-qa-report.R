# Invariants and counts over the shipped outputs -> data/qa/*.csv, committed so a
# regression shows as a diff in the PR. QA_STRICT=1 (set by CI) turns any "fail" into
# an error; unset locally so a red check is reported, not fatal.
library(tidyverse)
library(sf)

source("code/functions.R")
options(tigris_use_cache = TRUE)

strict <- nzchar(Sys.getenv("QA_STRICT"))
dir.create("data/qa", showWarnings = FALSE)

agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")
all_sf     <- st_read("data/intermediate/match-all-features.parquet", quiet = TRUE)
level_sf   <- st_read("data/agreement-level-sf.parquet", quiet = TRUE)
ids        <- arrow::read_parquet("data/intermediate/match-agency-identifiers.parquet")
missing    <- arrow::read_parquet("data/intermediate/match-missing-identifiers.parquet")
identities <- arrow::read_parquet("data/intermediate/identity-agreements.parquet")
pubs       <- arrow::read_parquet("data/intermediate/sheet-publications.parquet")
partnerships <- arrow::read_parquet("data/partnerships.parquet")
claims     <- read_csv("data/intermediate/historical-source-claims.csv", show_col_types = FALSE)
disagree   <- read_csv("data/intermediate/partnership-disagreements.csv", show_col_types = FALSE)

features <- all_sf |>
  mutate(empty = st_is_empty(geometry)) |>
  st_drop_geometry()
active_features <- features |> filter(status == "active")

# the newest participating sheet is the ground truth for the active count
newest_dir <- list.files("sheets", "^sheets_2", full.names = TRUE) |> sort() |> last()
newest_sheet <- list.files(newest_dir, "^participatingAgencies.*\\.xlsx$",
                           full.names = TRUE, ignore.case = TRUE) |> first()
newest_rows <- readxl::read_excel(newest_sheet, col_types = "text") |>
  filter(if_any(1:2, ~ !is.na(.x) & .x != "")) |>
  nrow()

manifests <- list.files("agreements", "^manifest\\.csv$", recursive = TRUE, full.names = TRUE) |>
  map(\(p) read_csv(p, col_types = cols(.default = "c"), progress = FALSE) |>
             mutate(folder = basename(dirname(p)), .before = 1)) |>
  list_rbind()

census_counties <- counties_reference(2024) |> st_drop_geometry() |> pull(geoid)
ori_ok <- str_detect(coalesce(ids$ORI9, "AA0000000"), "^[A-Z]{2}[A-Z0-9]{7}$")
county_ok <- with(all_sf, is.na(county_fips) |
  (str_detect(county_fips, "^\\d{5}$") & str_sub(county_fips, 1, 2) == state_fips))

# one row per check; `expected` NA means the row is a tracked count, not a test
check <- function(check, value, expected = NA, scope = "all") {
  tibble(check = check, scope = scope, value = as.character(value),
         expected = as.character(expected),
         status = case_when(is.na(expected) ~ "info",
                            value == expected ~ "pass",
                            TRUE ~ "fail"))
}

summary <- bind_rows(
  check("agreements rows", nrow(agreements)),
  check("agreements active", sum(agreements$status == "active")),
  check("agreements removed", sum(agreements$status == "removed")),
  check("agreements superseded", sum(agreements$status == "superseded")),
  check("agreements == identities", nrow(agreements), nrow(identities)),
  check("partnerships", n_distinct(agreements$partnership_id)),
  check("identities folding more than one spelling", sum(identities$n_spellings > 1)),
  identities |> count(identity_resolution, name = "n") |>
    pmap(\(identity_resolution, n) check("identities by resolution", n, scope = identity_resolution)) |> list_rbind(),
  check("superseded agreements name a successor",
        sum(agreements$status == "superseded" & is.na(agreements$succeeded_by)), 0),
  check("sheet publications", nrow(pubs)),
  check("sheet publications dated by ICE's filename", sum(pubs$published_on_source == "ice_filename")),
  check("sheet publications dated by an archive capture", sum(pubs$published_on_source == "archive_capture")),
  check("sheet publications out of date order", sum(diff(pubs$published_on[order(pubs$pub_seq)]) < 0), 0),
  pubs |> filter(!is.na(date_flag)) |> count(date_flag, name = "n") |>
    pmap(\(date_flag, n) check("sheet publications with a date flag", n, scope = date_flag)) |> list_rbind(),
  check("agreements absent from a publication inside their listing window",
        sum(identities$n_pub < identities$last_seq - identities$first_seq + 1)),
  check("identity candidates awaiting a verdict", nrow(read_csv("data/qa/identity-candidates.csv", show_col_types = FALSE))),
  check("partnerships across every era", nrow(partnerships)),
  partnerships |> count(jurisdiction_level, name = "n") |>
    pmap(\(jurisdiction_level, n) check("partnerships by jurisdiction level", n, scope = coalesce(jurisdiction_level, "none"))) |> list_rbind(),
  check("partnerships added from ICE press releases", sum(!partnerships$ice_published)),
  check("source claims", nrow(claims)),
  claims |> count(resolution, name = "n") |>
    pmap(\(resolution, n) check("source claims by resolution", n, scope = resolution)) |> list_rbind(),
  check("source claims left unresolved",
        sum(str_starts(claims$resolution, "unresolved") & claims$field %in% c("listed", "signed"))),
  disagree |> count(kind, name = "n") |>
    pmap(\(kind, n) check("partnership disagreements by kind", n, scope = kind)) |> list_rbind(),
  check("active agreements == newest sheet rows", sum(agreements$status == "active"), newest_rows),
  check("duplicate identity rows in agreements",
        sum(duplicated(agreements[c("state", "agency", "support_type", "signed")])), 0),
  check("distinct states in agreements", n_distinct(agreements$state)),
  check("feature rows", nrow(all_sf)),
  check("agreement-level rows == agreements rows", nrow(level_sf), nrow(agreements)),
  check("agreements with no feature row",
        sum(!agreements$agreement_id %in% all_sf$agreement_id), 0),
  features |> count(match_layer, name = "n") |>
    pmap(\(match_layer, n) check("feature rows by layer", n, scope = match_layer)) |> list_rbind(),
  features |> filter(empty) |> count(match_layer, name = "n") |>
    pmap(\(match_layer, n) check("empty-geometry features by layer", n, scope = match_layer)) |> list_rbind(),
  check("active features flagged needs_review", sum(active_features$needs_review)),
  active_features |> count(match_quality, name = "n") |>
    pmap(\(match_quality, n) check("active features by match quality", n, scope = match_quality)) |> list_rbind(),
  check("active agreements flagged needs_review", sum(level_sf$needs_review[level_sf$status == "active"])),
  check("active features with a pending MOA", sum(active_features$moa_pending)),
  check("active features with an addendum", sum(active_features$has_addendum)),
  check("county geoids outside the census county list",
        sum(!is.na(features$geoid[features$match_layer == "county"]) &
              !features$geoid[features$match_layer == "county"] %in% census_counties)),
  check("DOC agreements fanned out to state prisons",
        n_distinct(features$agreement_id[str_starts(coalesce(features$match_type, ""), "doc_")])),
  check("agreement-identifiers rows == agreements rows", nrow(ids), nrow(agreements)),
  check("active agreements with an ORI9",
        sum(!is.na(ids$ORI9[ids$agreement_id %in% agreements$agreement_id[agreements$status == "active"]]))),
  ids |> count(ori_source, name = "n") |>
    pmap(\(ori_source, n) check("ORI9 by source", n, scope = coalesce(ori_source, "none"))) |> list_rbind(),
  check("malformed ORI9 values", sum(!ori_ok), 0),
  check("ORI9 sentinel -1 shipped", sum(coalesce(ids$ORI9, "") == "-1"), 0),
  check("ori_conflict flagged", sum(coalesce(ids$ori_conflict, FALSE))),
  check("county_fips malformed or outside its state", sum(!county_ok), 0),
  features |> filter(!empty, is.na(geoid), match_layer != "facility") |>
    count(match_layer, name = "n") |>
    pmap(\(match_layer, n) check("placed features with no geoid", n, 0, scope = match_layer)) |> list_rbind(),
  check("Connecticut features carrying a legacy county fips",
        sum(features$state == "Connecticut" & str_detect(coalesce(features$county_fips, ""), "^090(0[1-9]|1[0-5])$"))),
  check("Connecticut features carrying a planning-region fips",
        sum(features$state == "Connecticut" & str_detect(coalesce(features$county_fips, ""), "^091")), 0),
  features |> filter(!empty) |> count(geometry_vintage, name = "n") |>
    pmap(\(geometry_vintage, n) check("placed features by geometry vintage", n, scope = as.character(geometry_vintage))) |> list_rbind(),
  missing |> count(missing_identifier_type, name = "n") |>
    pmap(\(missing_identifier_type, n) check("missing identifiers by type", n, scope = missing_identifier_type)) |> list_rbind(),
  check("distinct MOA urls on the sheet",
        n_distinct(agreements$moa[str_detect(coalesce(agreements$moa, ""), "^https?://")])),
  check("distinct MOA urls in agreements manifests",
        n_distinct(manifests$url[str_detect(coalesce(manifests$url, ""), "^https?://")])),
  check("agreements folders with outstanding failed downloads",
        length(Filter(\(p) file.size(p) > 0,
                      list.files("agreements", "^failed_downloads\\.txt$", recursive = TRUE, full.names = TRUE))), 0),
  check("MOA urls the sheet links that the origin reports gone and no archive holds",
        list.files("agreements", "^unreachable\\.csv$", recursive = TRUE, full.names = TRUE) |>
          map(\(p) read_csv(p, col_types = cols(.default = "c"), progress = FALSE)) |>
          list_rbind() |>
          (\(d) if (nrow(d)) sum(!unique(d$url) %in% manifests$url[file.exists(manifests$saved_path)]) else 0L)())
)
write_csv(summary, "data/qa/qa-summary.csv")

# why rows are flagged: the review vocabulary, one column per flag
flag_cols <- c("geom_class_unknown", "geometry_unmatched", "fuzzy_match", "weak_match", "ambiguous_candidates",
               "type_mismatch", "university_address_mismatch", "university_county_mismatch",
               "roster_county_disagrees", "ori_conflict", "ori_ambiguous", "duplicate_sheet_row")
active_features |>
  select(match_layer, all_of(flag_cols), moa_pending, has_addendum) |>
  pivot_longer(-match_layer, names_to = "flag", values_to = "hit") |>
  filter(coalesce(hit, FALSE)) |>
  count(match_layer, flag, name = "n") |>
  arrange(match_layer, flag) |>
  write_csv("data/qa/qa-review-reasons.csv")

features |>
  count(match_layer, match_type, needs_review, name = "n") |>
  arrange(match_layer, match_type, needs_review) |>
  write_csv("data/qa/qa-match-types.csv")

# provenance health per snapshot folder
folders <- c(list.dirs("agreements", recursive = FALSE), list.dirs("sheets", recursive = FALSE))
map(folders, \(d) {
  mf <- file.path(d, "manifest.csv")
  if (!file.exists(mf)) return(NULL)
  m <- read_csv(mf, col_types = cols(.default = "c"), progress = FALSE)
  disk <- setdiff(list.files(d, recursive = TRUE), c("manifest.csv", "download_path_log.csv", "failed_downloads.txt"))
  urls <- m |> filter(!is.na(url), url != "") |> summarise(n = n_distinct(file_hash), .by = url)
  failed <- file.path(d, "failed_downloads.txt")
  # a row may point at bytes kept in another folder; a ghost is a row whose file exists nowhere
  exists_anywhere <- file.exists(m$saved_path) | file.exists(file.path(d, basename(m$saved_path)))
  tibble(folder = d, manifest_rows = nrow(m), files_on_disk = length(disk),
         ghost_rows = sum(!exists_anywhere),
         urls_with_multiple_hashes = sum(urls$n > 1),
         failed_download_lines = if (file.exists(failed)) length(readLines(failed, warn = FALSE)) else 0L)
}) |>
  list_rbind() |>
  arrange(folder) |>
  write_csv("data/qa/qa-acquisition.csv")

fails <- summary |> filter(status == "fail")
cat(sprintf("qa: %d checks, %d pass, %d fail, %d info\n",
            nrow(summary), sum(summary$status == "pass"), nrow(fails), sum(summary$status == "info")))
if (nrow(fails)) print(as.data.frame(fails |> select(check, scope, value, expected)))
if (strict && nrow(fails)) stop("QA_STRICT: ", nrow(fails), " failing check(s)")
