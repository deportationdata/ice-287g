# Every archived participating-agencies sheet, hash-grouped into publications
# -> data/sheet-publications.parquet, data/sheet-publication-files.parquet,
#    data/intermediate/sheet-rows.parquet
library(tidyverse)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")
# ICE printed some agreements under the wrong agency and later corrected the name;
# keyed on the erroneous row, so a fix does nothing on every row ICE printed correctly
agency_name_fixes <- read_csv("inputs/agency-name-fixes.csv", col_types = "cccDcc") |>
  transmute(state, agency, support_key = canonical_support(support_type), signed, agency_fixed)
# ICE has printed an agreement under the wrong state for a stretch; keyed the same way
state_fixes <- read_csv("inputs/state-fixes.csv", col_types = "cccDcc") |>
  transmute(state, agency, support_key = canonical_support(support_type), signed, state_fixed)
# ICE has renewed an agreement by linking the new MOA from the old row without re-dating it;
# rows on lists from the first to link the renewal take its date, so the old agreement ends
# there. Keyed on the erroneous row, so a fix does nothing once ICE prints the renewal's date
renewal_date_fixes <- read_csv("inputs/renewal-date-fixes.csv", col_types = "cccDDDc") |>
  transmute(state, agency, support_key = canonical_support(support_type), signed, published_from, signed_fixed)
# ICE has printed a wrong signing date (a year typo, a neighbour's date); keyed the same way,
# and applied here so the corrected rows join the agreement they belong to
signed_date_fixes <- read_csv("inputs/signed-date-fixes.csv", col_types = "cccDDc") |>
  transmute(state, agency, support_key = canonical_support(support_type), signed, signed_fixed)
# ICE has printed an agreement under the wrong model for a list or two (Morehouse Parish's task
# force as a jail agreement); keyed the same way, and applied before the date fixes and the
# identity rules so the corrected rows join the agreement they belong to
support_type_fixes <- read_csv("inputs/support-type-fixes.csv", col_types = "cccDcc") |>
  transmute(state, agency, support_key = canonical_support(support_type), signed, support_type_fixed)

# ICE's workbooks, and the roster table of every archived page that carries one
# (the pages before April 2008 describe the program in prose and yield nothing)
workbooks <- list.files("sheets", "\\.xlsx$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
# pending lists are a different population
workbooks <- workbooks[!str_detect(basename(workbooks), regex("^pending", ignore_case = TRUE))]
pages <- list.files("sheets", "\\.html?$", recursive = TRUE, full.names = TRUE)
page_tables <- set_names(map(pages, read_roster_html), pages) |> compact()
files <- c(workbooks, names(page_tables))

manifests <- snapshot_manifests("sheets") |>
  select(folder, path_now, url, note, retrieved_at, capture_time, last_modified)

# a workbook is one publication per distinct bytes; a page is one per distinct
# table per capture day, so a table that returns after a different one (a row
# dropped and restored) still reads as a change rather than folding into the
# earlier publication and hiding the removal between them
hash_of <- \(p) {
  if (!p %in% names(page_tables)) return(digest::digest(file = p, algo = "sha256"))
  day <- str_match(basename(p), "^(?:ice_287g_)?(\\d{8})")[, 2]
  digest::digest(list(page_tables[[p]], day))
}

# a publication is dated by ICE's own filename date wherever a file carries one (every
# list since March 2025), else by ice.gov's Last-Modified, else by the Eastern date of
# its earliest archive capture;
# capture times are kept only as evidence, to order lists that share a date and to
# catch a filename date the file was online before (dated once its rows are read)
stamp <- \(x) as.POSIXct(x, format = "%Y%m%d%H%M%S", tz = "UTC")
file_meta <- tibble(path = files) |>
  mutate(
    file_hash = map_chr(path, hash_of),
    folder = str_extract(path, "^sheets/[^/]+")
  ) |>
  left_join(manifests, by = c("path" = "path_now", "folder")) |>
  mutate(
    ice_date = ice_filename_date(path),
    ice_part = ice_filename_part(path),
    modified_on = as.Date(format(as.POSIXct(last_modified, format = "%a, %d %b %Y %H:%M:%S", tz = "UTC"),
                                 tz = "America/New_York")),
    # when the file was demonstrably online: a Wayback capture, our scraper's run folder,
    # a logged download, or the once-a-day snapshot folder of a mirror
    captured_at = coalesce(
      stamp(coalesce(capture_time, str_match(url, "/web/(\\d{14})")[, 2],
                     str_match(basename(path), "^(?:ice_287g_)?(\\d{14})")[, 2])),
      str_match(path, "sheets_(\\d{8})_(\\d{6})") |> (\(m) stamp(paste0(m[, 2], m[, 3])))(),
      as.POSIXct(if_else(str_detect(coalesce(retrieved_at, ""), "T"), retrieved_at, NA_character_),
                 format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      str_match(url, "Tracking_287g/.*/sheets_(\\d{8})_(\\d{6})/") |> (\(m) stamp(paste0(m[, 2], m[, 3])))()
    ),
    source_kind = case_when(
      str_detect(folder, "pre2018|pre2011") ~ "factsheet",
      str_detect(path, "\\.html?$") ~ "wayback_table",
      str_detect(folder, "wayback") ~ "wayback_xlsx",
      TRUE ~ "live"
    )
  ) |>
  filter(!is.na(ice_date) | !is.na(captured_at))

publications <- file_meta |>
  arrange(coalesce(ice_date, as.Date(captured_at)), captured_at) |>
  group_by(file_hash) |>
  summarise(
    n_files = n(),
    ice_date = if (all(is.na(ice_date))) as.Date(NA) else min(ice_date, na.rm = TRUE),
    ice_part = first(na.omit(ice_part)),
    modified_on = if (all(is.na(modified_on))) as.Date(NA) else min(modified_on, na.rm = TRUE),
    captured_at = if (all(is.na(captured_at))) as.POSIXct(NA, tz = "UTC") else min(captured_at, na.rm = TRUE),
    source_kind = first(source_kind),
    has_live = any(source_kind == "live"),
    primary_path = first(path),
    .groups = "drop"
  ) |>
  mutate(publication_id = paste0("pb_", substr(file_hash, 1, 12)))

publication_files <- file_meta |>
  left_join(publications |> select(file_hash, publication_id, primary_path), by = "file_hash") |>
  transmute(publication_id, path, ice_date, ice_part, captured_at, is_primary = path == primary_path)

read_failures <- character()
observations <- publications |>
  select(publication_id, primary_path) |>
  pmap(\(publication_id, primary_path) {
    rows <- read_sheet_rows(primary_path, table = page_tables[[primary_path]])
    if (is.null(rows)) { read_failures <<- c(read_failures, primary_path); return(NULL) }
    mutate(rows, publication_id = publication_id, .before = 1)
  }) |>
  list_rbind()

if (length(read_failures)) {
  warning(length(read_failures), " workbook(s) had no roster columns: ",
          paste(read_failures, collapse = ", "))
}

# keys: state through the same snap as the agreements build, agency and support
# through the identity family
observations <- observations |>
  mutate(
    state = snap_state_name(str_to_title(str_squish(raw_state)), state_xwalk$state_full),
    state = if_else(state == "Northern Mariana Islands", "Commonwealth of the Northern Mariana Islands", state),
    support_key = canonical_support(raw_support), agency = str_squish(raw_agency)
  ) |>
  left_join(state_fixes, by = c("state", "agency", "support_key", "signed")) |>
  mutate(state = coalesce(state_fixed, state), state_fixed = !is.na(state_fixed)) |>
  left_join(support_type_fixes, by = c("state", "agency", "support_key", "signed")) |>
  mutate(support_fixed = !is.na(support_type_fixed),
         raw_support = coalesce(support_type_fixed, raw_support),
         support_key = canonical_support(raw_support)) |>
  select(-support_type_fixed) |>
  left_join(signed_date_fixes, by = c("state", "agency", "support_key", "signed")) |>
  mutate(signed_date_fixed = !is.na(signed_fixed), signed_printed = signed, signed = coalesce(signed_fixed, signed)) |>
  select(-signed_fixed)

# ICE has printed an agreement under the wrong state for a stretch (Burnet County TX under
# Florida for three captures): the same agency, model and date under two states, never on one
# list, with the shorter run at most a quarter of the longer or ten lists, takes the longer
# run's state. Tested 2026-09-13: it repairs the two hand fixes and seven more stretches, and
# leaves the four real same-name pairs (Anderson SC/KS, Logan AR/KS, Madison NY/TN, the two
# Bureaus of Investigation), which share lists, alone
stretch_rows <- observations |>
  filter(!is.na(signed)) |>
  mutate(k = norm_key(raw_agency)) |>
  distinct(k, support_key, signed, state, publication_id)
stretch_runs <- stretch_rows |>
  summarise(n = n_distinct(publication_id), .by = c(k, support_key, signed, state)) |>
  add_count(k, support_key, signed, name = "n_states") |>
  filter(n_states == 2) |>
  arrange(k, support_key, signed, desc(n)) |>
  summarise(major = first(state), minor = last(state), n_major = first(n), n_minor = last(n), .by = c(k, support_key, signed)) |>
  filter(n_minor <= 10 | n_minor / n_major <= 0.25)
co_published <- stretch_rows |>
  semi_join(stretch_runs, by = c("k", "support_key", "signed")) |>
  count(k, support_key, signed, publication_id) |>
  filter(n > 1) |>
  distinct(k, support_key, signed)
state_stretches <- stretch_runs |> anti_join(co_published, by = c("k", "support_key", "signed"))
observations <- observations |>
  mutate(k = norm_key(raw_agency)) |>
  left_join(state_stretches |> select(k, support_key, signed, minor, major), by = c("k", "support_key", "signed")) |>
  mutate(state_stretch_fixed = coalesce(state == minor, FALSE),
         state = if_else(state_stretch_fixed, major, state)) |>
  select(-k, -minor, -major)
message(sprintf("wrong-state stretches: %d sheet rows of %d agreements moved to the state of the longer run",
                sum(observations$state_stretch_fixed), nrow(state_stretches)))

observations <- observations |>
  left_join(state_xwalk |> select(state = state_full, state_abbr), by = "state") |>
  left_join(agency_name_fixes, by = c("state", "agency", "support_key", "signed")) |>
  mutate(agency_name_fixed = !is.na(agency_fixed), raw_agency = coalesce(agency_fixed, raw_agency)) |>
  select(-agency, -agency_fixed) |>
  mutate(
    state_key = norm_state(state),
    canonical_agency = canonical_agency(raw_agency, coalesce(state_abbr, ""), coalesce(state, "")),
    support_key = canonical_support(raw_support),
    is_addendum = str_detect(str_to_lower(raw_agency), "\\s(addendum|amendment)$")
  )

unmatched_fixes <- agency_name_fixes |>
  anti_join(observations |> filter(agency_name_fixed) |> distinct(state, agency = raw_agency, support_key, signed) |>
              rename(agency_fixed = agency), by = c("state", "agency_fixed", "support_key", "signed"))
if (nrow(unmatched_fixes)) {
  message(nrow(unmatched_fixes), " agency-name fix(es) in inputs/agency-name-fixes.csv match no sheet row; kept as the record")
}
message(sprintf("agency-name fixes: %d sheet rows renamed by %d fixes", sum(observations$agency_name_fixed),
                nrow(agency_name_fixes) - nrow(unmatched_fixes)))
unmatched_state_fixes <- state_fixes |>
  anti_join(observations |> filter(state_fixed) |> distinct(state_fixed = state, support_key, signed), by = c("state_fixed", "support_key", "signed"))
if (nrow(unmatched_state_fixes)) {
  message(nrow(unmatched_state_fixes), " state fix(es) in inputs/state-fixes.csv match no sheet row; kept as the record")
}
message(sprintf("state fixes: %d sheet rows moved by %d fixes", sum(observations$state_fixed), nrow(state_fixes) - nrow(unmatched_state_fixes)))
unmatched_date_fixes <- signed_date_fixes |>
  anti_join(observations |> filter(signed_date_fixed) |> distinct(state, support_key, signed_fixed = signed), by = c("state", "support_key", "signed_fixed"))
if (nrow(unmatched_date_fixes)) {
  message(nrow(unmatched_date_fixes), " signing-date fix(es) in inputs/signed-date-fixes.csv match no sheet row; kept as the record")
}
message(sprintf("signing-date fixes: %d sheet rows re-dated by %d fixes", sum(observations$signed_date_fixed), nrow(signed_date_fixes) - nrow(unmatched_date_fixes)))
unmatched_support_fixes <- support_type_fixes |>
  anti_join(observations |> filter(support_fixed) |> distinct(state, support_key, signed) |> rename(support_key_fixed = support_key),
            by = join_by(state, signed)) |>
  bind_rows(support_type_fixes |>
              mutate(support_key_fixed = canonical_support(support_type_fixed)) |>
              anti_join(observations |> filter(support_fixed) |> distinct(state, support_key_fixed = support_key, signed),
                        by = c("state", "support_key_fixed", "signed")) |>
              select(-support_key_fixed)) |>
  distinct(state, agency, support_key, signed, .keep_all = TRUE)
if (nrow(unmatched_support_fixes)) {
  message(nrow(unmatched_support_fixes), " model fix(es) in inputs/support-type-fixes.csv match no sheet row; kept as the record")
}
message(sprintf("model fixes: %d sheet rows given another model by %d fixes", sum(observations$support_fixed),
                nrow(support_type_fixes) - nrow(unmatched_support_fixes)))

stopifnot(
  "every publication must have at least one row" =
    setequal(publications$publication_id, unique(observations$publication_id))
)

# ICE prints the same agreement twice under two spellings now and then; a fold
# inside one publication is reported for review, never a reason to stop the build
dir.create("data/qa", showWarnings = FALSE)
within_folds <- observations |>
  filter(!is.na(signed)) |>
  distinct(publication_id, state_key, canonical_agency, support_key, signed, raw_agency) |>
  add_count(publication_id, state_key, canonical_agency, support_key, signed, name = "n_spellings") |>
  filter(n_spellings > 1) |>
  summarise(spellings = paste(sort(unique(raw_agency)), collapse = " | "),
            .by = c(state_key, canonical_agency, support_key, signed)) |>
  left_join(observations |> summarise(n_publications = n_distinct(publication_id),
                                      .by = c(state_key, canonical_agency, support_key, signed)),
            by = c("state_key", "canonical_agency", "support_key", "signed")) |>
  arrange(state_key, canonical_agency)
write_csv(within_folds, "data/qa/within-publication-folds.csv")
if (nrow(within_folds)) {
  message(nrow(within_folds), " identit(ies) fold two spellings printed in one publication; see data/qa/within-publication-folds.csv")
}

# ICE has dated a filename a year early (01062025 on a January 2026 list), and a list
# cannot predate the newest signing it prints; a list online before its filename date
# takes that capture's date instead. Lists sharing a date keep ICE's am/mid/pm order,
# then capture order (ICE has re-posted a file under the same name)
# printed dates: a date fix can postdate lists that carried the agreement
newest_signed <- observations |>
  filter(!is.na(signed_printed)) |>
  summarise(newest_signed = max(signed_printed), .by = publication_id)
observations <- observations |> select(-signed_printed)
publications <- publications |>
  left_join(newest_signed, by = "publication_id") |>
  mutate(
    ice_date = if_else(coalesce(ice_date < newest_signed & ice_date %m+% years(1) >= newest_signed, FALSE),
                       ice_date %m+% years(1), ice_date),
    captured_on = as.Date(format(captured_at, tz = "America/New_York")),
    date_flag = case_when(
      coalesce(ice_date < newest_signed, FALSE) ~ "ICE filename date before the newest signing date it lists",
      coalesce(captured_on < ice_date - 1, FALSE) ~ "online before its ICE filename date",
      TRUE ~ NA_character_
    ),
    published_on = case_when(
      !is.na(ice_date) & !(date_flag %in% "online before its ICE filename date") ~ ice_date,
      is.na(ice_date) & !is.na(modified_on) ~ modified_on,
      TRUE ~ captured_on
    ),
    published_on_source = case_when(
      coalesce(published_on == ice_date, FALSE) ~ "ice_filename",
      is.na(ice_date) & !is.na(modified_on) ~ "ice_last_modified",
      TRUE ~ "archive_capture"
    )
  ) |>
  arrange(published_on, case_when(ice_part == "am" ~ 1L, ice_part == "pm" ~ 3L, TRUE ~ 2L), captured_at, file_hash) |>
  mutate(pub_seq = row_number(), prev_publication_id = lag(publication_id), is_current = pub_seq == max(pub_seq))
if (!publications$has_live[publications$is_current]) {
  warning("the newest publication is an archived copy, not a live scrape: ", publications$publication_id[publications$is_current])
}

# a renewal fix starts on a list's date, so it waits until the lists are dated
observations <- observations |>
  mutate(agency = str_squish(raw_agency)) |>
  left_join(publications |> select(publication_id, published_on), by = "publication_id") |>
  left_join(renewal_date_fixes, by = c("state", "agency", "support_key", "signed")) |>
  mutate(renewal_date_fixed = coalesce(published_on >= published_from, FALSE),
         signed = if_else(renewal_date_fixed, signed_fixed, signed)) |>
  select(-agency, -published_on, -published_from, -signed_fixed)
unmatched_renewal_fixes <- renewal_date_fixes |>
  anti_join(observations |> filter(renewal_date_fixed) |> distinct(state, support_key, signed_fixed = signed),
            by = c("state", "support_key", "signed_fixed"))
if (nrow(unmatched_renewal_fixes)) {
  message(nrow(unmatched_renewal_fixes), " renewal date fix(es) in inputs/renewal-date-fixes.csv match no sheet row; kept as the record")
}
message(sprintf("renewal date fixes: %d sheet rows re-dated by %d fixes", sum(observations$renewal_date_fixed),
                nrow(renewal_date_fixes) - nrow(unmatched_renewal_fixes)))

# ICE has printed a signing year early (2025-02-23 for an agreement first listed in February
# 2026; 2016 for 2026): a listing first seen on an ICE-dated list more than 300 days after its
# printed date, whose month and day fall within the 60 days before that list in the list's
# year or the year before, takes that year. A listing is a spelling, so a spelling ICE
# corrected on an ICE-dated list is not new: when the same date was already printed for a
# near-identical spelling (Albermarle District Jail, signed 2020-03-19, respelled Albemarle
# on the 2025-03-26 list), the date stands. Tested 2026-09-13 with the two year-typo rows of
# inputs/signed-date-fixes.csv (kept as the record) lifted: exactly those two and Tenaha
# Police Department, which ICE itself later re-dated, and none of the archive-era listings,
# which the source test excludes
first_listed <- observations |>
  filter(!is.na(signed)) |>
  inner_join(publications |> select(publication_id, published_on, published_on_source), by = "publication_id") |>
  mutate(k = norm_key(raw_agency)) |>
  arrange(published_on) |>
  summarise(first_on = first(published_on), first_source = first(published_on_source), .by = c(state, k, support_key, signed))
year_typos <- first_listed |>
  filter(first_source == "ice_filename", as.numeric(first_on - signed) > 300) |>
  mutate(same_year = suppressWarnings(as.Date(sprintf("%d-%02d-%02d", year(first_on), month(signed), day(signed)))),
         year_before = same_year %m-% years(1),
         signed_year_fixed = case_when(
           !is.na(same_year) & same_year <= first_on & same_year >= first_on - 60 ~ same_year,
           !is.na(year_before) & year_before <= first_on & year_before >= first_on - 60 ~ year_before,
           TRUE ~ as.Date(NA))) |>
  filter(!is.na(signed_year_fixed))
respelled <- year_typos |>
  inner_join(first_listed |> select(state, support_key, signed, k_prior = k, prior_on = first_on),
             by = c("state", "support_key", "signed"), relationship = "many-to-many") |>
  filter(k_prior != k, prior_on < first_on, stringdist::stringdist(k, k_prior, method = "osa") <= 3) |>
  distinct(state, k, support_key, signed)
year_typos <- year_typos |>
  anti_join(respelled, by = c("state", "k", "support_key", "signed")) |>
  select(state, k, support_key, signed, signed_year_fixed)
observations <- observations |>
  mutate(k = norm_key(raw_agency)) |>
  left_join(year_typos, by = c("state", "k", "support_key", "signed")) |>
  mutate(year_typo_fixed = !is.na(signed_year_fixed), signed = coalesce(signed_year_fixed, signed)) |>
  select(-k, -signed_year_fixed)
message(sprintf("year typos: %d sheet rows of %d listings re-dated to the year they were first listed",
                sum(observations$year_typo_fixed), nrow(year_typos)))
publications <- publications |>
  left_join(observations |> summarise(n_rows = n(), n_unsigned = sum(is.na(signed)), .by = publication_id),
            by = "publication_id") |>
  select(publication_id, file_hash, n_files, published_on, published_on_source, ice_date, ice_part, captured_at,
         date_flag, source_kind, n_rows, n_unsigned, is_current, pub_seq, prev_publication_id)

message(sprintf("sheets: %d files -> %d publications (%d dated by ICE's filename, %d by ICE's Last-Modified, %d by archive capture, %d date flags; current: %s); %d observation rows, %d unsigned",
                nrow(file_meta), nrow(publications), sum(publications$published_on_source == "ice_filename"),
                sum(publications$published_on_source == "ice_last_modified"),
                sum(publications$published_on_source == "archive_capture"), sum(!is.na(publications$date_flag)),
                publications$publication_id[publications$is_current],
                nrow(observations), sum(is.na(observations$signed))))

arrow::write_parquet(publications, "data/intermediate/sheet-publications.parquet")
arrow::write_parquet(publication_files, "data/intermediate/sheet-publication-files.parquet")
arrow::write_parquet(observations, "data/intermediate/sheet-rows.parquet")
