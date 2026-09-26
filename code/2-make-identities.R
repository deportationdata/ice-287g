# Resolve every sheet observation to an agreement identity and an agency.
# Automatic tiers first (exact key, typo, modifier, closed under union), the
# committed alias table for the tail, and a candidate report for what is left;
# the pipeline never waits on a manual check.
# -> data/intermediate/identity-agreements.parquet, data/intermediate/identity-agencies.parquet,
#    data/intermediate/identity-agency-spellings.parquet, data/intermediate/sheet-row-agreements.parquet,
#    data/qa/identity-candidates.csv, data/qa/identity-summary.csv
library(tidyverse)
library(stringdist)

source("code/functions.R")

pubs <- arrow::read_parquet("data/intermediate/sheet-publications.parquet")
pub_files <- arrow::read_parquet("data/intermediate/sheet-publication-files.parquet") |>
  filter(is_primary) |>
  select(publication_id, path)
obs <- arrow::read_parquet("data/intermediate/sheet-rows.parquet") |>
  inner_join(pubs |> select(publication_id, pub_seq, is_current), by = "publication_id")
current_seq <- pubs$pub_seq[pubs$is_current]

# each publication's date: ICE's filename date, else its archive capture's (1-read-sheets.R)
pub_dates <- pubs |> select(pub_seq, published_on, published_on_source)
# every sheet is committed under sheets/; this serves it as ICE posted it
SHEET_URL_BASE <- "https://github.com/deportationdata/ice-287g/raw/refs/heads/main/"

xwalk <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")
aliases <- read_agency_aliases(xwalk)
same_alias <- aliases |> filter(relation == "same") |> distinct(state_key, alias_key, target_key)
distinct_verdicts <- aliases |> filter(relation == "distinct") |> distinct(state_key, alias_key)

# T0: the alias table pins a spelling; applied first so a row never depends on it
signed_obs <- obs |>
  filter(!is.na(signed)) |>
  left_join(same_alias, by = c("state_key", "canonical_agency" = "alias_key")) |>
  mutate(aliased = !is.na(target_key),
         observed_agency = canonical_agency,
         canonical_agency = coalesce(target_key, canonical_agency)) |>
  select(-target_key)

# T1: one variant per (bucket, spelling), with its observation window; a spelling
# published beyond this bucket is an agency in its own right, not a typo
agency_life <- signed_obs |>
  summarise(agency_n_pub = n_distinct(publication_id), .by = c(state_key, canonical_agency))
variants <- signed_obs |>
  group_by(state_key, support_key, signed, canonical_agency) |>
  summarise(first_seq = min(pub_seq), last_seq = max(pub_seq),
            n_pub = n_distinct(publication_id), aliased = any(aliased), .groups = "drop") |>
  left_join(agency_life, by = c("state_key", "canonical_agency")) |>
  mutate(variant_id = row_number(),
         in_current = last_seq == current_seq,
         established = agency_n_pub > n_pub,
         digits = map_chr(str_extract_all(canonical_agency, "\\d+"), paste, collapse = ","),
         tokens = str_split(canonical_agency, " "))

# T2 typo and T3 modifier, scoped to a bucket: the tightest scope in which two
# rows can be the same agreement. Digit signature keeps "precinct 1" apart from
# "precinct 2"; a typo either hands over to its correction or is a dominated,
# otherwise-unknown spelling (Pima beside Pinal is neither)
pairs <- variants |>
  inner_join(variants, by = c("state_key", "support_key", "signed"),
             suffix = c("_a", "_b"), relationship = "many-to-many") |>
  filter(variant_id_a < variant_id_b) |>
  mutate(
    dist = stringdist(canonical_agency_a, canonical_agency_b, method = "osa"),
    same_digits = digits_a == digits_b,
    disjoint = last_seq_a < first_seq_b | last_seq_b < first_seq_a,
    ratio = pmin(n_pub_a, n_pub_b) / pmax(n_pub_a, n_pub_b),
    minority_established = if_else(n_pub_a <= n_pub_b, established_a, established_b),
    superset = map2_lgl(tokens_a, tokens_b, \(x, y) all(x %in% y) || all(y %in% x)),
    dominated = ratio <= 0.25 & !minority_established,
    # a typo at distance 3 merges only when disjoint and dominated (Freedom for Freeborn,
    # Brookfield for Brookford); at distance 2 either suffices
    t2 = same_digits & ((dist <= 2 & (disjoint | dominated)) | (dist == 3 & disjoint & dominated)),
    # a spelling with extra words merges when the windows are disjoint, or when it is dominated
    # (one list of "Arkansas State Police" inside the run of "Arkansas State Police Department")
    t3 = !t2 & superset & same_digits & (disjoint | dominated),
    merge = t2 | t3,
    # two spellings that each live in other buckets are two agencies, not a typo pair
    near = !merge & (dist <= 3 | superset) & same_digits & !(established_a & established_b)
  )

# T3b: a rename. ICE relists an agreement under a new name (a sheriff's jail agreement moved
# to the county commission; "Louisiana State Patrol" corrected to State Police): the old
# spelling ends on one list, the new begins on the next, and both link the same MOA file,
# whose name carries a word of one of the spellings. Tested 2026-09-13 over every list: 22
# such handovers, every one the same agreement
moa_file_key <- \(u) str_to_lower(str_remove_all(basename(coalesce(u, "")), "[^A-Za-z0-9]"))
href <- \(x) if_else(str_detect(coalesce(x, ""), "^https?://"), x, NA_character_)
# an MOA cell links an MOA when it is ICE's hyperlinked "Link" (with or without "| Addendum"), a
# url, or an anchor to a real file; "Link Pending", a blank cell, a stray word ICE left in the
# column ("Cons", "#note") and an anchor to ICE's placeholder xxx.pdf do not
linked <- \(cell) {
  cell <- str_to_lower(str_squish(coalesce(cell, "")))
  str_detect(cell, "^https?://") | str_detect(cell, "^link( \\| addendum)?$") |
    (str_detect(cell, "href=") & !str_detect(cell, "/xxx\\.pdf"))
}
no_link <- \(cell) !linked(cell)
# the MOA hyperlink behind given rows: from the workbook for xlsx lists, the cell's href otherwise
row_moa_links <- function(rows) {
  need <- rows |> distinct(publication_id, sheet_row) |> inner_join(pub_files, by = "publication_id")
  xl <- need |> filter(str_detect(path, "\\.xlsx$")) |> distinct(path)
  links <- if (nrow(xl)) {
    xl |> mutate(l = map(path, workbook_links)) |> unnest(l) |>
      inner_join(need, by = c("path", "sheet_row")) |> select(publication_id, sheet_row, moa_link)
  } else {
    tibble(publication_id = character(), sheet_row = integer(), moa_link = character())
  }
  rows |> left_join(links, by = c("publication_id", "sheet_row")) |> mutate(moa_link = coalesce(moa_link, href(raw_moa)))
}
variant_rows <- signed_obs |>
  inner_join(variants |> select(state_key, support_key, signed, canonical_agency, variant_id),
             by = c("state_key", "support_key", "signed", "canonical_agency")) |>
  select(variant_id, pub_seq, publication_id, sheet_row, raw_moa)
adjacent <- pairs |>
  filter(!merge, last_seq_a + 1L == first_seq_b | last_seq_b + 1L == first_seq_a) |>
  mutate(old_id = if_else(last_seq_a + 1L == first_seq_b, variant_id_a, variant_id_b),
         new_id = if_else(last_seq_a + 1L == first_seq_b, variant_id_b, variant_id_a))
pairs$t3b <- FALSE
if (nrow(adjacent)) {
  last_rows <- variant_rows |> filter(variant_id %in% adjacent$old_id) |>
    slice_max(pub_seq, n = 1, by = variant_id, with_ties = FALSE) |> row_moa_links() |>
    select(old_id = variant_id, link_old = moa_link)
  first_rows <- variant_rows |> filter(variant_id %in% adjacent$new_id) |>
    slice_min(pub_seq, n = 1, by = variant_id, with_ties = FALSE) |> row_moa_links() |>
    select(new_id = variant_id, link_new = moa_link)
  generic_words <- c("county", "parish", "sheriff", "sheriffs", "office", "police", "department", "of", "the", "and",
                     "city", "town", "board", "commissioners", "state", "dept", "jail", "correction", "corrections",
                     "task", "force", "model", "law", "enforcement", "division")
  renamed <- adjacent |>
    left_join(last_rows, by = "old_id") |>
    left_join(first_rows, by = "new_id") |>
    filter(!is.na(link_old), !is.na(link_new), moa_file_key(link_old) == moa_file_key(link_new)) |>
    mutate(file_key = moa_file_key(link_new),
           names_file = map2_lgl(paste(canonical_agency_a, canonical_agency_b), file_key, \(n, f) {
             tok <- setdiff(unique(str_split(n, " ")[[1]]), generic_words)
             tok <- tok[nchar(tok) >= 4]
             length(tok) > 0 && any(str_detect(f, fixed(tok)))
           })) |>
    filter(names_file)
  pairs$t3b[match(paste(renamed$variant_id_a, renamed$variant_id_b), paste(pairs$variant_id_a, pairs$variant_id_b))] <- TRUE
  pairs$merge <- pairs$merge | pairs$t3b
}

# T3c: a relabel. While an MOA is still pending, ICE can list the agreement under the wrong
# agency and then print it under the right one (Pinal County Sheriff's Office for the Pinal
# County Attorney's Office, Haskell Police Department for Haskell County Sheriff's Office): a
# spelling that never linked an MOA ends on one list, and on the very next list another spelling
# first appears with the same state, model and signing date. It is judged on whole agreements,
# after the tiers above have joined each agency's spellings, so a short-lived typo of one agency
# never pairs with another agency's spelling (Greene and South Pymatuning Township constables,
# both signed 2025-07-15). Only one agreement may begin there, and a "distinct" row in the alias
# table keeps two names apart. Tested 2026-09-13 over every list: four such handovers, each
# confirmed by the MOA the later agency posted
component_of <- function(from, to, n) {
  parent <- seq_len(n)
  find_root <- function(i) { while (parent[i] != i) { parent[i] <<- parent[parent[i]]; i <- parent[i] }; i }
  for (k in seq_along(from)) {
    ra <- find_root(from[k]); rb <- find_root(to[k])
    if (ra != rb) parent[max(ra, rb)] <- min(ra, rb)
  }
  vapply(seq_len(n), find_root, integer(1))
}
so_far <- tibble(variant_id = variants$variant_id,
                 component = component_of(pairs$variant_id_a[pairs$merge], pairs$variant_id_b[pairs$merge], nrow(variants)))
held_apart <- variants |>
  semi_join(distinct_verdicts, by = c("state_key", "canonical_agency" = "alias_key")) |>
  inner_join(so_far, by = "variant_id") |>
  distinct(component)
agreements_so_far <- variants |>
  inner_join(so_far, by = "variant_id") |>
  arrange(desc(n_pub)) |>
  summarise(first_seq = min(first_seq), last_seq = max(last_seq), rep_id = min(variant_id),
            spelling = first(canonical_agency), .by = c(state_key, support_key, signed, component)) |>
  left_join(variant_rows |> inner_join(so_far, by = "variant_id") |>
              summarise(never_linked = all(no_link(raw_moa)), .by = component), by = "component") |>
  anti_join(held_apart, by = "component")
relabels <- agreements_so_far |>
  filter(never_linked, last_seq < current_seq) |>
  inner_join(agreements_so_far |> select(state_key, support_key, signed, component_new = component, first_seq_new = first_seq,
                                         rep_new = rep_id, spelling_new = spelling),
             by = c("state_key", "support_key", "signed"), relationship = "many-to-many") |>
  filter(component_new != component, first_seq_new == last_seq + 1L) |>
  filter(n() == 1, .by = component) |>
  mutate(old_agency = spelling, new_agency = spelling_new,
         variant_id_a = pmin(rep_id, rep_new), variant_id_b = pmax(rep_id, rep_new))
pairs$t3c <- FALSE
pairs$t3c[match(paste(relabels$variant_id_a, relabels$variant_id_b), paste(pairs$variant_id_a, pairs$variant_id_b))] <- TRUE
pairs$merge <- pairs$merge | pairs$t3c
relabelled_variants <- so_far$variant_id[so_far$component %in% c(relabels$component, relabels$component_new)]
message(sprintf("relabels: %d pending listings joined to the agency ICE printed on the next list", nrow(relabels)))

# union-find closure over the merge edges
parent <- seq_len(nrow(variants))
find_root <- function(i) { while (parent[i] != i) { parent[i] <<- parent[parent[i]]; i <- parent[i] }; i }
for (k in which(pairs$merge)) {
  ra <- find_root(pairs$variant_id_a[k]); rb <- find_root(pairs$variant_id_b[k])
  if (ra != rb) parent[max(ra, rb)] <- min(ra, rb)
}
variants$component <- vapply(variants$variant_id, find_root, integer(1))

# one canonical spelling per component: an alias target, else what the current
# sheet prints, else the most published, else the earliest, else alphabetical
chosen <- variants |>
  arrange(component, desc(aliased), desc(in_current), desc(n_pub), first_seq, canonical_agency) |>
  distinct(component, .keep_all = TRUE) |>
  select(component, chosen_agency = canonical_agency)
variants <- variants |> left_join(chosen, by = "component")
merged_t3 <- unique(c(pairs$variant_id_a[pairs$t3], pairs$variant_id_b[pairs$t3]))
merged_t2 <- unique(c(pairs$variant_id_a[pairs$t2], pairs$variant_id_b[pairs$t2]))
merged_t3b <- unique(c(pairs$variant_id_a[pairs$t3b], pairs$variant_id_b[pairs$t3b]))
merged_t3c <- relabelled_variants

obs_ids <- signed_obs |>
  left_join(variants |> select(state_key, support_key, signed, canonical_agency, variant_id, component, chosen_agency),
            by = c("state_key", "support_key", "signed", "canonical_agency"),
            relationship = "many-to-one")

# T4: a signing date ICE corrected is one agreement, not a re-signing. When an agency and
# model hand over from one signing date to another between consecutive sheets, with no
# other listing of theirs spanning the handover, the earlier rows take the later date if
# the earlier listing never linked an MOA (its cell said "link pending", or was blank, as
# ICE's first sheets of March 2025 left it) and the later one posts the MOA (ICE prints the
# MOA's own date once it has it: 06-12 became 06-11 for dozens of agencies in June 2025),
# or both link the same MOA file within 30 days, or the earlier listing's last cell shows no link
# and the new date is within three months
listings <- obs_ids |>
  arrange(pub_seq, sheet_row) |>
  summarise(component = first(component), first_seq = min(pub_seq), last_seq = max(pub_seq), n_pub = n_distinct(pub_seq),
            pending_only = all(no_link(raw_moa)),
            first_pending = no_link(first(raw_moa)),
            first_pub = first(publication_id), first_row = first(sheet_row), first_moa = first(raw_moa),
            last_pub = last(publication_id), last_row = last(sheet_row), last_moa = last(raw_moa),
            .by = c(state_key, support_key, chosen_agency, signed))
handovers <- listings |>
  filter(last_seq < current_seq) |>
  inner_join(listings |> select(state_key, support_key, chosen_agency, signed_s = signed, component_s = component,
                                first_seq_s = first_seq, first_pending_s = first_pending,
                                first_pub_s = first_pub, first_row_s = first_row, first_moa_s = first_moa),
             by = c("state_key", "support_key", "chosen_agency"), relationship = "many-to-many") |>
  filter(signed_s != signed, first_seq_s == last_seq + 1L)
straddled <- handovers |>
  select(state_key, support_key, chosen_agency, signed, signed_s, last_seq, first_seq_s) |>
  inner_join(listings |> select(state_key, support_key, chosen_agency, x_signed = signed, x_first = first_seq, x_last = last_seq),
             by = c("state_key", "support_key", "chosen_agency"), relationship = "many-to-many") |>
  filter(x_signed != signed, x_signed != signed_s, x_first <= last_seq, x_last >= first_seq_s) |>
  distinct(state_key, support_key, chosen_agency, signed, signed_s)

# the same-file test reads the MOA hyperlink behind each side of a close handover
need_links <- handovers |>
  filter(!pending_only, abs(as.numeric(signed_s - signed)) <= 30 | abs(as.numeric(signed_s - signed)) %in% c(365, 366)) |>
  (\(h) bind_rows(h |> transmute(publication_id = last_pub, sheet_row = last_row),
                  h |> transmute(publication_id = first_pub_s, sheet_row = first_row_s)))() |>
  distinct() |>
  inner_join(pub_files, by = "publication_id")
xlsx_paths <- need_links |> filter(str_detect(path, "\\.xlsx$")) |> distinct(path)
row_links <- if (nrow(xlsx_paths)) {
  xlsx_paths |>
    mutate(links = map(path, workbook_links)) |>
    unnest(links) |>
    inner_join(need_links, by = c("path", "sheet_row")) |>
    select(publication_id, sheet_row, moa_link)
} else {
  tibble(publication_id = character(), sheet_row = integer(), moa_link = character())
}
corrections <- handovers |>
  anti_join(straddled, by = c("state_key", "support_key", "chosen_agency", "signed", "signed_s")) |>
  left_join(row_links |> rename(last_pub = publication_id, last_row = sheet_row, link_a = moa_link), by = c("last_pub", "last_row")) |>
  left_join(row_links |> rename(first_pub_s = publication_id, first_row_s = sheet_row, link_b = moa_link), by = c("first_pub_s", "first_row_s")) |>
  mutate(link_a = coalesce(link_a, href(last_moa)), link_b = coalesce(link_b, href(first_moa_s)),
         rule = case_when(
           pending_only & !first_pending_s ~ "link_pending_then_posted",
           # re-dated while still pending: no MOA ever existed under the earlier date
           pending_only & abs(as.numeric(signed_s - signed)) <= 30 ~ "link_pending_redated",
           # one PDF under two dates within a month, or exactly a year apart (a year typo)
           !is.na(link_a) & !is.na(link_b) & moa_file_key(link_a) == moa_file_key(link_b) &
             (abs(as.numeric(signed_s - signed)) <= 30 | abs(as.numeric(signed_s - signed)) %in% c(365, 366)) ~ "same_moa_file",
           # re-dated within three months while the last cell showed no link: a pending date replaced
           # by the MOA's own (Lawrence County AL, 05-20 to 06-30), or a link that fell back to "Link
           # Pending" before a one-day re-date (Orange County TX, 05-07 to 05-06). Tested 2026-09-13:
           # seven such handovers; the next closest pair is 109 days apart and a real addendum
           no_link(last_moa) & abs(as.numeric(signed_s - signed)) <= 90 ~ "unlinked_redated",
           TRUE ~ NA_character_)) |>
  filter(!is.na(rule)) |>
  slice_min(first_seq_s, n = 1, by = c(state_key, support_key, chosen_agency, signed), with_ties = FALSE)

# a pending listing's date ICE printed for a stretch and then reverted: a run wholly inside
# another date's listing window, which is absent exactly while it is printed, takes that
# date (never where a handover above already maps the other way)
reverts <- listings |>
  filter(pending_only, n_pub == last_seq - first_seq + 1L) |>
  inner_join(listings |> select(state_key, support_key, chosen_agency, signed_s = signed, component_s = component,
                                first_seq_s = first_seq, last_seq_s = last_seq, n_pub_s = n_pub),
             by = c("state_key", "support_key", "chosen_agency"), relationship = "many-to-many") |>
  filter(signed_s != signed, first_seq_s < first_seq, last_seq_s > last_seq,
         n_pub_s == (last_seq_s - first_seq_s + 1L) - n_pub,
         abs(as.numeric(signed_s - signed)) <= 30) |>
  anti_join(corrections, by = c("state_key", "support_key", "chosen_agency", "signed")) |>
  anti_join(corrections |> select(state_key, support_key, chosen_agency, signed = signed_s, signed_s = signed),
            by = c("state_key", "support_key", "chosen_agency", "signed", "signed_s")) |>
  transmute(state_key, support_key, chosen_agency, signed, signed_s, component_s, first_seq_s, rule = "pending_date_reverted")
corrections <- bind_rows(corrections, reverts)

# a chain of corrections lands on its last date
target <- corrections |>
  transmute(state_key, support_key, chosen_agency, signed, to_signed = signed_s, to_component = component_s, rule)
repeat {
  hop <- target |>
    inner_join(target |> select(state_key, support_key, chosen_agency, to_signed = signed,
                                next_signed = to_signed, next_component = to_component),
               by = c("state_key", "support_key", "chosen_agency", "to_signed"))
  if (!nrow(hop)) break
  target <- target |>
    left_join(hop |> select(state_key, support_key, chosen_agency, signed, next_signed, next_component),
              by = c("state_key", "support_key", "chosen_agency", "signed")) |>
    mutate(to_signed = coalesce(next_signed, to_signed), to_component = coalesce(next_component, to_component)) |>
    select(-next_signed, -next_component)
}
obs_ids <- obs_ids |>
  left_join(target |> select(-rule), by = c("state_key", "support_key", "chosen_agency", "signed")) |>
  mutate(date_corrected = !is.na(to_signed),
         signed = coalesce(to_signed, signed),
         component = coalesce(to_component, component)) |>
  select(-to_signed, -to_component)

identities <- obs_ids |>
  arrange(pub_seq, sheet_row) |>
  group_by(state_key, support_key, signed, component) |>
  summarise(
    state = first(state),
    state_abbr = first(state_abbr),
    # before canonical_agency is overwritten below: a folded date correction adds a
    # variant but not a spelling
    n_spellings = n_distinct(canonical_agency),
    canonical_agency = first(chosen_agency),
    identity_resolution = case_when(
      any(aliased) ~ "alias",
      any(variant_id %in% merged_t3b) ~ "rename",
      any(variant_id %in% merged_t3c) ~ "relabel",
      any(variant_id %in% merged_t3) ~ "modifier",
      any(variant_id %in% merged_t2) ~ "typo",
      any(date_corrected) ~ "date_corrected",
      TRUE ~ "exact"
    ),
    first_seq = min(pub_seq), last_seq = max(pub_seq),
    n_pub = n_distinct(publication_id),
    n_sheet_rows = max(table(publication_id)),
    latest_sheet_row = min(sheet_row[pub_seq == max(pub_seq)]),
    raw_state_last = last(raw_state), raw_agency_last = last(raw_agency),
    raw_support_last = last(raw_support), raw_type_last = last(raw_type),
    raw_county_last = last(raw_county), raw_moa_last = last(raw_moa),
    .groups = "drop"
  ) |>
  mutate(
    agency_id = paste0(coalesce(state_abbr, "XX"), "-", slug(canonical_agency)),
    agreement_id = paste0(agency_id, "#", support_abbr(support_key), "#", signed)
  ) |>
  left_join(pub_dates |> select(first_seq = pub_seq, first_appeared = published_on,
                                first_appeared_source = published_on_source), by = "first_seq") |>
  left_join(pub_dates |> select(last_seq = pub_seq, last_appeared = published_on), by = "last_seq") |>
  left_join(pubs |> select(last_seq = pub_seq, publication_id) |>
              inner_join(pub_files, by = "publication_id") |>
              transmute(last_seq, latest_sheet = path), by = "last_seq") |>
  mutate(latest_sheet_url = paste0(SHEET_URL_BASE, map_chr(latest_sheet, utils::URLencode))) |>
  # the first publication without it; none while the current sheet still lists it
  left_join(pub_dates |> transmute(last_seq = pub_seq - 1L, removed_by = published_on,
                                   removed_by_source = published_on_source), by = "last_seq")

# lineage: same model or same-date relabel, next list or within 60 days, no same-model straddler
windows <- identities |>
  select(agency_id, agreement_id, support_key, signed, first_seq, last_seq, n_pub, first_appeared, last_appeared)
edges <- windows |>
  filter(last_seq < current_seq) |>
  inner_join(windows, by = "agency_id", suffix = c("", "_s"), relationship = "many-to-many") |>
  filter(agreement_id != agreement_id_s, support_key_s == support_key | signed_s == signed,
         first_seq_s > last_seq,
         first_seq_s == last_seq + 1L | as.numeric(first_appeared_s - last_appeared) <= 60) |>
  left_join(windows |> select(agency_id, x_support = support_key, x_id = agreement_id, x_first = first_seq, x_last = last_seq),
            by = "agency_id", relationship = "many-to-many") |>
  group_by(agreement_id, agreement_id_s, support_key, support_key_s, signed_s, first_seq_s, last_seq) |>
  summarise(straddled = any(x_support %in% c(support_key, support_key_s) & x_id != agreement_id & x_id != agreement_id_s &
                              x_first <= last_seq & x_last >= first_seq_s), .groups = "drop") |>
  filter(!straddled) |>
  slice_min(order_by = tibble(first_seq_s, support_key_s != support_key, signed_s), n = 1, by = agreement_id, with_ties = FALSE) |>
  # one predecessor per successor
  slice_max(last_seq, n = 1, by = agreement_id_s, with_ties = FALSE) |>
  select(agreement_id, succeeded_by = agreement_id_s)

identities <- identities |>
  left_join(edges, by = "agreement_id") |>
  mutate(status = case_when(last_seq == current_seq ~ "active",
                            !is.na(succeeded_by) ~ "superseded",
                            TRUE ~ "removed"))

# the lineage root: follow predecessors until none remain
root <- set_names(identities$agreement_id, identities$agreement_id)
pred_of <- set_names(edges$agreement_id, edges$succeeded_by)
repeat {
  moved <- FALSE
  for (s in names(pred_of)) {
    p <- pred_of[[s]]
    if (root[[s]] != root[[p]]) { root[[s]] <- root[[p]]; moved <- TRUE }
  }
  if (!moved) break
}
identities$agreement_lineage_id <- unname(root[identities$agreement_id])

id_lookup <- identities |>
  select(state_key, support_key, signed, component, agreement_id, agency_id)

agency_state <- identities |>
  summarise(active_supports = list(unique(support_key[status == "active"])), .by = agency_id)
# a model switch: another model, signed another day, first listed in the lineage window
switched <- windows |>
  inner_join(windows, by = "agency_id", suffix = c("", "_n"), relationship = "many-to-many") |>
  filter(support_key_n != support_key, signed_n != signed, first_seq_n > last_seq,
         first_seq_n == last_seq + 1L | as.numeric(first_appeared_n - last_appeared) <= 60) |>
  distinct(agreement_id) |>
  mutate(switched = TRUE)
identities <- identities |>
  left_join(agency_state, by = "agency_id") |>
  left_join(switched, by = "agreement_id") |>
  mutate(removal_flag = case_when(
    status != "removed" ~ NA_character_,
    map2_lgl(support_key, active_supports, \(s, a) s %in% a) ~ "possible_resign",
    coalesce(switched, FALSE) ~ "model_switch",
    TRUE ~ NA_character_
  )) |>
  select(agreement_id, agency_id, agreement_lineage_id, state, state_abbr, state_key,
         canonical_agency, support_key, signed, status, succeeded_by,
         first_appeared, first_appeared_source, last_appeared, removed_by, removed_by_source,
         removal_flag, first_seq, last_seq, n_pub,
         latest_sheet_row, latest_sheet, latest_sheet_url, n_sheet_rows, identity_resolution, n_spellings,
         starts_with("raw_"))

agencies <- identities |>
  arrange(last_seq) |>
  group_by(agency_id, state, state_abbr, state_key, canonical_agency) |>
  summarise(display_agency = last(raw_agency_last),
            n_agreements = n(), n_active = sum(status == "active"),
            first_seen = min(first_appeared), last_seen = max(last_appeared),
            ice_first_signed = min(signed), .groups = "drop") |>
  mutate(is_current = n_active > 0)

observation_ids <- obs_ids |>
  select(publication_id, sheet_row, is_current, state_key, support_key, signed, component) |>
  left_join(id_lookup, by = c("state_key", "support_key", "signed", "component"),
            relationship = "many-to-one")

# every spelling ICE ever printed, mapped to the agency it resolved to
spellings <- obs_ids |>
  distinct(state_key, state_abbr, observed_agency, raw_agency, chosen_agency) |>
  mutate(agency_id = paste0(coalesce(state_abbr, "XX"), "-", slug(chosen_agency)),
         n_agencies = n_distinct(agency_id), .by = c(state_key, observed_agency)) |>
  select(state_key, state_abbr, observed_agency, raw_agency, agency_id, n_agencies) |>
  arrange(state_key, observed_agency, raw_agency)

stopifnot(
  "exactly one publication may be current" = length(current_seq) == 1L,
  "agreement_id must be unique across identities" = !anyDuplicated(identities$agreement_id),
  "every identity must resolve to an agency" = all(identities$agency_id %in% agencies$agency_id),
  "active identities must be exactly the current publication's signed rows" =
    setequal(identities$agreement_id[identities$status == "active"],
             observation_ids$agreement_id[observation_ids$is_current]),
  "no identity may be both active and superseded" =
    !any(identities$status == "active" & !is.na(identities$succeeded_by)),
  "a superseded identity must name a successor" =
    all(is.na(identities$succeeded_by) == (identities$status != "superseded")),
  "identity resolution must never merge across state, model or signing date" =
    nrow(distinct(identities, agreement_id, state_key, support_key, signed)) == nrow(identities)
)

# candidates: propose, never block
verdict_pairs <- distinct_verdicts |> transmute(state_key, k = alias_key)
near <- pairs |>
  filter(near) |>
  left_join(variants |> select(variant_id, comp_a = component), by = c("variant_id_a" = "variant_id")) |>
  left_join(variants |> select(variant_id, comp_b = component), by = c("variant_id_b" = "variant_id")) |>
  filter(comp_a != comp_b) |>
  anti_join(verdict_pairs, by = c("state_key", "canonical_agency_a" = "k")) |>
  anti_join(verdict_pairs, by = c("state_key", "canonical_agency_b" = "k")) |>
  transmute(kind = "identity_near_dup", state_key, support_key, signed,
            left_agency = canonical_agency_a, right_agency = canonical_agency_b,
            distance = dist, pub_ratio = ratio, windows_overlap = !disjoint,
            left_n_pub = n_pub_a, right_n_pub = n_pub_b,
            suggested_relation = if_else(dist <= 2, "same", "distinct"))

# a rename is judged on the naming tokens: "x county sheriff office" and "y county
# sheriff office" share every generic word and nothing else
generic <- c("county", "parish", "borough", "city", "town", "township", "village", "sheriff", "office",
             "police", "department", "of", "the", "and", "constable", "state", "public", "safety")
name_tokens <- \(x) map(str_split(x, " "), setdiff, generic)
gone <- identities |> filter(status == "removed")
renames <- gone |>
  inner_join(identities |> select(state_key, agency_id_s = agency_id, agency_s = canonical_agency,
                                  first_seq_s = first_seq, support_key_s = support_key),
             by = "state_key", relationship = "many-to-many") |>
  filter(agency_id != agency_id_s, first_seq_s == last_seq + 1L) |>
  mutate(ta = name_tokens(canonical_agency), tb = name_tokens(agency_s),
         jaccard = map2_dbl(ta, tb, \(x, y) length(intersect(x, y)) / max(1L, length(union(x, y)))),
         contained = str_detect(agency_s, fixed(canonical_agency)) | str_detect(canonical_agency, fixed(agency_s))) |>
  filter(jaccard >= 0.5 | contained) |>
  transmute(kind = "agency_rename", state_key, support_key, signed,
            left_agency = canonical_agency, right_agency = agency_s,
            distance = stringdist(canonical_agency, agency_s, method = "osa"),
            pub_ratio = NA_real_, windows_overlap = FALSE,
            left_n_pub = n_pub, right_n_pub = NA_integer_, suggested_relation = "same") |>
  distinct()

# a later agreement of the same agency that the adjacency rule could not link
unlinked <- gone |>
  inner_join(windows |> select(agency_id, successor = agreement_id, first_seq_s = first_seq, n_pub_s = n_pub),
             by = "agency_id", relationship = "many-to-many") |>
  filter(first_seq_s > last_seq + 1L) |>
  slice_min(first_seq_s, n = 1, by = agreement_id, with_ties = FALSE) |>
  transmute(kind = "lineage_unlinked", state_key, support_key, signed,
            left_agency = agreement_id, right_agency = successor,
            distance = first_seq_s - last_seq - 1L, pub_ratio = NA_real_, windows_overlap = FALSE,
            left_n_pub = n_pub, right_n_pub = n_pub_s, suggested_relation = NA_character_)

dir.create("data/qa", showWarnings = FALSE)
candidates <- bind_rows(near, renames, unlinked) |> arrange(kind, state_key, left_agency)
write_csv(candidates, "data/qa/identity-candidates.csv")
# every signing date folded into another, for review
target |>
  transmute(state_key, support_key, agency = chosen_agency, signed_printed = signed, signed = to_signed, rule) |>
  arrange(state_key, agency, support_key, signed_printed) |>
  write_csv("data/qa/signing-date-corrections.csv")
# every pending listing joined to the agency ICE printed on the next list, for review
relabels |>
  transmute(state_key, support_key, signed, printed_agency = old_agency, agency = new_agency) |>
  arrange(state_key, printed_agency) |>
  write_csv("data/qa/identity-relabels.csv")

status_n <- table(identities$status)
resolution_n <- table(identities$identity_resolution)
summary_row <- tibble(
  run_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  publications = nrow(pubs), observations = nrow(obs), unsigned_observations = sum(is.na(obs$signed)),
  identities = nrow(identities), agencies = nrow(agencies),
  active = sum(status_n["active"], na.rm = TRUE), superseded = sum(status_n["superseded"], na.rm = TRUE),
  removed = sum(status_n["removed"], na.rm = TRUE),
  merged_by_alias = sum(resolution_n["alias"], na.rm = TRUE),
  merged_by_typo = sum(resolution_n["typo"], na.rm = TRUE),
  merged_by_modifier = sum(resolution_n["modifier"], na.rm = TRUE),
  date_corrections = nrow(target),
  candidates = nrow(candidates)
)
write_csv(summary_row, "data/qa/identity-summary.csv")
message(sprintf("identities: %d (%d active, %d superseded, %d removed); agencies %d; candidates %d",
                nrow(identities), summary_row$active, summary_row$superseded, summary_row$removed,
                nrow(agencies), nrow(candidates)))

arrow::write_parquet(identities, "data/intermediate/identity-agreements.parquet")
arrow::write_parquet(agencies, "data/intermediate/identity-agencies.parquet")
arrow::write_parquet(spellings, "data/intermediate/identity-agency-spellings.parquet")
arrow::write_parquet(observation_ids |> select(publication_id, sheet_row, agreement_id, agency_id),
                     "data/intermediate/sheet-row-agreements.parquet")
