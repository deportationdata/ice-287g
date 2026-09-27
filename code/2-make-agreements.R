# Every agreement ever observed, active rows from the current sheet and others as last printed
# -> data/intermediate/agreements.parquet
library(tidyverse)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")
# a fix with an agency applies to it alone; a blank fix removes the county, a blank county fills one
county_name_fixes <- read_csv("inputs/county-name-fixes.csv", col_types = "ccccc", na = character()) |>
  transmute(state, agency = na_if(agency, ""), county = na_if(county, ""), county_fixed)
# keyed on the erroneous TYPE, so a fix does nothing once ICE corrects it
agency_type_fixes <- read_csv("inputs/agency-type-fixes.csv", col_types = "ccccc") |>
  transmute(state, agency, type_clean = str_to_lower(type), type_fixed = str_to_lower(type_fixed))
# the agency's own MOA, used only while the sheet links the wrong file (blank moa_linked: link pending)
moa_link_fixes <- read_csv("inputs/moa-link-fixes.csv", col_types = "cccDccc") |>
  transmute(state, agency, support_type = str_to_title(norm_support_key(support_type)), signed, moa_linked, moa_url)
# an addendum changes an agreement without replacing it; ICE links some, others are pages of the MOA's own PDF (#page=)
moa_addenda <- read_csv("inputs/moa-addenda.csv", col_types = "cccDciDcc") |>
  transmute(state, agency, support_type = str_to_title(norm_support_key(support_type)), signed, addendum_url, moa_page, addendum_signed)
moa_manifest <- snapshot_manifests("agreements")
held_moas <- moa_manifest |>
  filter(on_disk, str_detect(coalesce(url, ""), "^https://www\\.ice\\.gov/doclib/287gMOA/")) |>
  distinct(url) |>
  mutate(file = basename(str_remove(url, "[?#].*$")))
# levels are keyed to agency_id so an alias spelling reaches the same row
counties_ref <- arrow::read_parquet("data/intermediate/reference-counties.parquet")
aliases <- read_agency_aliases(state_xwalk)
level_manual <- read_csv("inputs/manual-jurisdiction-levels.csv", col_types = cols(.default = "c")) |>
  left_join(state_xwalk |> select(state = state_full, state_abbr), by = "state") |>
  transmute(agency_id = agency_id_of(state, agency, state_abbr, aliases), manual_level = jurisdiction_level) |>
  distinct(agency_id, .keep_all = TRUE)
stopifnot("every level in inputs/manual-jurisdiction-levels.csv is one of the eight" =
            all(level_manual$manual_level %in% JURISDICTION_LEVELS))

# longest run of up to three words before County/Parish that is a real county of the state
county_from_agency_name <- function(agency, state, counties_ref) {
  ref <- counties_ref |> distinct(state_key, county_key, county)
  out <- rep(NA_character_, length(agency))
  for (k in 3:1) {
    words <- str_match(str_squish(agency), paste0("\\b((?:[A-Za-z.'-]+\\s+){", k - 1, "}[A-Za-z.'-]+)\\s+(County|Parish)\\b"))[, 2]
    hit <- tibble(state_key = norm_state(state), county_key = norm_county(words)) |>
      left_join(ref, by = c("state_key", "county_key")) |>
      pull(county)
    out <- coalesce(out, hit)
  }
  out
}

identities <- arrow::read_parquet("data/intermediate/identity-agreements.parquet")
publications <- arrow::read_parquet("data/intermediate/sheet-publications.parquet")
publication_files <- arrow::read_parquet("data/intermediate/sheet-publication-files.parquet")
observations <- arrow::read_parquet("data/intermediate/sheet-rows.parquet")
observation_ids <- arrow::read_parquet("data/intermediate/sheet-row-agreements.parquet")

current_id <- publications$publication_id[publications$is_current]
current_path <- publication_files |>
  filter(publication_id == current_id, is_primary) |>
  pull(path)

# the MOA / ADDENDUM urls live in the current workbook's embedded hyperlinks
current_header <- names(readxl::read_excel(current_path, n_max = 0)) |> str_squish() |> str_to_upper()
stopifnot("current sheet has no MOA/ADDENDUM column; layout changed?" = all(c("MOA", "ADDENDUM") %in% current_header))
links <- workbook_links(current_path)

# one row per identity; the first row wins where ICE prints an agreement twice
active <- observations |>
  filter(publication_id == current_id) |>
  inner_join(observation_ids, by = c("publication_id", "sheet_row"), relationship = "one-to-one") |>
  filter(!is.na(agreement_id)) |>
  left_join(links, by = "sheet_row") |>
  arrange(sheet_row) |>
  distinct(agreement_id, .keep_all = TRUE) |>
  transmute(agreement_id, status = "Active", state,
            raw_agency, raw_type, raw_county, raw_support, raw_moa,
            signed, moa_link, addendum_link)

# removed and superseded rows as last printed; archived pages carry the link in the MOA cell
last_seen <- identities |>
  filter(status != "Active") |>
  select(agreement_id, pub_seq = last_seq) |>
  inner_join(publications |> select(publication_id, pub_seq), by = "pub_seq") |>
  inner_join(observation_ids |> select(publication_id, sheet_row, agreement_id), by = c("publication_id", "agreement_id")) |>
  slice_min(sheet_row, n = 1, by = agreement_id, with_ties = FALSE) |>
  inner_join(publication_files |> filter(is_primary) |> select(publication_id, path), by = "publication_id")
gone_links <- last_seen |>
  filter(str_detect(path, "\\.xlsx$")) |>
  distinct(path) |>
  mutate(links = map(path, workbook_links)) |>
  unnest(links) |>
  inner_join(last_seen |> select(agreement_id, path, sheet_row), by = c("path", "sheet_row")) |>
  select(agreement_id, moa_link, addendum_link)
gone <- identities |>
  filter(status != "Active") |>
  left_join(gone_links, by = "agreement_id") |>
  transmute(agreement_id, status, state,
            raw_agency = raw_agency_last, raw_type = raw_type_last, raw_county = raw_county_last,
            raw_support = raw_support_last, raw_moa = raw_moa_last, signed,
            moa_link = coalesce(moa_link, if_else(str_detect(coalesce(raw_moa_last, ""), "^https?://"), raw_moa_last, NA_character_)),
            addendum_link)

agreements <- bind_rows(active, gone) |>
  transmute(
    agreement_id, status, state,
    # published as ice_county
    raw_county,
    # missing counties arrive as #N/A-style text, not blanks
    county = str_to_title(str_squish(raw_county)),
    county = if_else(str_to_lower(county) %in% c("#na", "#n/a", "na", "n/a"), NA_character_, county),
    agency = str_squish(raw_agency),
    signed,
    # sentinel string, not NA: needs_review keys off this exact value
    moa = case_when(
      !is.na(moa_link) ~ moa_link,
      str_to_lower(str_trim(raw_moa)) == "link pending" ~ "pending",
      TRUE ~ NA_character_
    ),
    addendum = addendum_link,
    support_type = str_to_title(norm_support_key(raw_support)),
    ice_type = str_squish(raw_type),
    type_clean = str_to_lower(str_squish(raw_type)),
    support_clean = str_to_lower(norm_support_key(raw_support))
  ) |>
  # keys on cleaned values, so it must follow the title-casing pass above
  left_join(county_name_fixes |> filter(!is.na(agency)), by = c("state", "agency", "county")) |>
  left_join(county_name_fixes |> filter(is.na(agency)) |> select(-agency), by = c("state", "county"), suffix = c("", "_any")) |>
  mutate(county = na_if(coalesce(county_fixed, county_fixed_any, county), "")) |>
  select(-county_fixed, -county_fixed_any) |>
  left_join(agency_type_fixes, by = c("state", "agency", "type_clean")) |>
  mutate(type_clean = coalesce(type_fixed, type_clean)) |>
  select(-type_fixed)

# a pending agreement takes a held MOA that ICE posted but hasn't linked; the sheet's link wins once added
pending_held <- agreements |>
  filter(coalesce(moa, "pending") == "pending") |>
  left_join(state_xwalk |> select(state = state_full, state_abbr), by = "state") |>
  mutate(model = support_abbr(norm_support_key(support_type))) |>
  filter(model %in% c("JEM", "TFM", "WSO"), !is.na(state_abbr), !is.na(signed)) |>
  transmute(agreement_id, file = pmap(list(agency, state_abbr, model, signed), moa_candidate_files)) |>
  unnest(file) |>
  inner_join(held_moas, by = "file") |>
  distinct(agreement_id, held_url = url) |>
  filter(n() == 1, .by = agreement_id)

agreements <- agreements |>
  left_join(pending_held, by = "agreement_id") |>
  mutate(moa = coalesce(held_url, moa)) |>
  select(-held_url) |>
  left_join(moa_link_fixes, by = c("state", "agency", "support_type", "signed")) |>
  mutate(moa_fixed = !is.na(moa_url) & coalesce(moa, "pending") == coalesce(moa_linked, "pending"),
         moa = if_else(moa_fixed, moa_url, moa)) |>
  select(-moa_url, -moa_linked) |>
  # a sheet that links a standalone addendum in the MOA cell links no MOA
  mutate(moa = if_else(moa %in% moa_addenda$addendum_url, NA_character_, moa)) |>
  # the county an agency names is the county it is in, overriding the sheet
  mutate(county_named = county_from_agency_name(agency, state, counties_ref),
         county_from_name = !is.na(county_named) & (is.na(county) | county_named != county),
         county = coalesce(county_named, county)) |>
  select(-county_named) |>
  # a plain police department typed County is municipal; a county's sheriff/jail/police typed otherwise is County
  mutate(
    agency_lower = str_to_lower(agency),
    plain_police = str_detect(agency_lower, "\\bpolice (department|dept)\\b") &
      !str_detect(agency_lower, "\\b(county|parish|university|college|school|state|airport|port|regional|authority|tribal)\\b"),
    county_office = str_detect(agency_lower, "\\b(county|parish)\\b") &
      str_detect(agency_lower, "\\b(sheriff|jail|detention|correction|constable|police department)\\b") &
      !str_detect(agency_lower, "\\bregional\\b"),
    type_from_name = case_when(
      plain_police & type_clean == "county" ~ "municipality",
      county_office & type_clean %in% c("municipality", "state agency", "state") ~ "county",
      TRUE ~ NA_character_
    ),
    type_clean = coalesce(type_from_name, type_clean)
  ) |>
  (\(d) {
    message(sprintf("name rules: %d counties taken from the agency's name, %d TYPEs corrected by the name",
                    sum(d$county_from_name), sum(!is.na(d$type_from_name))))
    d
  })() |>
  left_join(identities |> select(agreement_id, agency_id), by = "agreement_id", relationship = "one-to-one") |>
  left_join(level_manual, by = "agency_id") |>
  mutate(
    type_level = case_when(
      type_clean %in% c("state agency", "state") ~ "State",
      type_clean == "county" ~ "County",
      type_clean == "municipality" ~ "Municipal",
      TRUE ~ NA_character_
    ),
    # one of eight levels; precedence: manual list, name rules, ICE's TYPE, then the name alone where no TYPE was printed
    name_level = case_when(
      is_constable_district(agency, state) ~ "Constable District",
      is_county_constable(agency, state) ~ "County",
      is_campus_agency(agency) ~ "Campus",
      is_judicial_district_agency(agency) ~ "Judicial District",
      is_port_agency(agency) ~ "Port",
      is_regional_agency(agency) ~ "Regional",
      # a sheriff or jail naming its own county is the county's whatever TYPE says
      is_exact_county_pattern(agency, county) ~ "County",
      TRUE ~ NA_character_
    ),
    jurisdiction_level = coalesce(manual_level, name_level, type_level,
                                  na_if(str_to_title(agency_level_from_name(agency, state)), "Unknown")),
    jurisdiction_level_source = case_when(
      !is.na(manual_level) ~ "Manual list",
      !is.na(name_level) ~ "Name pattern",
      !is.na(type_level) ~ "ICE type",
      !is.na(jurisdiction_level) ~ "Name, no ICE type",
      TRUE ~ NA_character_
    ),
    # jail models are a point at the jail; a task force is the body's territory
    geometry_type = case_when(
      support_clean %in% c("jail enforcement model", "warrant service officer", "jail & task force") ~ "Point",
      support_clean == "task force model" & !is.na(jurisdiction_level) ~ "Polygon",
      TRUE ~ NA_character_
    )
  ) |>
  # identity-level only; 6-make-agreement-level-sf.R adds the other review reasons
  mutate(needs_review = is.na(geometry_type)) |>
  left_join(
    identities |>
      select(agreement_id, signed_source, agreement_lineage_id, succeeded_by,
             first_appeared, first_appeared_source, last_appeared, removed_by, removed_by_source,
             removal_flag, latest_sheet_row, latest_sheet, latest_sheet_url, n_sheet_rows, identity_resolution),
    by = "agreement_id", relationship = "one-to-one"
  ) |>
  select(
    agreement_id, status, state, county, raw_county, agency, ice_type, jurisdiction_level, jurisdiction_level_source, support_type, signed, signed_source,
    moa, addendum, geometry_type, needs_review,
    first_appeared, first_appeared_source, last_appeared, removed_by, removed_by_source, removal_flag,
    agency_id, agreement_lineage_id, succeeded_by, latest_sheet_row, latest_sheet, latest_sheet_url, n_sheet_rows,
    identity_resolution
  ) |>
  arrange(desc(last_appeared), latest_sheet_row, first_appeared, agreement_id)

# every addendum of an agreement, dated from inputs/moa-addenda.csv; one found but not in the file is published undated
listed_addenda <- agreements |>
  select(agreement_id, state, agency, support_type, signed, moa) |>
  inner_join(moa_addenda, by = c("state", "agency", "support_type", "signed"), relationship = "one-to-many")
unmatched_addenda <- anti_join(moa_addenda, listed_addenda, by = c("state", "agency", "support_type", "signed", "addendum_url"))
if (nrow(unmatched_addenda)) {
  message(nrow(unmatched_addenda), " addend(a) in inputs/moa-addenda.csv match no agreement; kept as the record")
}
# an addendum amends the agreement in force: signed on or after it, before the next signing of the same agency and model
misdated_addenda <- listed_addenda |>
  left_join(agreements |> select(agreement_id, succeeded_by), by = "agreement_id") |>
  left_join(agreements |> select(succeeded_by = agreement_id, next_signed = signed), by = "succeeded_by") |>
  filter(addendum_signed < signed | coalesce(addendum_signed >= next_signed, FALSE))
if (nrow(misdated_addenda)) {
  stop("addenda in inputs/moa-addenda.csv signed outside their agreement's term: ",
       paste(misdated_addenda$agreement_id, misdated_addenda$addendum_signed, collapse = ", "))
}

# ICE binds some addenda into the MOA's own PDF: find such pages in every held copy of an agreement's MOA.
# Each file is read once and cached by hash.
addendum_pages_cache <- "data/intermediate/cache-moa-addendum-pages.parquet"
# a copy belongs to an MOA by the full url it came from, less any Wayback prefix, or by the name it was saved under.
# Not by the url's file name: ICE reused names across folders (r_287gazdeptofcorrections.pdf is the 2013 MOA
# under foia/ and, in 2020, the 2016 one under 287gMOA/, which we saved as the 2016 MOA).
url_key <- \(u) u |> str_remove("^https?://web\\.archive\\.org/web/[^/]+/") |> str_remove("^https?://") |>
  str_remove("[?#].*$") |> map_chr(\(x) tryCatch(URLdecode(x), error = \(e) x)) |> str_to_lower()
moa_links <- agreements |> filter(!is.na(moa), moa != "pending") |> transmute(agreement_id, agency, moa, key = url_key(moa))
held_pdfs <- moa_manifest |>
  filter(on_disk, !is.na(file_hash), str_detect(path_now, regex("\\.pdf$", ignore_case = TRUE))) |>
  transmute(file_hash, path_now, key = url_key(na_if(url, "")), name = doc_key(path_now))
moa_copies <- bind_rows(
  held_pdfs |> filter(!is.na(key)) |> select(-name) |> inner_join(moa_links, by = "key", relationship = "many-to-many"),
  held_pdfs |> select(-key) |> inner_join(moa_links |> mutate(name = doc_key(moa)), by = "name", relationship = "many-to-many") |>
    select(-name)
) |>
  distinct(file_hash, agreement_id, .keep_all = TRUE)
# pages without a text layer (scans) are read by tesseract when it is installed; `ocr` records whether a file's
# scanned pages were read, so a file cached without OCR is read again once tesseract is available
has_ocr <- nzchar(Sys.which("tesseract"))
page_cache <- if (file.exists(addendum_pages_cache)) arrow::read_parquet(addendum_pages_cache) else
  tibble(file_hash = character(), page = integer(), text = character(), ocr = logical())
if (!"ocr" %in% names(page_cache)) page_cache$ocr <- NA
ocr_done <- page_cache |> summarise(ocr = all(coalesce(ocr, FALSE)), .by = file_hash)
# every held PDF, not only current MOA copies, so CI knows exactly which new files to pull from LFS
unread <- held_pdfs |>
  distinct(file_hash, .keep_all = TRUE) |>
  left_join(ocr_done, by = "file_hash") |>
  filter(is.na(ocr) | (has_ocr & !ocr)) |>
  mutate(lfs_pointer = map_lgl(path_now, \(p) identical(readBin(p, "raw", 24), charToRaw("version https://git-lfs."))))
if (any(unread$lfs_pointer)) {
  message(sum(unread$lfs_pointer), " held PDF(s) are Git LFS pointers and were not checked for addenda; `git lfs pull` them")
}
unread <- filter(unread, !lfs_pointer)
if (nrow(unread) && !requireNamespace("pdftools", quietly = TRUE)) {
  message("pdftools is not installed; ", nrow(unread), " held PDF(s) were not checked for addenda")
  unread <- unread[0, ]
}
if (nrow(unread)) {
  read_addendum_pages <- \(p) {
    # a file too damaged to read (truncated Wayback captures) has no pages and is cached as read
    text <- tryCatch(pdftools::pdf_text(p), error = \(e) character())
    # a scan can still carry a few words of text layer, such as a digital-signature stamp
    scanned <- which(str_count(text, "[A-Za-z]{3,}") < 40)
    if (has_ocr) {
      for (pg in scanned) {
        png <- tempfile(fileext = ".png")
        text[pg] <- tryCatch({
          suppressWarnings(pdftools::pdf_convert(p, format = "png", pages = pg, dpi = 200, filenames = png, verbose = FALSE))
          paste(system2("tesseract", c(shQuote(png), "stdout"), stdout = TRUE, stderr = FALSE), collapse = "\n")
        }, error = \(e) text[pg])
        unlink(png)
      }
    }
    text <- str_to_lower(text)
    hits <- which(str_detect(text, "addendum\\s+to\\s+(modify|extend)|hereby\\s+agree\\s+to\\s+(modify|extend)"))
    ocr <- has_ocr || !length(scanned)
    # a file with no addendum page is cached with page NA
    if (length(hits)) tibble(page = hits, text = text[hits], ocr = ocr) else tibble(page = NA_integer_, text = NA_character_, ocr = ocr)
  }
  pages <- parallel::mclapply(unread$path_now, read_addendum_pages, mc.cores = max(1, parallel::detectCores() - 1))
  page_cache <- unread |>
    select(file_hash) |>
    mutate(pages = pages) |>
    unnest(pages) |>
    bind_rows(page_cache |> anti_join(unread, by = "file_hash"))
  arrow::write_parquet(page_cache, addendum_pages_cache)
}
if (!has_ocr && any(!page_cache$ocr, na.rm = TRUE)) {
  message(n_distinct(page_cache$file_hash[!page_cache$ocr]), " held PDF(s) have scanned pages not checked for addenda; install tesseract")
}
# a recorded addendum covers its own #page= and the MOA page it is also bound as (moa_page)
recorded_pages <- bind_rows(
  listed_addenda |>
    filter(str_detect(addendum_url, "#page=\\d+$")) |>
    transmute(agreement_id, key = url_key(addendum_url), page = as.integer(str_extract(addendum_url, "\\d+$"))),
  listed_addenda |>
    filter(!is.na(moa_page)) |>
    transmute(agreement_id, key = url_key(moa), page = moa_page)
)
# the page must name the agency: ICE's Terrell County TX 2018 PDF carries Teller County CO's addendum.
# Letters only and one letter's slack, since scanned text reads "f loyd" and "l{orry"
agency_word <- \(agency) coalesce(str_extract(str_to_lower(agency), "\\b(?!(?:city|town|village|county|state|the)\\b)[a-z]{4,}\\b"), "")
names_agency <- \(text, agency) map2_lgl(str_remove_all(text, "[^a-z]"), agency_word(agency), \(t, w) agrepl(w, t, max.distance = 1, fixed = TRUE))
found_addenda <- moa_copies |>
  inner_join(page_cache |> filter(!is.na(page)), by = "file_hash", relationship = "many-to-many") |>
  filter(names_agency(text, agency)) |>
  distinct(agreement_id, moa, key, page) |>
  anti_join(recorded_pages, by = c("agreement_id", "key", "page")) |>
  transmute(agreement_id, addendum_url = str_c(moa, "#page=", page), found_in = "MOA PDF page")
linked_addenda <- agreements |>
  filter(!is.na(addendum)) |>
  select(agreement_id, addendum_url = addendum) |>
  anti_join(listed_addenda, by = c("agreement_id", "addendum_url")) |>
  mutate(found_in = "Sheet link")
undated_addenda <- bind_rows(linked_addenda, found_addenda)
# a review list for the update PR: each needs its signature dates read into inputs/moa-addenda.csv
write_csv(undated_addenda, "data/qa/undated-addenda.csv", na = "")
if (nrow(undated_addenda)) {
  message(nrow(undated_addenda), " addend(a) not in inputs/moa-addenda.csv are published undated (data/qa/undated-addenda.csv)")
}

agreements <- agreements |>
  select(-addendum) |>
  left_join(
    bind_rows(listed_addenda |> select(agreement_id, addendum_url, addendum_signed), undated_addenda |> select(-found_in)) |>
      arrange(agreement_id, addendum_signed) |>
      summarise(addendum = str_c(addendum_url, collapse = "; "),
                addendum_signed = if (all(is.na(addendum_signed))) NA_character_
                                  else str_c(coalesce(as.character(addendum_signed), "Undated"), collapse = "; "),
                .by = agreement_id),
    by = "agreement_id", relationship = "one-to-one"
  ) |>
  relocate(addendum, addendum_signed, .after = moa)

stopifnot(
  "every jurisdiction level is one of the eight" =
    all(is.na(agreements$jurisdiction_level) | agreements$jurisdiction_level %in% JURISDICTION_LEVELS),
  "the agreements dataset and the identity table must hold the same agreements" =
    setequal(agreements$agreement_id, identities$agreement_id) && !anyDuplicated(agreements$agreement_id),
  "every agreement carries an agency_id" = !anyNA(agreements$agency_id),
  "active agreements must be exactly the current publication's signed identities" =
    sum(agreements$status == "Active") ==
      n_distinct(observation_ids$agreement_id[observation_ids$publication_id == current_id & !is.na(observation_ids$agreement_id)])
)

# warn, not stop: a failure here aborts the daily workflow and drops the snapshot
unknown_states <- setdiff(agreements$state, state_xwalk$state_full)
if (length(unknown_states) > 0) {
  warning("State value(s) not in the xwalk even after snapping: ", paste(unknown_states, collapse = ", "))
}

arrow::write_parquet(agreements, "data/intermediate/agreements.parquet")
n_link_fixed <- agreements |>
  inner_join(moa_link_fixes, by = c("state", "agency", "support_type", "signed")) |>
  filter(moa == moa_url) |>
  nrow()
message(sprintf("MOA links: %d pending agreements take a held PDF, %d take inputs/moa-link-fixes.csv; %d still pending",
                nrow(pending_held), n_link_fixed, sum(agreements$moa == "pending", na.rm = TRUE)))
message(sprintf("agreements: %d (%d active, %d superseded, %d removed)",
                nrow(agreements), sum(agreements$status == "Active"),
                sum(agreements$status == "Superseded"), sum(agreements$status == "Removed")))
