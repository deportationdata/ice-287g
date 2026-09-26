# The agreements dataset: every identity ever observed, active rows carrying the
# current sheet's values and removed rows their last-observed ones, cleaned once
# -> data/agreements.parquet
library(tidyverse)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")
# a county typo is fixed wherever printed, a fix naming an agency applies to it alone, a blank
# fix removes the county and a blank county fills one ICE never printed
county_name_fixes <- read_csv("inputs/county-name-fixes.csv", col_types = "ccccc", na = character()) |>
  transmute(state, agency = na_if(agency, ""), county = na_if(county, ""), county_fixed)
# ICE's TYPE for an agency, keyed on the erroneous value so a fix does nothing once ICE corrects it
agency_type_fixes <- read_csv("inputs/agency-type-fixes.csv", col_types = "ccccc") |>
  transmute(state, agency, type_clean = str_to_lower(type), type_fixed = str_to_lower(type_fixed))
# the agency's own MOA where the sheet links another agency's, used only while it links that
# file (with moa_linked blank, while the sheet says link pending); a row does nothing once ICE fixes it
moa_link_fixes <- read_csv("inputs/moa-link-fixes.csv", col_types = "cccDccc") |>
  transmute(state, agency, support_type = str_to_title(norm_support_key(support_type)), signed, moa_linked, moa_url)
# every MOA PDF held under agreements/, by the ice.gov url it was fetched from
held_moas <- snapshot_manifests("agreements") |>
  filter(on_disk, str_detect(coalesce(url, ""), "^https://www\\.ice\\.gov/doclib/287gMOA/")) |>
  distinct(url) |>
  mutate(file = basename(str_remove(url, "[?#].*$")))
# the Census counties, for the county an agency names; the hand list of jurisdiction levels,
# keyed to agencies so an alias spelling reaches the same row
counties_ref <- arrow::read_parquet("data/intermediate/reference-counties.parquet")
aliases <- read_agency_aliases(state_xwalk)
level_manual <- read_csv("inputs/manual-jurisdiction-levels.csv", col_types = cols(.default = "c")) |>
  left_join(state_xwalk |> select(state = state_full, state_abbr), by = "state") |>
  transmute(agency_id = agency_id_of(state, agency, state_abbr, aliases), manual_level = jurisdiction_level) |>
  distinct(agency_id, .keep_all = TRUE)
stopifnot("every level in inputs/manual-jurisdiction-levels.csv is one of the eight" =
            all(level_manual$manual_level %in% JURISDICTION_LEVELS))

# the county an agency names ("Lavaca County Sheriff's Office"): the longest run of up to three
# words before County or Parish that names a real county of the agency's state
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

# active rows: the current publication, one row per identity (ICE prints a few
# agreements twice; the first row speaks for the identity)
active <- observations |>
  filter(publication_id == current_id) |>
  inner_join(observation_ids, by = c("publication_id", "sheet_row"), relationship = "one-to-one") |>
  filter(!is.na(agreement_id)) |>
  left_join(links, by = "sheet_row") |>
  arrange(sheet_row) |>
  distinct(agreement_id, .keep_all = TRUE) |>
  transmute(agreement_id, status = "active", state,
            raw_agency, raw_type, raw_county, raw_support, raw_moa,
            signed, moa_link, addendum_link)

# removed and superseded rows read as ICE last printed them, links included: a
# workbook's links come from the workbook the agreement was last seen in, and an
# archived page's table already carries the href in its MOA cell
last_seen <- identities |>
  filter(status != "active") |>
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
  filter(status != "active") |>
  left_join(gone_links, by = "agreement_id") |>
  transmute(agreement_id, status, state,
            raw_agency = raw_agency_last, raw_type = raw_type_last, raw_county = raw_county_last,
            raw_support = raw_support_last, raw_moa = raw_moa_last, signed,
            moa_link = coalesce(moa_link, if_else(str_detect(coalesce(raw_moa_last, ""), "^https?://"), raw_moa_last, NA_character_)),
            addendum_link)

agreements <- bind_rows(active, gone) |>
  transmute(
    agreement_id, status, state,
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
    # ICE's SUPPORT TYPE and TYPE as printed
    ice_support_type = str_squish(raw_support),
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

# an MOA ICE has posted to ice.gov under a name its pattern predicts but not yet linked from
# its sheet (0-acquire-moa-probe.R fetches them): a pending agreement takes the held PDF, and
# the sheet's own link takes over once ICE adds it
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
  # the county an agency names is the county it is in: it fills a blank sheet county and replaces
  # one that is not a real county of the state or is a different real county. Tested 2026-09-13
  # over the 1,087 agencies naming a county: none disagrees with a real sheet county, and the two
  # it overrides (Bradford PA printed as Bedford, Madison TN as Macon) ICE corrected itself
  mutate(county_named = county_from_agency_name(agency, state, counties_ref),
         county_from_name = !is.na(county_named) & (is.na(county) | county_named != county),
         county = coalesce(county_named, county)) |>
  select(-county_named) |>
  # ICE's TYPE against the name, two narrow patterns tested over everything ICE has printed
  # (74 flips, none wrong): a plain police department typed County is a municipality's, and a
  # county's sheriff, jail, constable or police department typed otherwise is the county's
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
    # what the agency covers, one of eight levels: the hand list, then the name rules (each
    # tested over every agency with no false positive), then ICE's TYPE, then, for the
    # rosters that carried no TYPE, the name alone
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
      !is.na(manual_level) ~ "manual",
      !is.na(name_level) ~ "name_rule",
      !is.na(type_level) ~ "ice_type",
      !is.na(jurisdiction_level) ~ "agency_name",
      TRUE ~ NA_character_
    ),
    # jail models are a point at the jail; a task force is the body's territory
    geometry_type = case_when(
      support_clean %in% c("jail enforcement model", "warrant service officer", "jail & task force") ~ "point",
      support_clean == "task force model" & !is.na(jurisdiction_level) ~ "polygon",
      TRUE ~ NA_character_
    )
  ) |>
  # identity-level concerns only; the geometry, roster and document flags are
  # composed into review_reason by 6-make-agreement-level-sf.R
  mutate(needs_review = is.na(geometry_type)) |>
  left_join(
    identities |>
      select(agreement_id, agreement_lineage_id, succeeded_by,
             first_appeared, first_appeared_source, last_appeared, removed_by, removed_by_source,
             removal_flag, latest_sheet_row, latest_sheet, latest_sheet_url, n_sheet_rows, identity_resolution),
    by = "agreement_id", relationship = "one-to-one"
  ) |>
  select(
    agreement_id, status, state, county, agency, ice_type, jurisdiction_level, jurisdiction_level_source, support_type, ice_support_type, signed,
    moa, addendum, geometry_type, needs_review,
    first_appeared, first_appeared_source, last_appeared, removed_by, removed_by_source, removal_flag,
    agency_id, agreement_lineage_id, succeeded_by, latest_sheet_row, latest_sheet, latest_sheet_url, n_sheet_rows,
    identity_resolution
  ) |>
  arrange(desc(last_appeared), latest_sheet_row, first_appeared, agreement_id)

stopifnot(
  "every jurisdiction level is one of the eight" =
    all(is.na(agreements$jurisdiction_level) | agreements$jurisdiction_level %in% JURISDICTION_LEVELS),
  "the agreements dataset and the identity table must hold the same agreements" =
    setequal(agreements$agreement_id, identities$agreement_id) && !anyDuplicated(agreements$agreement_id),
  "every agreement carries an agency_id" = !anyNA(agreements$agency_id),
  "active agreements must be exactly the current publication's signed identities" =
    sum(agreements$status == "active") ==
      n_distinct(observation_ids$agreement_id[observation_ids$publication_id == current_id & !is.na(observation_ids$agreement_id)])
)

# An unplaceable state would silently miss every state-keyed join. Warn rather
# than stop: a failure here aborts the daily workflow and drops the snapshot.
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
                nrow(agreements), sum(agreements$status == "active"),
                sum(agreements$status == "superseded"), sum(agreements$status == "removed")))
