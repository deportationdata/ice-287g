# One record per agency; other sources become claims arbitrated against ICE, differences recorded
# -> data/agencies.parquet, data/intermediate/historical-source-claims.csv,
#    data/intermediate/historical-source-claims-unresolved.csv, data/agency-disagreements.csv,
#    data/qa/historical-summary.md
library(tidyverse)

source("code/functions.R")

sources <- read_sources()
src <- \(id) sources |> filter(source_id == id)
xwalk <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")
aliases <- read_agency_aliases(xwalk)
identities <- arrow::read_parquet("data/intermediate/identity-agreements.parquet")
ice <- arrow::read_parquet("data/intermediate/identity-agencies.parquet")
spellings <- arrow::read_parquet("data/intermediate/identity-agency-spellings.parquet")
agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")
pubs <- arrow::read_parquet("data/intermediate/sheet-publications.parquet")
obs_ids <- arrow::read_parquet("data/intermediate/sheet-row-agreements.parquet")
geography <- arrow::read_parquet(
  "data/agreements-sf.parquet",
  col_select = c("agreement_id", "county_fips")
)

# only an ICE press release may add an agency; other sources attest to existing ones
MINTING_SOURCES <- "ICE press release"

# --- claims: each source reduced to (state, agency, field, value, as_of) --------
rd <- \(f) read_csv(f, col_types = cols(.default = "c"), progress = FALSE) |> mutate(row = row_number())
claim <- function(source_id, rows, field, value, value_date = as.Date(NA), as_of = src(source_id)$as_of,
                  evidence = paste0(src(source_id)$provenance_file, "#row", rows$row)) {
  tibble(source_id, state = rows$state, agency_raw = rows$agency, field, value = as.character(value),
         value_date = as.Date(value_date), as_of = as.Date(as_of), evidence)
}

lists <- rd("data/intermediate/historical-ice-lists.csv")
oig <- rd("data/intermediate/historical-oig-2009.csv")
idx <- rd("data/intermediate/historical-ice-archive-index.csv")
press <- rd(src("ICE press release")$provenance_file)
stopifnot("press claim fields are signed, rescinded, listed or model" =
            all(press$field %in% c("signed", "rescinded", "listed", "model")))

claims <- bind_rows(
  claim("ICE undated lists", lists, "listed", "TRUE", as_of = lists$as_of,
        evidence = paste0("sheets/sheets_wayback_pre2011/", lists$capture, "_s3.html")),
  claim("OIG 2009 report", oig, if_else(oig$status == "pending", "pending", "listed"), "TRUE"),
  oig |> filter(!is.na(date_signed_original)) |>
    (\(r) claim("OIG 2009 report", r, "signed", r$date_signed_original, value_date = r$date_signed_original))(),
  oig |> filter(!is.na(model)) |> (\(r) claim("OIG 2009 report", r, "model", r$model))(),
  claim("ICE archive index", idx, "moa_file", idx$moa_file),
  idx |> filter(!is.na(date_signed)) |>
    (\(r) claim("ICE archive index", r, "signed", r$date_signed, value_date = r$date_signed))(),
  claim("ICE press release", press, press$field, press$value,
        value_date = if_else(press$field %in% c("signed", "rescinded"), as.Date(press$value), as.Date(NA)),
        as_of = press$as_of, evidence = press$evidence)
)

# --- resolve every claim to an agency ---------------------------------------
registry <- agency_registry(ice, spellings, aliases)
# a source's one signing date for an agency lets the date tier resolve its rows
signed_of <- claims |>
  filter(field == "signed") |>
  distinct(source_id, state, agency_raw, value_date) |>
  add_count(source_id, state, agency_raw) |>
  filter(n == 1) |>
  select(-n)
row_signed <- claims |> select(source_id, state, agency_raw) |>
  left_join(signed_of, by = c("source_id", "state", "agency_raw")) |> pull(value_date)
resolved <- resolve_agency(claims$state, claims$agency_raw, claims$source_id %in% MINTING_SOURCES,
                                registry, ice, identities, xwalk, aliases, signed = row_signed)
claims <- claims |>
  mutate(agency_id = resolved$agency_id, resolution = resolved$resolution,
         state = coalesce(resolved$state, state)) |>
  arrange(source_id, state, agency_raw, field, as_of) |>
  select(all_of(claim_columns))
stopifnot(
  "every claim belongs to a registered source" = all(claims$source_id %in% sources$source_id),
  "a resolved claim carries an agency id" = all(is.na(claims$agency_id) == str_starts(claims$resolution, "unresolved")),
  "only a minting source may mint" = all(claims$resolution != "minted" | claims$source_id %in% MINTING_SOURCES)
)
pa <- claims |> filter(!is.na(agency_id))

# --- the universe: ICE's agencies plus what a minting source added ----------
minted <- pa |>
  filter(resolution == "minted") |>
  arrange(as_of) |>
  distinct(agency_id, .keep_all = TRUE) |>
  transmute(agency_id, state, state_key = norm_state(state), display_agency = agency_raw) |>
  left_join(xwalk |> select(state = state_full, state_abbr), by = "state")

# --- ICE's record: listing window, models, signing dates, removal ------------
ice_record <- identities |>
  group_by(agency_id) |>
  arrange(first_seq, .by_group = TRUE) |>
  summarise(
    ice_listed_from = min(first_appeared), ice_listed_from_source = first_appeared_source[which.min(first_appeared)],
    ice_listed_to = max(last_appeared),
    n_agreements = n(), n_active = sum(status == "Active"),
    removed_between_from = if (any(status == "Active")) as.Date(NA) else max(last_appeared),
    removed_between_to = if (any(status == "Active")) as.Date(NA) else max(removed_by, na.rm = TRUE),
    models = paste(unique(support_abbr(support_key)), collapse = "; "),
    # in signing order; agreements ICE listed out of that order, or on the same sheet, would otherwise jumble it
    model_history = paste(sprintf("%s %s", support_abbr(support_key), signed)[order(signed, first_seq)], collapse = " -> "),
    .groups = "drop"
  ) |>
  mutate(terminated = n_active == 0,
         terminated_basis = if_else(n_active > 0, "listed on the current ICE sheet", "absent from the current ICE sheet"))

# the earliest signing date wins; ICE's sheet is named as source when it ties
signed_claims <- bind_rows(
  identities |> transmute(agency_id, source_id = "ICE sheet", value_date = signed),
  pa |> filter(field == "signed", !is.na(value_date)) |> select(agency_id, source_id, value_date)
)
first_signed <- signed_claims |>
  arrange(agency_id, value_date, source_id != "ICE sheet", source_id) |>
  distinct(agency_id, .keep_all = TRUE) |>
  transmute(agency_id, first_signed = value_date, first_signed_source = source_id)
signing_dates <- signed_claims |>
  distinct(agency_id, value_date, source_id) |>
  arrange(agency_id, value_date, source_id) |>
  summarise(sources = paste(source_id, collapse = ", "), .by = c(agency_id, value_date)) |>
  summarise(signing_dates = paste(sprintf("%s (%s)", value_date, sources), collapse = "; "),
            latest_signed = max(value_date), .by = agency_id)

# --- what every source attests: the widest window the evidence speaks to --------
attested <- pa |>
  mutate(spoken_to = case_when(field %in% c("listed", "pending") ~ as_of,
                               field %in% c("signed", "rescinded") ~ value_date,
                               TRUE ~ as.Date(NA))) |>
  filter(!is.na(spoken_to)) |>
  summarise(attested_active_from = min(spoken_to), attested_active_to = max(spoken_to),
            source_ids = paste(sort(unique(source_id)), collapse = "; "), .by = agency_id)
rescinded <- pa |>
  filter(field == "rescinded", !is.na(value_date)) |>
  arrange(agency_id, desc(value_date)) |>
  distinct(agency_id, .keep_all = TRUE) |>
  transmute(agency_id, rescinded_on = value_date, rescinded_source = source_id)

moa <- bind_rows(
  pa |> filter(field == "moa_file", !is.na(value)) |> transmute(agency_id, moa_file = value),
  identities |> filter(str_detect(coalesce(raw_moa_last, ""), "^https?://")) |>
    transmute(agency_id, moa_file = basename(raw_moa_last)),
  agreements |> filter(str_detect(coalesce(moa, ""), "^https?://")) |> transmute(agency_id, moa_file = basename(moa))
) |>
  distinct(agency_id, moa_file)

# --- jurisdiction and county --------------------------------------------------------
# the agency takes its active or latest agreement's level and counties ("; "-joined, none for a state agency); one ICE never listed takes its name's level
level_modern <- agreements |>
  arrange(desc(status == "Active"), desc(last_appeared)) |>
  distinct(agency_id, .keep_all = TRUE) |>
  left_join(geography, by = "agreement_id") |>
  transmute(agency_id, jurisdiction_level, county_fips)

# --- assemble ---------------------------------------------------------------------
agencies <- bind_rows(ice |> select(agency_id, state, state_abbr, state_key, canonical_agency, display_agency) |>
                            mutate(ice_published = TRUE),
                          minted |> mutate(ice_published = FALSE)) |>
  arrange(agency_id, desc(ice_published)) |>
  distinct(agency_id, .keep_all = TRUE) |>
  left_join(ice_record, by = "agency_id") |>
  left_join(first_signed, by = "agency_id") |>
  left_join(signing_dates, by = "agency_id") |>
  left_join(attested, by = "agency_id") |>
  left_join(rescinded, by = "agency_id") |>
  left_join(level_modern, by = "agency_id") |>
  mutate(
    jurisdiction_level = coalesce(jurisdiction_level,
                                  str_to_title(na_if(agency_level_from_name(display_agency, state), "unknown"))),
    n_agreements = coalesce(n_agreements, 0L), n_active = coalesce(n_active, 0L),
    is_current = n_active > 0,
    # an agreement ICE announced and never listed ended when it was rescinded
    terminated = coalesce(terminated, !is.na(rescinded_on)),
    terminated_basis = case_when(!is.na(terminated_basis) ~ terminated_basis,
                                 !is.na(rescinded_on) ~ sprintf("rescinded %s (%s)", rescinded_on, rescinded_source),
                                 TRUE ~ "never on an ICE roster we hold"),
    attested_active_from = pmin(ice_listed_from, attested_active_from, na.rm = TRUE),
    attested_active_to = pmax(ice_listed_to, attested_active_to, na.rm = TRUE),
    source_ids = case_when(ice_published & !is.na(source_ids) ~ paste("ICE sheet", source_ids, sep = "; "),
                           ice_published ~ "ICE sheet", TRUE ~ source_ids)
  ) |>
  select(agency_id, state, state_abbr, display_agency, jurisdiction_level, county_fips, ice_published, is_current,
         n_agreements, n_active, ice_listed_from, ice_listed_from_source, ice_listed_to, removed_between_from, removed_between_to,
         terminated, terminated_basis, attested_active_from, attested_active_to,
         first_signed, first_signed_source, latest_signed, signing_dates, models, model_history,
         source_ids) |>
  arrange(state, display_agency)

stopifnot("every jurisdiction level is one of the eight" =
            all(is.na(agencies$jurisdiction_level) | agencies$jurisdiction_level %in% JURISDICTION_LEVELS))
if (anyNA(agencies$jurisdiction_level)) {
  warning(sum(is.na(agencies$jurisdiction_level)), " agency(s) have no jurisdiction level: ",
          paste(agencies$agency_id[is.na(agencies$jurisdiction_level)], collapse = ", "))
}

# --- disagreements: every place a source and ICE differ -------------------------
p_state <- agencies |> select(agency_id, p_state = state)
dis_state <- pa |>
  inner_join(p_state, by = "agency_id") |>
  filter(!is.na(state), state != p_state) |>
  distinct(agency_id, source_id, state, p_state) |>
  transmute(agency_id, kind = "state", field = "state", value_a = state, source_a = source_id,
            value_b = p_state, source_b = "registry", published_value = p_state,
            resolution_rule = "state comes from the registry; the source printed another")
dis_date <- pa |>
  filter(field == "signed", !is.na(value_date)) |>
  inner_join(identities |> summarise(ice_dates = list(unique(signed)), .by = agency_id), by = "agency_id") |>
  filter(!map2_lgl(value_date, ice_dates, \(d, s) d %in% s)) |>
  mutate(nearest = map2_chr(value_date, ice_dates, \(d, s) as.character(s[which.min(abs(as.numeric(d - s)))]))) |>
  distinct(agency_id, source_id, value_date, nearest) |>
  transmute(agency_id, kind = "date", field = "signed", value_a = as.character(value_date), source_a = source_id,
            value_b = nearest, source_b = "ICE sheet", published_value = NA_character_,
            resolution_rule = "ICE's dates are published as identities; the earliest of all dates is first_signed")
model_map <- c("Jail Enforcement" = "JEM", "Task Force" = "TFM", "Hybrid" = "JTF")
dis_model <- pa |>
  filter(field == "model", value %in% names(model_map)) |>
  inner_join(identities |> summarise(ice_models = list(unique(support_abbr(support_key))), .by = agency_id),
             by = "agency_id") |>
  filter(!map2_lgl(value, ice_models, \(m, s) model_map[[m]] %in% s)) |>
  distinct(agency_id, source_id, value, ice_models) |>
  transmute(agency_id, kind = "model", field = "model", value_a = value, source_a = source_id,
            value_b = map_chr(ice_models, paste, collapse = "; "), source_b = "ICE sheet",
            published_value = value_b, resolution_rule = "models are ICE's; a source's differing model is recorded only")
# a dated list's members against ICE's nearest publication at or before its date
pub_members <- obs_ids |>
  filter(!is.na(agency_id)) |>
  distinct(publication_id, agency_id) |>
  inner_join(pubs |> select(publication_id, published_on), by = "publication_id")
dis_presence <- pa |>
  filter(field == "listed") |>
  distinct(source_id, as_of) |>
  pmap(\(source_id, as_of) {
    nearest <- pubs |> filter(published_on <= as_of) |> slice_max(pub_seq, n = 1)
    if (!nrow(nearest)) return(NULL)
    ice_set <- pub_members |> filter(publication_id == nearest$publication_id) |> pull(agency_id)
    src_set <- pa |> filter(source_id == !!source_id, as_of == !!as_of, field == "listed") |> pull(agency_id) |> unique()
    bind_rows(
      tibble(agency_id = setdiff(src_set, ice_set), value_a = "listed", source_a = source_id,
             value_b = "absent", source_b = nearest$publication_id),
      tibble(agency_id = setdiff(ice_set, src_set), value_a = "absent", source_a = source_id,
             value_b = "listed", source_b = nearest$publication_id)
    ) |> mutate(kind = "presence", field = "listed", published_value = NA_character_,
                resolution_rule = sprintf("ICE publication nearest to %s decides ice_listed; the source's list is recorded", as_of))
  }) |>
  list_rbind()
dis_ambiguous <- claims |>
  filter(resolution == "unresolved_ambiguous") |>
  distinct(source_id, state, agency_raw) |>
  transmute(agency_id = NA_character_, kind = "ambiguous_resolution", field = "name", value_a = agency_raw,
            source_a = source_id, value_b = state, source_b = "registry", published_value = NA_character_,
            resolution_rule = "the name begins more than one agency in the state; left unresolved")
# one MOA file linked for two agencies is recorded for review, never reassigned
moa_keys <- moa |> transmute(agency_id, moa_file, file_key = str_to_lower(moa_file))
dis_moa <- moa_keys |>
  inner_join(moa_keys |> select(file_key, other = agency_id), by = "file_key", relationship = "many-to-many") |>
  filter(other != agency_id) |>
  distinct(agency_id, moa_file, other) |>
  transmute(agency_id, kind = "moa_file", field = "moa_file", value_a = moa_file, source_a = "moa_link",
            value_b = other, source_b = "moa_link", published_value = moa_file,
            resolution_rule = "the same MOA file is linked for another agency; recorded, not reassigned")
disagreements <- bind_rows(dis_state, dis_date, dis_model, dis_presence, dis_ambiguous, dis_moa) |>
  select(kind, agency_id, field, value_a, source_a, value_b, source_b, published_value, resolution_rule) |>
  arrange(kind, agency_id, source_a)

stopifnot(
  "every agency id is unique" = !anyDuplicated(agencies$agency_id),
  "every ICE agency is present" = all(ice$agency_id %in% agencies$agency_id),
  "every resolved sheet row belongs to an agency" =
    !any(is.na(obs_ids$agency_id) & !is.na(obs_ids$agreement_id))
)

dir.create("data", showWarnings = FALSE)
arrow::write_parquet(agencies, "data/agencies.parquet")
write_csv(claims, "data/intermediate/historical-source-claims.csv", na = "")
unresolved <- claims |>
  filter(str_starts(resolution, "unresolved") | resolution == "minted") |>
  distinct(source_id, state, agency_raw, resolution, agency_id) |>
  arrange(resolution, source_id, state, agency_raw)
write_csv(unresolved, "data/intermediate/historical-source-claims-unresolved.csv", na = "")
write_csv(disagreements, "data/intermediate/agency-disagreements.csv", na = "")

# the counts the documentation quotes, written here so they cannot drift
per_source <- claims |>
  summarise(as_of = if (all(is.na(as_of))) "—" else paste(sort(unique(as.character(as_of))), collapse = ", "),
            agencies = n_distinct(agency_id[!is.na(agency_id)]),
            listed = n_distinct(agency_id[field %in% c("listed", "pending") & !is.na(agency_id)]),
            signed = n_distinct(agency_id[field == "signed" & !is.na(agency_id)]),
            unresolved = n_distinct(paste(state, agency_raw)[str_starts(resolution, "unresolved")]),
            .by = source_id)
summary_md <- c(
  "<!-- generated by code/7-make-agencies.R; do not edit -->",
  sprintf("Agencies: **%d** across every era (%d published by ICE, %d added from ICE press releases); %d current.",
          nrow(agencies), sum(agencies$ice_published), sum(!agencies$ice_published), sum(agencies$is_current)),
  sprintf("Source claims: **%d** from %d sources; %d agencies no rule resolves (`data/intermediate/historical-source-claims-unresolved.csv`); %d disagreements recorded (%s).",
          nrow(claims), n_distinct(claims$source_id), sum(str_starts(unresolved$resolution, "unresolved")), nrow(disagreements),
          disagreements |> count(kind) |> mutate(s = paste(n, kind)) |> pull(s) |> paste(collapse = ", ")),
  "",
  "| source | as of | agencies | listed | signing dates | unresolved |",
  "|---|---|---|---|---|---|",
  per_source |> mutate(row = sprintf("| %s | %s | %d | %d | %d | %d |", source_id, as_of, agencies, listed, signed, unresolved)) |> pull(row)
)
dir.create("data/qa", showWarnings = FALSE)
writeLines(summary_md, "data/qa/historical-summary.md")

message(sprintf("agencies: %d (%d ICE-published, %d from press releases; %d current)",
                nrow(agencies), sum(agencies$ice_published), sum(!agencies$ice_published), sum(agencies$is_current)))
message(sprintf("claims: %d from %d sources; %d unresolved, %d minted", nrow(claims), n_distinct(claims$source_id),
                sum(str_starts(claims$resolution, "unresolved")), sum(claims$resolution == "minted")))
message(sprintf("disagreements: %d (%s)", nrow(disagreements),
                disagreements |> count(kind) |> mutate(s = paste(kind, n)) |> pull(s) |> paste(collapse = ", ")))
