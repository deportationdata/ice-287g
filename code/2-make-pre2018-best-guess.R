# Reconcile every parsed source into one record per (state, agency) partnership
# -> data/pre2018-{agreements-best-guess,source-attestation,
#    completeness-by-period,east-vs-ice-audit}.csv
#
# Published semantics, not derivable from the code below:
#   active_from/_to    interval censored: true start <= from, true end >= to
#   first_signed_best  earliest signing in ANY source, so adoption at best
#   roster-grade       ICE rosters, GAO-09-109, ICE FOIA (Yale, removal stats)
#   confidence         high = roster-grade + 1 other; medium = roster-grade or
#                      2 non-roster; low = single non-roster
#   removal stats      activity only: widen the window, never set a date, and
#                      are no census, so they stay out of the ledger

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(tidyr) })

ROOT <- "."  # run from the repo root
OUT  <- file.path(ROOT, "data")
need <- c("pre2018-roster-long.csv", "pre2018-kostandini-tableA1.csv")
miss <- need[!file.exists(file.path(OUT, need))]
if (length(miss)) {
  stop(
    "Run the 1-read-pre2018-* scripts first. Missing: ",
    paste(miss, collapse = ", "),
    ". inputs/pre2018/prebuilt/ holds the ORIGINAL archive's outputs and is ",
    "kept only as provenance -- copying those into data/ would silently ",
    "replace the current reconstruction with a much earlier one."
  )
}

rd <- function(f) if (file.exists(file.path(OUT, f))) read.csv(file.path(OUT, f), stringsAsFactors = FALSE) else NULL
ice  <- rd("pre2018-roster-long.csv")
kost <- rd("pre2018-kostandini-tableA1.csv")
yale <- rd("pre2018-yale-foia-2008.csv")
east <- rd("pre2018-east-county-spells.csv")
sec  <- rd("pre2018-secondary-rosters-long.csv")
rem  <- rd("pre2018-ice-removals-long.csv")
cw   <- read.csv(file.path(ROOT, "inputs", "pre2018", "crosswalk", "ice_agency_to_county.csv"), stringsAsFactors = FALSE)

# name normalisation for cross-source matching: norm_agency in functions.R
source("code/functions.R")

# ICE roster backbone
ice <- ice |>
  mutate(agency_key = norm_agency(agency),
         content_asof = as.Date(content_asof),
         date_signed  = as.Date(date_signed))
final_asof <- max(ice$content_asof, na.rm = TRUE)

ice_agg <- ice |>
  group_by(agency_key) |>
  summarise(
    state            = first(state),
    agency           = agency[which.max(content_asof)],   # latest spelling ICE used
    n_rosters        = n(),
    active_from      = min(content_asof),
    active_to        = max(content_asof),
    first_signed_ice = suppressWarnings(min(date_signed, na.rm = TRUE)),
    latest_signed    = suppressWarnings(max(date_signed, na.rm = TRUE)),
    signing_dates    = paste(sort(unique(na.omit(as.character(date_signed)))), collapse = "; "),
    models           = paste(sort(unique(support_type)), collapse = " -> "),
    moa_files        = paste(sort(unique(na.omit(moa_file))), collapse = "; "),
    .groups = "drop"
  ) |>
  mutate(terminated = active_to < final_asof)

kost_agg <- kost |> mutate(agency_key = norm_agency(agency)) |>
  group_by(agency_key) |>
  summarise(first_signed_kost = min(as.Date(date_signed)),
            kost_level = first(level), kost_jurisdiction = first(jurisdiction), .groups = "drop")

# Yale labels jurisdictions, not agencies: match by name, then by county
# containment within state; the rest stand alone as Yale-only orphans
yale_agg <- if (!is.null(yale)) {
  # bare "Florida" is the 2002 FDLE agreement, the program's first MOU
  y <- yale |>
    mutate(
      label = jurisdiction_label,
      label_clean = str_replace(jurisdiction_label,
                                regex("^L\\.A\\.", ignore_case = TRUE),
                                "Los Angeles"),
      label_clean = if_else(label_clean == "Florida",
                            "Florida Department of Law Enforcement",
                            label_clean),
      k1 = norm_agency(label_clean),
      has_comma = str_detect(label_clean, ","),
      county_part = norm_agency(str_remove(label_clean, ",[^,]*$")),
      state_part = toupper(str_remove_all(
        str_squish(str_remove(label_clean, "^.*,")), "\\."
      ))
    ) |>
    mutate(state_part = coalesce(
      setNames(toupper(state.name), state.abb)[state_part], state_part
    ))
  direct <- y |>
    filter(k1 %in% ice_agg$agency_key) |>
    transmute(agency_key = k1, yale_label = label)
  rest <- y |> filter(!k1 %in% ice_agg$agency_key)
  by_county <- tidyr::crossing(
    rest |> filter(has_comma) |>
      select(label, county_part, state_part),
    ice_agg |> transmute(agency_key, ice_state = toupper(state))
  ) |>
    # "Nashville & Davidson County": any &-separated segment may match
    mutate(tokens = str_split(county_part, "and")) |>
    rowwise() |>
    filter(ice_state == state_part,
           any(nchar(unlist(tokens)) > 3 &
                 str_detect(agency_key, fixed(unlist(tokens))))) |>
    ungroup() |>
    transmute(agency_key, yale_label = label)
  matched <- bind_rows(direct, by_county) |>
    distinct(agency_key, .keep_all = TRUE)
  orphans <- rest |>
    filter(!label %in% matched$yale_label) |>
    transmute(agency_key = k1, yale_label = label)
  bind_rows(matched, orphans) |>
    distinct(agency_key, .keep_all = TRUE) |>
    mutate(in_yale_2008 = TRUE)
} else tibble(agency_key = character(), in_yale_2008 = logical(),
              yale_label = character())

# ICE's internal agency names differ from the website's; undo the systematic
# forms, name the rest explicitly, and let anything unmatched stand alone
rem_to_roster <- c(
  "TN Department of Safety"                        = "Tennessee Highway Patrol / Department of Safety",
  "Prince William-Manassas Adult Detention Center" = "Prince William-Manassas Regional Adult Detention Center",
  "Lexington County Sheriff Department"             = "Lexington County Sheriff's Office"
)
rem_agency_clean <- function(agency) {
  # aliases first, or the rules below make a listed key unreachable
  out <- ifelse(agency %in% names(rem_to_roster),
                unname(rem_to_roster[agency]), agency)
  st_up <- setNames(toupper(state.name), state.abb)
  lead <- str_match(out, "^([A-Z]{2})\\s")[, 2]
  out <- ifelse(!is.na(st_up[lead]),
                str_replace(out, "^[A-Z]{2}\\s",
                            paste0(str_to_title(st_up[lead]), " ")),
                out)
  str_remove(out, "\\s[A-Z]{2}$")
}

rem_agg <- if (!is.null(rem)) {
  rem |>
    filter(attests_active) |>
    mutate(agency_key = norm_agency(rem_agency_clean(agency)),
           fy_start = as.Date(fy_start), fy_end = as.Date(fy_end)) |>
    group_by(agency_key) |>
    summarise(
      rem_state       = toupper(first(state)),
      rem_agency      = first(agency),
      rem_active_from = min(fy_start),
      rem_active_to   = max(fy_end),
      rem_fy_first    = min(fiscal_year),
      rem_fy_last     = max(fiscal_year),
      rem_identified  = sum(identified, na.rm = TRUE),
      rem_removed     = sum(removed, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(in_ice_removals = TRUE)
} else {
  tibble(agency_key = character(), in_ice_removals = logical())
}

# GAO is roster-grade, so split it out of the secondary pool for the window
# and confidence logic while it keeps attesting normally
gao_agg <- if (!is.null(sec)) {
  sec |>
    filter(source == "GAO-09-109 App. III (1 Sep 2007)") |>
    transmute(agency_key = norm_agency(agency)) |>
    distinct(agency_key) |>
    mutate(in_gao_2007 = TRUE)
} else tibble(agency_key = character(), in_gao_2007 = logical())

sec_agg <- if (!is.null(sec)) {
  sec |>
    filter(!needs_review) |>
    mutate(agency_key = norm_agency(agency),
           date_signed = as.Date(date_signed)) |>
    group_by(agency_key) |>
    summarise(
      sec_state  = toupper(first(state)),
      sec_agency = first(agency),
      first_signed_sec = suppressWarnings(min(date_signed, na.rm = TRUE)),
      first_signed_sec_source =
        if (all(is.na(date_signed))) NA_character_ else
          source[which.min(date_signed)],
      n_secondary_sources = n_distinct(source),
      secondary_sources = paste(sort(unique(source)), collapse = "; "),
      .groups = "drop") |>
    mutate(first_signed_sec =
             as.Date(ifelse(is.infinite(first_signed_sec), NA,
                            first_signed_sec)))
} else tibble(agency_key = character(), sec_state = character(),
              sec_agency = character(), first_signed_sec = as.Date(character()),
              first_signed_sec_source = character(),
              n_secondary_sources = integer(), secondary_sources = character())

# merge
best <- ice_agg |>
  full_join(kost_agg, by = "agency_key") |>
  full_join(sec_agg, by = "agency_key") |>
  full_join(yale_agg, by = "agency_key") |>
  full_join(rem_agg, by = "agency_key") |>
  left_join(gao_agg, by = "agency_key") |>
  left_join(cw |> filter(jurisdiction_level != "statewide") |>
              mutate(agency_key = norm_agency(ice_agency_name)) |>
              group_by(agency_key) |>
              summarise(county_fips = paste(county_fips, collapse = "; "),
                        county_name = paste(county_name, collapse = "; "), .groups = "drop"),
            by = "agency_key") |>
  left_join(cw |> mutate(agency_key = norm_agency(ice_agency_name)) |>
              distinct(agency_key, jurisdiction_level), by = "agency_key") |>
  mutate(
    state  = coalesce(state, toupper(kost_jurisdiction), sec_state, rem_state),
    agency = coalesce(agency, kost_jurisdiction, sec_agency, rem_agency, yale_label),
    in_gao_2007 = coalesce(in_gao_2007, FALSE),
    in_yale_2008 = coalesce(in_yale_2008, FALSE),
    in_ice_removals = coalesce(in_ice_removals, FALSE),
    in_ice_roster = !is.na(n_rosters),
    roster_grade = in_ice_roster | in_gao_2007 | in_yale_2008 | in_ice_removals,
    active_from = pmin(
      active_from,
      if_else(in_gao_2007, as.Date("2007-09-01"), as.Date(NA)),
      if_else(in_yale_2008, as.Date("2008-01-17"), as.Date(NA)),
      rem_active_from,
      na.rm = TRUE
    ),
    active_to = pmax(
      active_to,
      if_else(in_gao_2007 & !in_ice_roster, as.Date("2007-09-01"),
              as.Date(NA)),
      if_else(in_yale_2008 & !in_ice_roster, as.Date("2008-01-17"),
              as.Date(NA)),
      rem_active_to,
      na.rm = TRUE
    ),
    terminated = coalesce(terminated, roster_grade & !in_ice_roster),
    in_kostandini = !is.na(first_signed_kost),
    in_secondary  = coalesce(n_secondary_sources, 0L) > 0,
    n_secondary_sources = coalesce(n_secondary_sources, 0L),
    first_signed_best = pmin(first_signed_ice, first_signed_kost,
                             first_signed_sec, na.rm = TRUE),
    first_signed_source = case_when(
      !is.na(first_signed_sec) & first_signed_best == first_signed_sec ~
        first_signed_sec_source,
      !is.na(first_signed_kost) & first_signed_best == first_signed_kost ~
        "Kostandini Table A1",
      !is.na(first_signed_ice) ~ "ICE roster (earliest observed)",
      TRUE ~ NA_character_),
    n_sources = in_ice_roster + in_kostandini + in_yale_2008 +
      in_ice_removals + n_secondary_sources,
    confidence = case_when(
      roster_grade & n_sources >= 2 ~ "high",
      roster_grade | n_sources >= 2 ~ "medium",
      TRUE ~ "low"),
    # 18 of the 2017-era MOAs were linked but never archived: Wayback has only 404s
    moa_pdf_present = sapply(str_split(coalesce(moa_files, ""), "; "), function(v) {
      v <- v[nzchar(v)]
      if (!length(v)) return(NA)
      any(file.exists(file.path(ROOT, "agreements", "agreements_wayback_pre2018", v)))
    }),
    moa_archived_url = ifelse(moa_files == "" | is.na(moa_files), NA_character_,
      sapply(str_split(moa_files, "; "), function(v)
        paste(sprintf("https://web.archive.org/web/2017id_/http://www.ice.gov/doclib/foia/memorandumsofAgreementUnderstanding/%s", v),
              collapse = "; ")))
  ) |>
  select(state, agency, jurisdiction_level, county_fips, county_name,
         first_signed_best, first_signed_source, latest_signed, signing_dates,
         models, active_from, active_to, terminated, n_rosters,
         in_ice_roster, in_gao_2007, in_kostandini, in_yale_2008,
         in_ice_removals, rem_fy_first, rem_fy_last, rem_identified,
         rem_removed, in_secondary,
         n_secondary_sources, secondary_sources, n_sources, confidence,
         moa_files, moa_pdf_present, moa_archived_url) |>
  arrange(state, agency)

write.csv(best, file.path(OUT, "pre2018-agreements-best-guess.csv"), row.names = FALSE, na = "")

# attestation long form
att <- bind_rows(
  ice  |> transmute(agency_key, state, agency, source = "ICE fact-sheet roster",
                    observed_on = as.character(content_asof),
                    detail = paste0(support_type, "; signed ", date_signed)),
  kost |> transmute(agency_key = norm_agency(agency), state = toupper(jurisdiction), agency,
                    source = "Kostandini et al. 2014 Table A1",
                    observed_on = "c.2010-11 ICE roster", detail = paste("signed", date_signed)),
  if (!is.null(yale)) yale |> transmute(agency_key = norm_agency(jurisdiction_label),
                    state = NA_character_, agency = jurisdiction_label,
                    source = "Yale WIRAC FOIA", observed_on = "2008-01-17",
                    detail = "in ICE production") else NULL,
  if (!is.null(sec)) sec |> filter(!needs_review) |>
    transmute(agency_key = norm_agency(agency), state = toupper(state), agency,
              source, observed_on = as.character(as_of),
              detail = paste0(coalesce(model, ""), "; signed ",
                              coalesce(as.character(date_signed), "NA"))) else NULL,
  if (!is.null(rem)) rem |> filter(attests_active) |>
    transmute(agency_key = norm_agency(rem_agency_clean(agency)),
              state = toupper(state), agency,
              source = "ICE identification/removal statistics (FOIA)",
              observed_on = paste0("FY", fiscal_year),
              detail = paste0(coalesce(identified, 0), " identified; ",
                              coalesce(removed, 0), " removed")) else NULL
)
write.csv(att, file.path(OUT, "pre2018-source-attestation.csv"), row.names = FALSE, na = "")

# completeness ledger
comp <- ice |> group_by(content_asof, asof_basis) |>
  summarise(n_agreements = n(), moa_links = sum(!is.na(moa_file)), .groups = "drop") |>
  transmute(as_of = as.character(content_asof), n_agreements, moa_links,
            source = "ICE fact sheet (Wayback capture)", unit = "agency",
            basis = asof_basis,
            claim = "complete census of agreements ACTIVE on this date")
extra <- tibble(
  as_of = c("2007-09-01", "2008-01-17", "2009-02-01", "2009-06-01", "2010-08-01"),
  n_agreements = c(29L, 34L, 67L, 66L, 72L), moa_links = NA_integer_,
  source = c("GAO-09-109 App. III", "Yale WIRAC FOIA", "CRS RL32270 App. A",
             "DHS OIG-10-63 App. E", "MPI Delegation and Divergence App. 2"),
  unit = "agency",
  basis = c("stated as-of", "ICE production date", "stated as-of", "stated as-of", "stated as-of"),
  claim = "complete census of agreements ACTIVE on this date")
write.csv(bind_rows(extra, comp) |> arrange(as_of),
          file.path(OUT, "pre2018-completeness-by-period.csv"), row.names = FALSE, na = "")

# East vs ICE audit
if (!is.null(east)) {
  ice_cty <- ice |>
    left_join(cw |> mutate(agency_key = norm_agency(ice_agency_name)),
              by = "agency_key", relationship = "many-to-many") |>
    filter(jurisdiction_level == "local", !is.na(county_fips)) |>
    distinct(content_asof, county_fips)
  e <- east |> mutate(s = as.Date(paste0(start_ym, "-01")),
                      e_ = as.Date(paste0(end_ym, "-01")))
  # bound the audit by East's coverage, else every later roster reads as missing
  panel_end <- max(e$e_)
  dates <- sort(unique(ice_cty$content_asof))
  skipped <- dates[dates > panel_end]
  if (length(skipped))
    cat(sprintf("Audit skips %d ICE rosters after East's panel ends (%s): %s\n",
                length(skipped), panel_end, paste(skipped, collapse = ", ")))
  dates <- dates[dates <= panel_end]
  audit <- lapply(dates, function(d) {
    a <- ice_cty$county_fips[ice_cty$content_asof == d]
    b <- e$fips[e$s <= d & e$e_ >= d]
    bind_rows(
      tibble(as_of = as.character(d), county_fips = setdiff(a, b),
             discrepancy = "in ICE roster, missing from East"),
      tibble(as_of = as.character(d), county_fips = setdiff(b, a),
             discrepancy = "in East, absent from ICE roster"))
  }) |> bind_rows() |>
    left_join(cw |> distinct(county_fips, county_name), by = "county_fips")
  write.csv(audit, file.path(OUT, "pre2018-east-vs-ice-audit.csv"), row.names = FALSE, na = "")
  cat(sprintf("East-vs-ICE audit: %d discrepancies across %d dates\n",
              nrow(audit), n_distinct(audit$as_of)))
}

# report
cat(sprintf("\nagreements_best_guess.csv: %d agreements\n", nrow(best)))
print(best |> count(confidence))
cat(sprintf("\nterminated before last roster: %d\n", sum(best$terminated, na.rm = TRUE)))
cat(sprintf("roster linked an MOA:          %d\n", sum(!is.na(best$moa_archived_url))))
cat(sprintf("MOA PDF present on disk:       %d\n", sum(best$moa_pdf_present, na.rm = TRUE)))
nodoc <- best |> filter(!is.na(moa_files), nzchar(moa_files), moa_pdf_present == FALSE)
if (nrow(nodoc)) {
  cat(sprintf("\nLinked but NEVER ARCHIVED (%d) -- Wayback has only 404 captures:\n", nrow(nodoc)))
  print(nodoc |> select(state, agency, first_signed_best, moa_files), n = 30)
}
cat(sprintf("first signing observed:        %s\n", min(best$first_signed_best, na.rm = TRUE)))
cat(sprintf("earliest attested activity:    %s\n", min(best$active_from, na.rm = TRUE)))

if (!is.null(rem)) {
  cat(sprintf("\nICE removal statistics: attest %d of the %d partnerships\n",
              sum(best$in_ice_removals), nrow(best)))
  onlyrem <- best |> filter(in_ice_removals, !in_ice_roster)
  if (nrow(onlyrem)) {
    cat(sprintf("attested by ICE's own statistics but on NO roster we hold (%d):\n",
                nrow(onlyrem)))
    print(onlyrem |> select(state, agency, rem_fy_first, rem_fy_last,
                            rem_identified, rem_removed, confidence))
  }
  pushed <- best |> filter(in_ice_removals, active_from < as.Date("2007-09-01"))
  if (nrow(pushed)) {
    cat(sprintf("\nactive window pushed before GAO's Sep-2007 census (%d):\n", nrow(pushed)))
    print(pushed |> select(state, agency, active_from, rem_fy_first, rem_identified))
  }
}
cat("\nIn Kostandini but never in an ICE roster (check these by hand):\n")
print(best |> filter(in_kostandini, !in_ice_roster) |> select(state, agency, first_signed_best))
cat("\nOn ICE's Oct-2010 roster but ABSENT from Kostandini Table A1:\n")
print(best |> filter(in_ice_roster, !in_kostandini, active_from <= as.Date("2010-10-29")) |>
        select(state, agency, first_signed_best))
