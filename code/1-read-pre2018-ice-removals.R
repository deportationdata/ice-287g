# ICE's per-agency 287(g) identification/removal counts FY2006-2013 (roster-grade
# activity evidence, no signing dates) -> data/pre2018-ice-removals-long.csv

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(readr)
})

ROOT <- "."  # run from the repo root
SRC  <- file.path(ROOT, "inputs", "pre2018", "gapfill", "ICE287g-removals",
                  "287g_removals-2006_2013_ICE.csv")
OUT  <- file.path(ROOT, "data"); dir.create(OUT, showWarnings = FALSE)
if (!file.exists(SRC)) stop("Missing ", SRC)

raw <- read_csv(SRC, show_col_types = FALSE, progress = FALSE)

state_lookup <- setNames(toupper(state.name), state.abb)
state_names_up <- toupper(state.name)

# hand-compiled addresses are inconsistent, so fall through looser patterns
state_from_address <- function(addr) {
  addr <- coalesce(addr, "")
  out <- rep(NA_character_, length(addr))
  m <- str_match(addr, ",\\s*([A-Z]{2})[\\s_]+\\d{5}")[, 2]
  out <- coalesce(out, state_lookup[m])
  m <- toupper(str_match(addr, ",?\\s*([A-Za-z]+(?: [A-Za-z]+)?)\\s+\\d{5}")[, 2])
  out <- coalesce(out, if_else(m %in% state_names_up, m, NA_character_))
  m <- str_match(addr, "\\b([A-Z]{2})\\b(?=[\\s,]|$)")[, 2]
  out <- coalesce(out, state_lookup[m])
  for (i in which(is.na(out))) {
    hit <- state_names_up[str_detect(toupper(addr[i]), fixed(state_names_up))]
    if (length(hit) == 1) out[i] <- hit
  }
  out
}

# ICE names state agencies "CO Department of ..." and disambiguates shared
# county names with a trailing "... AR"
state_from_agency <- function(agency) {
  lead  <- str_match(agency, "^([A-Z]{2})\\s")[, 2]
  trail <- str_match(agency, "\\s([A-Z]{2})$")[, 2]
  coalesce(state_lookup[lead], state_lookup[trail])
}

removals <- raw |>
  transmute(
    agency_raw = str_squish(Jurisdiction_orig),
    # the source's asterisk is undocumented; keep the fact, drop it from the name
    source_footnote = str_detect(agency_raw, "\\*"),
    agency = str_squish(str_remove_all(agency_raw, "\\*")),
    agency_short = str_squish(Jurisdiction),
    address = str_squish(address),
    latitude  = suppressWarnings(as.numeric(LAT)),
    longitude = suppressWarnings(as.numeric(LON)),
    state = coalesce(state_from_address(address), state_from_agency(agency))
  )

# last resort: unique name match against ICE's own roster, not a hardcoded list
roster_path <- file.path(OUT, "pre2018-roster-long.csv")
if (anyNA(removals$state) && file.exists(roster_path)) {
  norm <- function(x) str_replace_all(str_to_lower(x), "[^a-z0-9]", "")
  roster_state <- read_csv(roster_path, show_col_types = FALSE, progress = FALSE) |>
    transmute(k = norm(agency), state_roster = toupper(state)) |>
    distinct() |>
    group_by(k) |>
    filter(n() == 1) |>
    ungroup()
  removals <- removals |>
    mutate(k = norm(agency)) |>
    left_join(roster_state, by = "k") |>
    mutate(state = coalesce(state, state_roster)) |>
    select(-k, -state_roster)
}

counts <- raw |>
  transmute(agency_raw = str_squish(Jurisdiction_orig),
            across(matches("^\\d{4}_(identified|removed)$"))) |>
  pivot_longer(-agency_raw, names_to = c("fiscal_year", "measure"),
               names_pattern = "^(\\d{4})_(identified|removed)$",
               values_to = "n") |>
  mutate(fiscal_year = as.integer(fiscal_year),
         n = suppressWarnings(as.numeric(n))) |>
  pivot_wider(names_from = measure, values_from = n)

long <- removals |>
  left_join(counts, by = "agency_raw") |>
  mutate(
    # federal FY2006 = 2005-10-01 .. 2006-09-30
    fy_start = as.Date(sprintf("%d-10-01", fiscal_year - 1)),
    fy_end   = as.Date(sprintf("%d-09-30", fiscal_year)),
    # a recorded zero is an explicit "no activity", so it attests nothing
    attests_active = coalesce(identified, 0) > 0 | coalesce(removed, 0) > 0,
    source = "ICE 287(g) identification/removal statistics FY2006-2013 (FOIA)"
  ) |>
  select(state, agency, agency_short, fiscal_year, fy_start, fy_end,
         identified, removed, attests_active, source_footnote,
         latitude, longitude, address, agency_raw, source) |>
  arrange(state, agency, fiscal_year)

write.csv(long, file.path(OUT, "pre2018-ice-removals-long.csv"),
          row.names = FALSE, na = "")

cat(sprintf("ICE removals statistics: %d agencies x %d fiscal years = %d rows\n",
            n_distinct(long$agency), n_distinct(long$fiscal_year), nrow(long)))
cat(sprintf("fiscal years: %s\n",
            paste(range(long$fiscal_year), collapse = "-")))
cat(sprintf("agency-years attesting activity: %d\n", sum(long$attests_active)))
unresolved <- long |> filter(is.na(state)) |> distinct(agency)
if (nrow(unresolved)) {
  cat("WARNING: state unresolved for", nrow(unresolved), "agencies:\n")
  print(unresolved$agency)
} else {
  cat("state resolved for every agency\n")
}
cat("\nearliest attested activity per fiscal year:\n")
print(long |> filter(attests_active) |> count(fiscal_year, name = "agencies"))
