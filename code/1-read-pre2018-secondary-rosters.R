# Roster appendices from the secondary reports, so the reconstruction can be
# checked against sources not downstream of ICE's website
# -> data/pre2018-secondary-rosters-long.csv
# Sources parsed, and the two deliberately skipped: inputs/pre2018/SOURCES.csv

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(purrr) })

# the source pdfs never change, so keep the committed output if neither
# pdftotext nor pdftools is available
if (Sys.which("pdftotext") == "" &&
      !requireNamespace("pdftools", quietly = TRUE)) {
  message(
    "neither pdftotext nor pdftools available; ",
    "keeping committed data/pre2018-secondary-rosters-long.csv"
  )
  quit(save = "no", status = 0)
}

ROOT <- "."  # run from the repo root
REP  <- file.path(ROOT, "inputs", "pre2018", "reports")
OUT  <- file.path(ROOT, "data")

# only pdftotext -layout preserves the column spacing these tables depend on
pdf_lines <- function(path) {
  if (nzchar(Sys.which("pdftotext"))) {
    tf <- tempfile(fileext = ".txt")
    system2("pdftotext", c("-layout", shQuote(path), shQuote(tf)), stdout = FALSE, stderr = FALSE)
    if (file.exists(tf)) return(readLines(tf, warn = FALSE, encoding = "UTF-8"))
  }
  warning("pdftotext not found; falling back to pdf_text(), column alignment may be lost")
  unlist(str_split(pdftools::pdf_text(path), "\n"))
}

# do NOT str_squish: runs of 2+ spaces are the column delimiters
clean <- function(x) x |> str_replace_all('[\u2018\u2019]', "'") |>
  str_replace_all('[\u201c\u201d]', '\"') |>
  str_replace_all('\t', '    ') |>
  str_replace('\\s+$', '') |> str_replace('^\\s+', '')

norm_model <- function(x) {
  x <- toupper(x)
  case_when(
    str_detect(x, "JAIL.*TASK|TASK.*JAIL|JEO AND TFO|DETENTION/TASK|JOINT|HYBRID") ~ "Hybrid",
    str_detect(x, "JAIL|DETENTION|JEO")  ~ "Jail Enforcement",
    str_detect(x, "TASK|TFO")            ~ "Task Force",
    TRUE ~ NA_character_)
}

ST <- c(state.name, "District of Columbia")
ABB <- setNames(state.name, state.abb)

# CRS RL32270 App. A; the model can wrap ("Detention/Task" then "Force")
parse_crs <- function() {
  f <- file.path(REP, "CRS-RL32270_2009-03-11.pdf"); if (!file.exists(f)) return(NULL)
  L <- clean(pdf_lines(f))
  m <- str_match(L, "^([A-Z]{2})\\s+(.+?)\\s{2,}(Detention/Task|Detention|Task Force|Detention\\s*/\\s*Task)\\s*(Force)?\\s+(\\d{1,2}/\\d{1,2}/\\d{4})$")
  keep <- !is.na(m[, 1])
  if (!any(keep)) return(NULL)
  tibble(
    source = "CRS RL32270 App. A (11 Mar 2009)", as_of = as.Date("2009-02-01"),
    state  = unname(ABB[m[keep, 2]]),
    agency = m[keep, 3],
    model  = norm_model(paste(m[keep, 4], coalesce(m[keep, 5], ""))),
    date_signed = as.Date(m[keep, 6], "%m/%d/%Y"))
}

# MPI Delegation and Divergence App. 2
parse_mpi_dd <- function() {
  f <- file.path(REP, "MPI_Delegation-and-Divergence_Jan2011.pdf"); if (!file.exists(f)) return(NULL)
  L <- clean(pdf_lines(f))
  pat <- sprintf("^(%s)\\s{2,}(.+?)\\s{2,}(Jail & Task Force|Jail Enforcement|Task Force)\\s+(\\d{2}/\\d{2}/\\d{4})$",
                 paste(ST, collapse = "|"))
  m <- str_match(L, pat); keep <- !is.na(m[, 1]); if (!any(keep)) return(NULL)
  tibble(source = "MPI Delegation and Divergence App. 2 (Aug 2010)", as_of = as.Date("2010-08-01"),
         state = m[keep, 2], agency = m[keep, 3], model = norm_model(m[keep, 4]),
         date_signed = as.Date(m[keep, 5], "%m/%d/%Y"))
}

# MPI Program in Flux and NCLR: state printed once per block, so carry it forward
parse_blocked <- function(file, src, asof, model_alt = "") {
  f <- file.path(REP, file); if (!file.exists(f)) return(NULL)
  L <- clean(pdf_lines(f))
  models <- paste(c("Jail Enforcement Officers \\(JEO\\)", "Task Force Officers \\(TFO\\)",
                    "JEO and TFO", "Jail & Task Force", "Task Force & Jail",
                    "Jail Enforcement", "Task Force", "JEO", "TFO", model_alt), collapse = "|")
  states_pat <- paste(c(toupper(ST), ST), collapse = "|")

  # these reports wrap agency names, so the dated line may hold only the tail
  is_data <- str_detect(L, sprintf("(%s)\\s+\\d{1,2}/\\d{1,2}/\\d{4}\\s*$", models))
  head_ok <- str_detect(L, "[A-Za-z]{3}") &
             !str_detect(L, "\\d{1,2}/\\d{1,2}/\\d{4}") &
             !str_detect(L, regex(models, ignore_case = TRUE)) &
             !str_detect(L, regex("^\\s*(appendix|table|source|note|state\\b)", ignore_case = TRUE))
  joined <- L
  for (k in which(is_data)) {
    if (k > 1 && head_ok[k - 1]) {
      prev <- str_squish(L[k - 1])
      if (!str_detect(prev, sprintf("^(%s)$", states_pat)) && nchar(prev) >= 3)
        joined[k] <- paste(prev, str_squish(L[k]))
    }
  }

  pat <- sprintf("^(?:(%s)\\s+)?(.+?)\\s{2,}(%s)\\s+(\\d{1,2}/\\d{1,2}/\\d{4})$", states_pat, models)
  pat_j <- sprintf("^(?:(%s)\\s+)?(.+?)\\s+(%s)\\s+(\\d{1,2}/\\d{1,2}/\\d{4})$", states_pat, models)
  cur <- NA_character_; rows <- list()
  for (idx in seq_along(joined)) {
    ln <- joined[idx]
    m <- str_match(ln, pat)
    if (is.na(m[1])) m <- str_match(ln, pat_j)
    if (is.na(m[1])) next
    st <- m[2]
    if (!is.na(st) && nzchar(st)) cur <- str_to_title(st)
    ag <- str_squish(m[3])
    ag <- str_replace(ag, sprintf("^(%s)\\s+", states_pat), "")
    rows[[length(rows) + 1]] <- tibble(state = cur, agency = ag,
                                       model = norm_model(m[4]),
                                       date_signed = as.Date(m[5], "%m/%d/%Y"))
  }
  if (!length(rows)) return(NULL)
  bind_rows(rows) |> distinct(agency, date_signed, .keep_all = TRUE) |>
    mutate(source = src, as_of = asof, .before = 1)
}

# GAO-09-109 App. III: names only, no dates
parse_gao <- function() {
  f <- file.path(REP, "GAO-09-109_Jan2009.pdf"); if (!file.exists(f)) return(NULL)
  L <- clean(pdf_lines(f))
  # the phrase also appears in the narrative, so take the LAST occurrence
  hits <- grep("agreements with ICE as of September 1, 2007", L)
  if (!length(hits)) return(NULL)
  i <- tail(hits, 1)
  blk <- L[(i + 1):min(i + 80, length(L))]
  # stop before the site-visits paragraph, which names agencies again in prose
  stop_at <- grep("^We also conducted", blk)[1]
  if (!is.na(stop_at)) blk <- blk[seq_len(stop_at - 1)]
  # the print punctuates line ends inconsistently, so accept all four shapes
  nm <- blk |> str_subset("^[A-Z].{8,90}(;\\s*and|[;.])?$") |>
    str_replace(";\\s*and$", "") |> str_replace("[;.]$", "") |>
    str_replace("\\s+$", "") |>
    str_subset("Department|Sheriff|Police|Correction|Patrol|Center|Safety|Bureau") |>
    str_subset("^(?!Page |Appendix )", negate = FALSE)
  if (!length(nm)) return(NULL)
  tibble(source = "GAO-09-109 App. III (1 Sep 2007)", as_of = as.Date("2007-09-01"),
         state = NA_character_, agency = unique(nm), model = NA_character_,
         date_signed = as.Date(NA))
}

# Cato WP 52 Table A1, transposed: counties across, dates in a row
parse_cato <- function() {
  f <- file.path(REP, "Cato-WP52_Forrester-Nowrasteh_2018.pdf"); if (!file.exists(f)) return(NULL)
  L <- clean(pdf_lines(f))
  hi <- grep("^Year\\s+Alamance", L)[1]; di <- grep("^MOA Signed", L)[1]
  if (is.na(hi) || is.na(di)) return(NULL)
  cty <- str_split(L[hi], "\\s{2,}")[[1]]; cty <- cty[cty != "Year" & cty != "Total"]
  dts <- str_extract_all(L[di], "\\d{1,2}/\\d{1,2}/\\d{4}")[[1]]
  n <- min(length(cty), length(dts)); if (!n) return(NULL)
  tibble(source = "Cato WP 52 Table A1 (2018)", as_of = as.Date(NA),
         state = "North Carolina", agency = paste(cty[1:n], "County Sheriff's Office"),
         model = NA_character_, date_signed = as.Date(dts[1:n], "%m/%d/%Y"))
}

# McCann et al. 2024 appendix: Springer glues zero-width spaces into the urls
# and the Signed column uses an en-dash
parse_mccann <- function() {
  f <- file.path(REP, "McCann-Boateng-Schimchak_JIMI_2024.pdf"); if (!file.exists(f)) return(NULL)
  L <- pdf_lines(f)
  i <- grep("MOAs, reporting with a link", L)[1]; if (is.na(i)) return(NULL)
  blk <- L[i:length(L)]
  zap <- function(x) str_replace_all(x, "\u200b|\u00ad|\u2060", "")   # zero-width / soft hyphen
  blk <- zap(blk)
  pat <- sprintf("^\\s*(%s)\\s{2,}(.+?)\\s{2,}(https?://\\S*)?\\s*(\\d{4}[-\u2013]\\d{2}-\\d{2})",
                 paste(ST, collapse = "|"))
  m <- str_match(blk, pat); keep <- !is.na(m[, 1]); if (!any(keep)) return(NULL)
  tibble(source = "McCann et al. 2024 Appendix", as_of = as.Date(NA),
         state = m[keep, 2], agency = str_squish(m[keep, 3]),
         model = NA_character_,
         date_signed = as.Date(str_replace(m[keep, 5], "\u2013", "-"), "%Y-%m-%d"),
         needs_review = FALSE)
}

# MyAttorneyUSA mirror: the only independent read on the 2017-08-01 roster
parse_myattorneyusa <- function() {
  f <- file.path(ROOT, "inputs", "pre2018", "gapfill",
                 "MyAttorneyUSA_roster_2017-08-01.tsv")
  if (!file.exists(f)) return(NULL)
  L <- readLines(f, warn = FALSE, encoding = "UTF-8")
  m <- str_match(L, "^([^\t]+)\t([^\t]+)\t(\\d{4}-\\d{2}-\\d{2})\\s*$")
  keep <- !is.na(m[, 1]); if (!any(keep)) return(NULL)
  tibble(source = "MyAttorneyUSA roster mirror (1 Aug 2017)",
         as_of = as.Date("2017-08-01"),
         state = str_to_title(str_squish(m[keep, 2])),
         agency = str_squish(m[keep, 3]),
         model = NA_character_,
         date_signed = as.Date(m[keep, 4]),
         needs_review = FALSE)
}

res <- bind_rows(
  parse_crs(), parse_mpi_dd(),
  parse_blocked("MPI_Program-in-Flux_Mar2010.pdf", "MPI A Program in Flux App. 1 (Jan 2010)", as.Date("2010-01-01")),
  parse_blocked("NCLR_Lacayo_2010.pdf", "NCLR/Lacayo App. A (2 Aug 2010)", as.Date("2010-08-02")),
  parse_gao(), parse_cato(), parse_mccann(), parse_myattorneyusa()
) |> filter(!is.na(agency), nzchar(agency)) |>
  # keep layout-damaged rows (they carry real dates) but flag them
  mutate(needs_review = str_detect(agency, "^(CAROLINA|SOUTH|NORTH|Department|Corrections|Enforcement|County|State Police|Highway Patrol)\\b") |
                        str_count(agency, "\\S+") < 3 |
                        str_detect(agency, "<U\\+|\\u2020")) |>
  mutate(repaired = FALSE) |>
  select(source, as_of, state, agency, model, date_signed, needs_review, repaired) |>
  arrange(source, state, agency)

# Repair flagged rows by their intact, near-unique signing date, accepting only
# an unambiguous name that ends with the fragment. Repairs `state` too, so it
# must run before anything downstream reads it.
clean_ref <- bind_rows(
  res |> filter(!needs_review, !is.na(date_signed)) |> select(state, agency, date_signed),
  {
    f <- file.path(OUT, "pre2018-roster-long.csv")
    if (file.exists(f)) {
      read.csv(f, stringsAsFactors = FALSE) |>
        transmute(state = str_to_title(state), agency,
                  date_signed = as.Date(date_signed)) |>
        filter(!is.na(date_signed))
    } else NULL
  }
) |> distinct(state, agency, date_signed)

key <- function(x) str_replace_all(tolower(x), "[^a-z0-9]", "")
repair_one <- function(ag, dt) {
  if (is.na(dt)) return(NULL)
  cand <- clean_ref |> filter(date_signed == dt)
  if (!nrow(cand)) return(NULL)
  frag <- key(str_replace(
    ag, "^(NORTH|SOUTH|WEST|EAST|CAROLINA|DAKOTA|VIRGINIA|JERSEY|MEXICO|HAMPSHIRE|ISLAND)\\s+", ""))
  frag <- str_replace_all(frag, "\\*+$", "")
  if (!nzchar(frag)) return(NULL)
  hit <- cand |> filter(str_detect(key(agency), paste0(frag, "$")))
  if (!nrow(hit)) return(NULL)
  # candidates may differ only by a leading state word; take the fullest spelling
  core <- key(str_replace(hit$agency,
                          sprintf("^(%s|[A-Z]{2})\\s+", paste(ST, collapse = "|")), ""))
  if (length(unique(core)) != 1) return(NULL)
  hit[which.max(nchar(hit$agency)), ][1, ]
}
n_fixed <- 0
for (i in which(res$needs_review)) {
  h <- repair_one(res$agency[i], res$date_signed[i])
  if (!is.null(h)) {
    res$state[i] <- h$state; res$agency[i] <- h$agency
    res$needs_review[i] <- FALSE; res$repaired[i] <- TRUE; n_fixed <- n_fixed + 1
  }
}
cat(sprintf("repaired %d flagged rows by signing-date match; %d still flagged\n",
            n_fixed, sum(res$needs_review)))

write.csv(res, file.path(OUT, "pre2018-secondary-rosters-long.csv"),
          row.names = FALSE, na = "")
cat(sprintf("secondary_rosters_long.csv: %d rows\n\n", nrow(res)))
print(res |> count(source, name = "rows"))
cat("\nrows with a signing date: ", sum(!is.na(res$date_signed)), "\n\n", sep = "")

# self-check: a shortfall means the parser missed rows, not that a source disagrees
expected <- c("CRS RL32270 App. A (11 Mar 2009)" = 67,
              "GAO-09-109 App. III (1 Sep 2007)" = 29,
              "MPI Delegation and Divergence App. 2 (Aug 2010)" = 72,
              "MPI A Program in Flux App. 1 (Jan 2010)" = 71,
              "NCLR/Lacayo App. A (2 Aug 2010)" = 71,
              "Cato WP 52 Table A1 (2018)" = 9,
              "MyAttorneyUSA roster mirror (1 Aug 2017)" = 60,
              "McCann et al. 2024 Appendix" = NA)
chk <- res |> count(source, name = "parsed") |>
  mutate(stated = unname(expected[source]), gap = stated - parsed,
         status = case_when(
           is.na(stated)      ~ "no stated total (see note)",
           gap == 0           ~ "exact",
           TRUE               ~ paste0("short by ", gap)))
print(chk)
cat("\nNote: McCann et al. print no total. The appendix has ~150 URL-bearing lines,\n",
    "of which some are URL continuations rather than entries; the parser recovers\n",
    "the rows where state, agency and signing date all sit on one line.\n", sep = "")
cat(sprintf("\nrows flagged needs_review: %d of %d (%s)\n", sum(res$needs_review), nrow(res),
            paste(unique(res$source[res$needs_review]), collapse = "; ")))
