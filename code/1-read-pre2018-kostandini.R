# Extract Appendix Table A1 from the Kostandini et al. (2014) scan -> data/pre2018-kostandini-tableA1.csv.

# pdftools needs poppler (brew install poppler); the source pdf never changes, so keep the committed output.
if (!requireNamespace("pdftools", quietly = TRUE)) {
  message("pdftools not installed; keeping committed data/pre2018-kostandini-tableA1.csv")
  quit(save = "no", status = 0)
}
suppressPackageStartupMessages({ library(pdftools); library(dplyr); library(stringr) })

ROOT <- "."  # run from the repo root
PDF  <- file.path(ROOT, "inputs", "pre2018", "reports", "Kostandini-et-al_AJAE_2014.pdf")
OUT  <- file.path(ROOT, "data"); dir.create(OUT, showWarnings = FALSE)
if (!file.exists(PDF)) stop("Missing ", PDF, " -- run code/0-287g-pre2018-download.R")

lines <- unlist(str_split(pdf_text(PDF), "\n"))

# OCR damage in the JSTOR text layer: l read for 1 ("Table Al", "l-Feb-08"), "Thlsa"/"Tiilsa" for Tulsa.
start <- grep("Table A[l1]\\.\\s*287\\(g\\) Contracts Signed", lines)[1]
stop_ <- grep("^\\s*Source:\\s*http://www\\.ice\\.gov", lines)
stop_ <- stop_[stop_ > start][1]
if (is.na(start) || is.na(stop_)) stop("Could not locate Table A1 boundaries.")
tab <- lines[start:stop_]

fix_ocr <- function(x) {
  x |>
    str_replace_all("\\bThlsa\\b|\\bTiilsa\\b|\\bTulsa\\b", "Tulsa") |>
    str_replace_all("Sheriffs Office", "Sheriff's Office") |>
    str_replace_all("Sheriff Office", "Sheriff's Office") |>
    str_replace_all("Maricopa county", "Maricopa County Sheriff's Office") |>
    str_squish()
}
fix_date <- function(d) {
  d <- str_replace_all(d, "^l-",  "1-")
  d <- str_replace_all(d, "^ll-", "11-")
  d <- str_replace_all(d, "^Il-", "11-")
  # month case is inconsistent in the scan ("19-NOV-05")
  parts <- str_match(d, "^(\\d{1,2})-([A-Za-z]{3})-(\\d{2})$")
  ifelse(is.na(parts[, 1]), NA_character_,
         sprintf("%s-%s-%s", parts[, 2],
                 paste0(toupper(substr(parts[, 3], 1, 1)), tolower(substr(parts[, 3], 2, 3))),
                 parts[, 4]))
}

STATES <- c(state.name, "District of Columbia")

pat <- sprintf("^\\s*(%s)\\s{2,}(.+?)\\s{2,}([lI0-9]{1,2}-[A-Za-z]{3}-\\d{2})\\s*$",
               paste(STATES, collapse = "|"))

level <- NA_character_; recs <- list()
for (ln in tab) {
  if (str_detect(ln, regex("State-level enforcement", ignore_case = TRUE)))  { level <- "state";  next }
  if (str_detect(ln, regex("County-level enforcement", ignore_case = TRUE))) { level <- "county"; next }
  m <- str_match(ln, pat)
  if (is.na(m[1])) next
  recs[[length(recs) + 1]] <- tibble(level = level, jurisdiction = m[2],
                                    agency = fix_ocr(m[3]), date_raw = m[4])
}
# The scan wraps some rows: an agency line with no date, then a date-only line.
orphan_agency <- NA_character_; orphan_state <- NA_character_
for (i in seq_along(tab)) {
  ln <- tab[i]
  m2 <- str_match(ln, sprintf("^\\s*(%s)\\s{2,}(.+?)\\s*$", paste(STATES, collapse = "|")))
  if (!is.na(m2[1]) && !str_detect(ln, "\\d{1,2}-[A-Za-z]{3}-\\d{2}") &&
      !str_detect(ln, regex("enforcement|Jurisdiction", ignore_case = TRUE))) {
    orphan_state <- m2[2]; orphan_agency <- m2[3]; next
  }
  dm <- str_match(ln, "^\\s{10,}\\S*\\s*([lI0-9]{1,2}-[A-Za-z]{3}-\\d{2})\\s*$")
  if (!is.na(dm[1]) && !is.na(orphan_agency)) {
    recs[[length(recs) + 1]] <- tibble(level = "county", jurisdiction = orphan_state,
                                       agency = fix_ocr(orphan_agency), date_raw = dm[2])
    orphan_agency <- NA_character_
  }
}

kost <- bind_rows(recs) |>
  mutate(date_signed = as.Date(fix_date(date_raw), format = "%d-%b-%y")) |>
  filter(!is.na(date_signed)) |>
  distinct(jurisdiction, agency, date_signed, .keep_all = TRUE) |>   # drops the duplicated 19-Nov-05 line
  mutate(source = "Kostandini et al. 2014, Table A1") |>
  arrange(level, jurisdiction, agency)

write.csv(kost, file.path(OUT, "pre2018-kostandini-tableA1.csv"), row.names = FALSE, na = "")

cat(sprintf("Table A1: %d agreements (%d state-level, %d county/local)\n",
            nrow(kost), sum(kost$level == "state"), sum(kost$level == "county")))
cat(sprintf("Date range: %s to %s\n", min(kost$date_signed), max(kost$date_signed)))
cat("\nNOTE: commonly described in the literature as covering 2002-2009; it in fact\n")
cat("runs to 2010-08-19 (Lexington County SC). It is a point-in-time roster read\n")
cat("backwards, NOT a complete history of agreements signed in that window.\n")
