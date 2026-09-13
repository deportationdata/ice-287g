# Find MOAs ICE has posted to ice.gov but not yet linked from its sheet. ICE names each PDF
# after the agency, state, model and signing date (LoudonCoSOTN_TFM_MOA_06222026.pdf), so for
# every agreement the sheet has shown "link pending" for weeks, ask ice.gov for the few names
# that pattern allows (moa_candidate_files). A hit is kept only when the PDF's text carries
# the signing date or the agency's name. Verified PDFs land in agreements/agreements_probe_<today>/
# with a manifest row, for review in a PR; 2-make-agreements.R then links the agreement to the
# held PDF by the same names until ICE's sheet links it.
#   MOA_PROBE_MIN_DAYS      probe agreements pending at least this many days (default 21)
#   MOA_PROBE_MAX_REQUESTS  stop after this many requests (default 3000)
#   MOA_PROBE_SLEEP         seconds between requests (default 1)
#   MOA_REPORT              append a markdown summary to this file (the PR body)
#   MOA_PROBE_IDS           probe only these agreement_ids (comma-separated), whatever their status
#   MOA_PROBE_DRY_RUN       report hits without saving PDFs
suppressPackageStartupMessages({
  library(httr); library(stringr); library(purrr); library(dplyr); library(readr); library(digest)
})
source("code/functions.R")
invisible(Sys.setlocale("LC_TIME", "C"))

min_days <- as.numeric(Sys.getenv("MOA_PROBE_MIN_DAYS", "21"))
max_requests <- as.numeric(Sys.getenv("MOA_PROBE_MAX_REQUESTS", "3000"))
pause <- as.numeric(Sys.getenv("MOA_PROBE_SLEEP", "1"))
report <- Sys.getenv("MOA_REPORT", "")
out_dir <- file.path("agreements", paste0("agreements_probe_", format(Sys.Date(), "%Y%m%d")))
base_url <- "https://www.ice.gov/doclib/287gMOA/"

xwalk <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")
ids <- Sys.getenv("MOA_PROBE_IDS", "")
dry_run <- nzchar(Sys.getenv("MOA_PROBE_DRY_RUN", ""))
agreements <- arrow::read_parquet("data/intermediate/agreements.parquet") |>
  left_join(xwalk |> select(state = state_full, state_abbr), by = "state")
pending <- if (nzchar(ids)) {
  agreements |> filter(agreement_id %in% str_split(ids, ",")[[1]])
} else {
  # a pending agreement whose PDF is already held no longer reads as pending (2-make-agreements.R)
  agreements |>
    filter(status == "active", moa == "pending", first_appeared <= Sys.Date() - min_days)
}
pending <- pending |>
  mutate(model = support_abbr(norm_support_key(support_type))) |>
  filter(model %in% c("JEM", "TFM", "WSO"), !is.na(state_abbr), !is.na(signed)) |>
  arrange(first_appeared)

pdf_text_of <- function(path) {
  if (requireNamespace("pdftools", quietly = TRUE)) {
    return(tryCatch(paste(pdftools::pdf_text(path), collapse = "\n"), error = \(e) ""))
  }
  if (nzchar(Sys.which("pdftotext"))) {
    return(paste(suppressWarnings(system2("pdftotext", c("-layout", shQuote(path), "-"), stdout = TRUE, stderr = FALSE)), collapse = "\n"))
  }
  ""
}
# NA when verified, else the reason it could not be
verify <- function(txt, agency, signed) {
  flat <- str_squish(txt)
  if (!nzchar(flat)) return("the PDF has no extractable text")
  m <- as.integer(format(signed, "%m")); d <- as.integer(format(signed, "%d"))
  dates <- unique(c(format(signed, "%m/%d/%Y"), format(signed, "%m/%d/%y"), sprintf("%d/%d/%s", m, d, format(signed, "%Y")),
                    sprintf("%d/%d/%s", m, d, format(signed, "%y")), sprintf("%s %d, %s", format(signed, "%B"), d, format(signed, "%Y"))))
  generic <- c("county", "sheriff", "sheriffs", "office", "police", "department", "dept", "city", "town", "township",
               "borough", "village", "of", "the", "and", "state", "public", "safety")
  tokens <- setdiff(str_to_lower(str_split(str_squish(str_replace_all(agency, "[^A-Za-z0-9 ]", " ")), " ")[[1]]), generic)
  tokens <- tokens[nchar(tokens) >= 3]
  date_hit <- any(str_detect(flat, fixed(dates)))
  name_hit <- length(tokens) > 0 && all(str_detect(str_to_lower(flat), fixed(tokens)))
  if (date_hit || name_hit) NA_character_ else "neither the signing date nor the agency's name is in the PDF text"
}

n_requests <- 0
stop_reason <- NA_character_
found <- list()
for (i in seq_len(nrow(pending))) {
  a <- pending[i, ]
  for (f in moa_candidate_files(a$agency, a$state_abbr, a$model, a$signed)) {
    if (n_requests >= max_requests) { stop_reason <- "request cap reached"; break }
    r <- tryCatch(HEAD(paste0(base_url, f), user_agent("Mozilla/5.0"), timeout(30)), error = \(e) NULL)
    n_requests <- n_requests + 1
    Sys.sleep(pause)
    code <- if (is.null(r)) NA_integer_ else status_code(r)
    # a refusal means ICE is rate-limiting us; stop before it blocks the hourly scraper too
    if (code %in% c(403L, 429L)) { stop_reason <- paste("ICE refused a request (HTTP", code, ")"); break }
    if (identical(code, 200L)) {
      found[[length(found) + 1]] <- mutate(a, file = f, url = paste0(base_url, f), probe_etag = headers(r)[["etag"]] %||% NA_character_,
                                           probe_last_modified = headers(r)[["last-modified"]] %||% NA_character_)
      break
    }
  }
  if (!is.na(stop_reason)) break
}
probed <- if (is.na(stop_reason)) nrow(pending) else i
message(sprintf("probed %d of %d pending agreements with %d requests; %d candidate PDF(s)%s",
                probed, nrow(pending), n_requests, length(found), if (is.na(stop_reason)) "" else paste0("; stopped: ", stop_reason)))

verified <- list(); unverified <- list()
for (h in found) {
  r <- tryCatch(ice_get(h$url), error = \(e) NULL)
  Sys.sleep(pause)
  bytes <- if (!is.null(r) && status_code(r) == 200) content(r, "raw") else raw()
  if (!body_has_magic(bytes, "pdf")) { unverified[[length(unverified) + 1]] <- mutate(h, reason = "the url did not return a PDF"); next }
  tmp <- tempfile(fileext = ".pdf"); writeBin(bytes, tmp)
  reason <- verify(pdf_text_of(tmp), h$agency, h$signed)
  if (!is.na(reason)) { unverified[[length(unverified) + 1]] <- mutate(h, reason = reason); next }
  path <- NA_character_
  if (!dry_run) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    path <- make_unique_file_path(out_dir, h$file)
    file.copy(tmp, path)
  }
  v <- response_validators(r)
  verified[[length(verified) + 1]] <- mutate(h, saved_path = path, file_hash = digest(file = tmp, algo = "sha256"),
                                             etag = v[["etag"]], last_modified = v[["last_modified"]])
}
verified <- list_rbind(verified); unverified <- list_rbind(unverified)

if (nrow(verified) && !dry_run) {
  append_manifest(out_dir, verified |> transmute(
    saved_path, file_hash, url, retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    state, agency, original_filename = file,
    note = paste0("found by probing ICE's MOA naming pattern while its sheet still said link pending (",
                  as.integer(Sys.Date() - first_appeared), " days); the PDF text names the agency or its signing date"),
    etag, last_modified))
}

if (nzchar(report)) {
  line <- \(d) sprintf("- %s — %s (%s signed %s, pending since %s): [%s](%s)", d$state, d$agency, d$model, d$signed, d$first_appeared, d$file, d$url)
  cat(c(sprintf("## MOA probe, %s", Sys.Date()), "",
        sprintf("Probed %d of %d %s, with %d requests to ice.gov%s.", probed, nrow(pending),
                if (nzchar(ids)) "requested agreements" else sprintf("agreements pending at least %d days", min_days), n_requests,
                if (is.na(stop_reason)) "" else paste0(" **Stopped early: ", stop_reason, ".**")), "",
        if (dry_run) sprintf("### Verified (dry run: nothing written) (%d)", nrow(verified)) else sprintf("### Verified and saved to `%s` (%d)", out_dir, nrow(verified)),
        if (nrow(verified)) map_chr(seq_len(nrow(verified)), \(k) line(verified[k, ])) else "None.", "",
        sprintf("### Found but not verified, so not added (%d)", nrow(unverified)),
        if (nrow(unverified)) map_chr(seq_len(nrow(unverified)), \(k) paste0(line(unverified[k, ]), ": ", unverified$reason[k])) else "None.", ""),
      file = report, sep = "\n", append = TRUE)
}
message(sprintf("verified %d, not verified %d", nrow(verified), nrow(unverified)))
