# Revalidate every MOA/addendum url the newest committed sheet links, without
# re-downloading it: ask the origin whether the document changed since we fetched
# it and store only what did. A snapshot folder is created only when something did.
#   MOA_FULL=1        revalidate every url (the sheet changed, or failures are outstanding)
#   MOA_SHARD=k       otherwise revalidate the k-th of MOA_SHARDS (default 6) url shards
#   MOA_RETRY_DEAD=1  re-ask urls already recorded as gone, even on a shard pass
suppressPackageStartupMessages({
  library(httr); library(stringr); library(purrr); library(dplyr); library(tidyr); library(readr); library(digest)
})
source("code/functions.R")
Sys.setlocale("LC_TIME", "C")

newest_dir <- list.files("sheets", "^sheets_2", full.names = TRUE) |> sort() |> last()
sheet <- list.files(newest_dir, "^(287g)?participatingAgencies.*\\.xlsx$",
                    full.names = TRUE, ignore.case = TRUE)[1]
stopifnot("no participating-agencies workbook in the newest sheets folder" = !is.na(sheet))
sheet_hash <- digest(file = sheet, algo = "sha256")

rows <- readxl::read_excel(sheet, col_types = "text") |>
  mutate(excel_row = row_number() + 1L)
hdr <- toupper(str_squish(names(rows)))
cols <- c(MOA = LETTERS[match("MOA", hdr)], addendum = LETTERS[match("ADDENDUM", hdr)])
stopifnot(
  "ICE sheet layout changed: STATE, LAW ENFORCEMENT AGENCY, MOA or ADDENDUM column missing" =
    !anyNA(cols) && all(c("STATE", "LAW ENFORCEMENT AGENCY") %in% hdr)
)
state_col  <- names(rows)[match("STATE", hdr)]
agency_col <- names(rows)[match("LAW ENFORCEMENT AGENCY", hdr)]

links <- xlsx_hyperlinks(sheet) |>
  filter(col %in% cols) |>
  transmute(excel_row = row,
            document_type = names(cols)[match(col, cols)],
            url = clean_moa_urls(url)) |>
  inner_join(rows |> select(excel_row, state = all_of(state_col), agency = all_of(agency_col)),
             by = "excel_row", relationship = "many-to-one") |>
  filter(!is.na(url), !is.na(state), !is.na(agency)) |>
  distinct(url, .keep_all = TRUE)

held <- moa_validator_map()
# zero links is the easy case; a few dozen where there were two thousand is the
# dangerous one, and only a floor against the archive catches it
if (nrow(links) == 0 || nrow(links) < 0.5 * nrow(held)) {
  stop(sprintf("only %d MOA links found where the archive holds %d urls; sheet layout drift?",
               nrow(links), nrow(held)))
}

known_dead <- list.files("agreements", "^unreachable\\.csv$", recursive = TRUE, full.names = TRUE) |>
  map(\(p) read_csv(p, col_types = cols(.default = "c"), progress = FALSE)) |>
  list_rbind() |>
  (\(d) if (nrow(d)) unique(d$url) else character())()
work <- links |>
  left_join(held, by = "url", relationship = "one-to-one") |>
  mutate(held = !is.na(file_hash) & !is.na(on_disk_path))
full <- nzchar(Sys.getenv("MOA_FULL"))
# a url the origin already reported gone is re-asked only on a full pass
if (!full && !nzchar(Sys.getenv("MOA_RETRY_DEAD"))) work <- work |> filter(!url %in% known_dead)
shard <- suppressWarnings(as.integer(Sys.getenv("MOA_SHARD")))
n_shards <- max(1L, suppressWarnings(as.integer(Sys.getenv("MOA_SHARDS", "6"))))
if (!full && !is.na(shard)) {
  work <- work |>
    filter(strtoi(substr(map_chr(url, digest, algo = "sha1"), 1, 4), 16L) %% n_shards == shard %% n_shards)
}
cat(sprintf("MOA pass: %d urls on the sheet, %d held, revalidating %d (%s)\n",
            nrow(links), sum(links$url %in% held$url), nrow(work),
            if (full) "full" else if (is.na(shard)) "full, unsharded" else paste("shard", shard)))

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
pass_dir <- file.path("agreements", paste0("agreements_", timestamp))
now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
http_date <- function(x) as.POSIXct(x, format = "%a, %d %b %Y %H:%M:%S", tz = "UTC")
body_hash <- function(r) digest(content(r, as = "raw"), algo = "sha256", serialize = FALSE)
# a 404/410 is the origin saying the document is gone: recorded, never retried
fetch <- function(url, ...) {
  r <- ice_get(url, ...)
  if (status_code(r) %in% c(404L, 410L)) {
    stop(structure(class = c("unreachable", "error", "condition"),
                   list(message = paste("HTTP", status_code(r)), call = NULL)))
  }
  r
}

# a row that records fresh validators against the copy we already hold
revalidated_row <- function(w, v, note = "revalidated; bytes identical") {
  tibble(saved_path = w$on_disk_path, file_hash = w$file_hash, url = w$url,
         retrieved_at = now_iso(), state = w$state, agency = w$agency,
         original_filename = basename(w$on_disk_path),
         note = paste(w$document_type, note, sep = "; "),
         etag = v[["etag"]], last_modified = v[["last_modified"]])
}

save_body <- function(r, w, note) {
  bytes <- content(r, as = "raw")
  if (!body_has_magic(bytes, "pdf") && !body_has_magic(bytes, "zip")) stop("body is not a pdf or xlsx")
  agency_dir <- file.path(pass_dir,
                          sanitize_path_component(w$state, "unknown_state"),
                          sanitize_path_component(w$agency, "unknown_agency"))
  dir.create(agency_dir, showWarnings = FALSE, recursive = TRUE)
  original <- response_filename(r, w$url,
                                paste0(sanitize_path_component(w$agency, "agency"), "_", w$document_type, "_agreement"))
  path <- make_unique_file_path(
    agency_dir,
    sanitize_path_component(paste0(w$document_type, "_", original), fallback = paste0("file_", w$document_type))
  )
  writeBin(bytes, path)
  v <- response_validators(r)
  tibble(saved_path = path, file_hash = digest(file = path, algo = "sha256"), url = w$url,
         retrieved_at = now_iso(), state = w$state, agency = w$agency,
         original_filename = original, note = paste(w$document_type, note, sep = "; "),
         etag = v[["etag"]], last_modified = v[["last_modified"]])
}

# a dead sheet link is usually a mistyped one: the archive index knows the spelling
# the origin served, and failing that holds the bytes itself
# two urls name the same document when their filenames agree letter for letter
same_stem <- function(a, b) {
  k <- \(x) str_to_lower(str_remove_all(URLdecode(basename(x)), "[^A-Za-z0-9]"))
  k(a) == k(b)
}
archive_index <- function(url) {
  ext <- str_extract(basename(url), "\\.[A-Za-z0-9]+$")
  stem <- str_remove(basename(url), "\\.[A-Za-z0-9]+$") |>
    str_split_1("[^A-Za-z0-9]+") |> Filter(f = nzchar) |> paste(collapse = ".*")
  q <- sprintf("http://web.archive.org/cdx/search/cdx?url=%s/*&output=text&fl=timestamp,original,statuscode&filter=statuscode:200&filter=original:(?i).*%s\\%s",
               str_remove(dirname(url), "^https?://"), stem, ext)
  lines <- tryCatch(readLines(q, warn = FALSE), error = function(e) character())
  m <- str_match(lines, "^(\\d{14}) (\\S+) 200$")
  tibble(ts = m[, 2], original = m[, 3]) |>
    filter(!is.na(ts), same_stem(original, url)) |>
    slice_max(ts, n = 1, by = original, with_ties = FALSE) |>
    arrange(original != url, desc(ts))
}
recover_document <- function(w, status) {
  found <- archive_index(w$url)
  for (i in seq_len(nrow(found))) {
    o <- found$original[i]
    if (o != w$url) {
      r <- tryCatch(ice_get(o), error = function(e) NULL)
      if (!is.null(r) && status_code(r) == 200L) {
        cat(sprintf("recovered: %s -> origin serves %s\n", w$url, o))
        return(save_body(r, w, sprintf("sheet link answers %s; origin serves %s", status, o)))
      }
    }
    r <- tryCatch(GET(sprintf("https://web.archive.org/web/%sid_/%s", found$ts[i], o),
                      user_agent("Mozilla/5.0"), timeout(180)), error = function(e) NULL)
    if (!is.null(r) && status_code(r) == 200L) {
      cat(sprintf("recovered: %s <- Wayback capture %s of %s\n", w$url, found$ts[i], o))
      return(save_body(r, w, sprintf("sheet link answers %s; Wayback capture %s of %s", status, found$ts[i], o)) |>
               mutate(etag = NA_character_, last_modified = NA_character_))
    }
  }
  NULL
}

tally <- c(n_304 = 0L, n_identical = 0L, n_changed = 0L, n_new = 0L, n_failed = 0L,
           n_unreachable = 0L, n_recovered = 0L)
bump <- function(k) tally[k] <<- tally[k] + 1L
manifest_rows <- list()
failed <- character()
unreachable <- list()

for (i in seq_len(nrow(work))) {
  w <- work[i, ]
  Sys.sleep(0.2)
  row <- tryCatch({
    if (!w$held) {
      r <- fetch(w$url); stop_for_status(r)
      bump("n_new")
      save_body(r, w, sprintf("new url in sheet %s", substr(sheet_hash, 1, 12)))
    } else if (!is.na(w$last_modified)) {
      # the origin honours If-Modified-Since (304, no body) but ignores If-None-Match,
      # so the ETag is recorded as evidence and never sent
      r <- fetch(w$url, add_headers(`If-Modified-Since` = w$last_modified))
      if (status_code(r) == 304) { bump("n_304"); NULL } else {
        stop_for_status(r)
        v <- response_validators(r)
        if (identical(body_hash(r), w$file_hash)) {
          bump("n_identical")
          if (identical(v[["last_modified"]], w$last_modified)) NULL
          else revalidated_row(w, v, sprintf("revalidated; bytes identical; validator advanced to %s", v[["last_modified"]]))
        } else {
          bump("n_changed")
          save_body(r, w, sprintf("content changed from %s first retrieved %s",
                                  substr(w$file_hash, 1, 12), substr(w$retrieved_at, 1, 10)))
        }
      }
    } else {
      # no validator yet: a HEAD settles it when the origin dates the document
      # at or before our fetch; otherwise compare bodies once and record the validator
      h <- HEAD(w$url, user_agent("Mozilla/5.0"), timeout(60))
      if (status_code(h) %in% c(404L, 410L)) stop(structure(class = c("unreachable", "error", "condition"),
                                                       list(message = paste("HTTP", status_code(h)), call = NULL)))
      stop_for_status(h)
      v <- response_validators(h)
      lm <- http_date(v[["last_modified"]])
      ra <- as.POSIXct(w$retrieved_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      len <- suppressWarnings(as.numeric(headers(h)[["content-length"]]))
      unchanged <- (!is.na(lm) && !is.na(ra) && lm <= ra) ||
        (is.na(lm) && !is.na(len) && len == file.size(w$on_disk_path))
      if (unchanged) { bump("n_identical"); revalidated_row(w, v) } else {
        r <- fetch(w$url); stop_for_status(r)
        if (identical(body_hash(r), w$file_hash)) { bump("n_identical"); revalidated_row(w, response_validators(r)) }
        else { bump("n_changed"); save_body(r, w, sprintf("content changed from %s first retrieved %s",
                                                           substr(w$file_hash, 1, 12), substr(w$retrieved_at, 1, 10))) }
      }
    }
  }, unreachable = function(e) {
    # bytes already held for a dead link are kept as they are
    if (w$held) { bump("n_unreachable"); return(NULL) }
    row <- tryCatch(recover_document(w, conditionMessage(e)), error = function(e2) NULL)
    if (!is.null(row)) { bump("n_recovered"); return(row) }
    cat(sprintf("unreachable: %s (%s)\n", w$url, conditionMessage(e)))
    unreachable[[length(unreachable) + 1]] <<- tibble(url = w$url, state = w$state, agency = w$agency,
                                                      document_type = w$document_type, status = conditionMessage(e),
                                                      checked_at = now_iso())
    bump("n_unreachable"); NULL
  }, error = function(e) {
    cat(sprintf("failed: %s (%s)\n", w$url, conditionMessage(e)))
    failed <<- c(failed, w$url); bump("n_failed"); NULL
  })
  if (!is.null(row)) manifest_rows[[length(manifest_rows) + 1]] <- row
}

newly_dead <- if (length(unreachable)) bind_rows(unreachable) |> filter(!url %in% known_dead) else tibble()
if (length(manifest_rows) || length(failed) || nrow(newly_dead)) {
  dir.create(pass_dir, showWarnings = FALSE, recursive = TRUE)
  if (length(manifest_rows)) append_manifest(pass_dir, bind_rows(manifest_rows))
  if (length(failed)) writeLines(failed, file.path(pass_dir, "failed_downloads.txt"))
  if (nrow(newly_dead)) write_csv(newly_dead, file.path(pass_dir, "unreachable.csv"))
}
dead <- c(known_dead, if (length(unreachable)) bind_rows(unreachable)$url)

# a failure recorded by an earlier run is retired once its url is held, or the
# workflow's check for outstanding failures would force a full pass every tick forever
if (any(tally[c("n_new", "n_changed", "n_unreachable")] > 0) || length(failed) == 0) {
  now_held <- moa_validator_map() |> filter(!is.na(on_disk_path)) |> pull(url)
  for (f in list.files("agreements", "^failed_downloads\\.txt$", recursive = TRUE, full.names = TRUE)) {
    if (dirname(f) == pass_dir) next
    # retired once held, or once the sheet no longer links it and nothing is left to fetch
    old <- readLines(f, warn = FALSE)
    still <- old[!old %in% now_held & old %in% links$url & !old %in% dead]
    if (length(still)) writeLines(still, f) else unlink(f)
  }
}

dir.create("manifests", showWarnings = FALSE)
log_path <- "manifests/moa-passes.csv"
log_row <- tibble(pass_started_at = now_iso(), sheet_hash = sheet_hash,
                  mode = if (full) "full" else if (is.na(shard)) "unsharded" else paste0("shard ", shard, "/", n_shards),
                  n_urls = nrow(work), n_304 = tally[["n_304"]], n_identical = tally[["n_identical"]],
                  n_changed = tally[["n_changed"]], n_new = tally[["n_new"]], n_failed = tally[["n_failed"]],
                  n_unreachable = tally[["n_unreachable"]], n_recovered = tally[["n_recovered"]])
write_csv(log_row, log_path, append = file.exists(log_path))

cat(sprintf("MOA pass done: %d not modified, %d identical, %d changed, %d new, %d failed, %d unreachable, %d recovered%s\n",
            tally[["n_304"]], tally[["n_identical"]], tally[["n_changed"]], tally[["n_new"]], tally[["n_failed"]],
            tally[["n_unreachable"]], tally[["n_recovered"]],
            if (dir.exists(pass_dir)) paste0(" -> ", pass_dir) else " (nothing written)"))
