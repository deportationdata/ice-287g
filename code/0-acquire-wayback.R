# Backfill 287(g) rosters from Wayback captures into sheets/sheets_wayback_*.

library(tidyverse)
library(rvest)

source("code/functions.R")

modern_url <- "https://www.ice.gov/identify-and-arrest/287g"
factsheet_url <- "http://www.ice.gov/factsheets/287g"
# the roster's only home between the 2017 fact-sheet url and the 2021 modern url
landing_url <- "https://www.ice.gov/287g"
# the roster's pre-2011 homes, before the news/library/factsheets era
pre2011_fs_url <- "http://www.ice.gov/pi/news/factsheets/section287_g.htm"
pre2011_fs2_url <- "http://www.ice.gov/pi/news/factsheets/section287g.htm"
pre2011_partners_url <- "http://www.ice.gov/partners/287g/Section287_g.htm"

# suffix per source url, so same-day captures of two pages don't collide
url_suffix <- function(u) {
  case_when(
    u == factsheet_url ~ "_fs",
    u == landing_url ~ "_lp",
    u == pre2011_fs_url ~ "_s1",
    u == pre2011_fs2_url ~ "_s2",
    u == pre2011_partners_url ~ "_s3",
    TRUE ~ ""
  )
}

runs <- list(
  list(
    out_dir = "sheets/sheets_wayback_20260911",
    queries = tribble(
      ~url, ~from, ~to, ~collapse,
      modern_url, "20220101", "20241231", "timestamp:6",
      modern_url, "20250101", "20250315", "timestamp:8"
    ),
    # captures at which the table's content changed
    extra_stamps = tibble(
      ts = c(
        "20220224143243", "20220304201114", "20220809011827",
        "20220826203419", "20221005190910", "20230106030652",
        "20230427120750", "20230615124612", "20240202203405",
        "20240627214404", "20241210194132", "20241218025939"
      ),
      url = modern_url
    )
  ),
  list(
    out_dir = "sheets/sheets_wayback_20260912",
    queries = tribble(
      ~url, ~from, ~to, ~collapse,
      modern_url, "20170901", "20211231", "timestamp:6",
      factsheet_url, "20170901", "20191231", "timestamp:6",
      landing_url, "20170901", "20211231", "timestamp:6"
    ),
    extra_stamps = tibble(ts = character(), url = character())
  ),
  list(
    # the early-2021 pages themselves, one per distinct content
    out_dir = "sheets/sheets_wayback_20260814",
    queries = tribble(
      ~url, ~from, ~to, ~collapse,
      modern_url, "20210101", "20210228", "digest"
    ),
    extra_stamps = tibble(ts = character(), url = character())
  ),
  list(
    out_dir = "sheets/sheets_wayback_pre2011",
    queries = tribble(
      ~url, ~from, ~to, ~collapse,
      pre2011_fs_url, "20060101", "20110219", "timestamp:6",
      pre2011_fs2_url, "20060101", "20110219", "timestamp:6",
      # the partners page froze in Jan 2009 and re-served that roster into 2010
      pre2011_partners_url, "20060101", "20090131", "timestamp:6"
    ),
    extra_stamps = tibble(ts = character(), url = character())
  )
)

cdx <- function(page_url, from, to, collapse) {
  u <- sprintf(
    paste0(
      "http://web.archive.org/cdx/search/cdx?url=%s&from=%s&to=%s",
      "&output=text&fl=timestamp,statuscode&collapse=%s"
    ),
    URLencode(page_url, reserved = TRUE), from, to, collapse
  )
  # the CDX API 503s freely under load
  for (i in 1:5) {
    lines <- tryCatch(readLines(u, warn = FALSE), error = function(e) NULL)
    if (!is.null(lines)) break
    Sys.sleep(15 * 2^(i - 1))
  }
  if (is.null(lines)) stop("CDX query failed after retries: ", u)
  m <- str_match(lines, "^(\\d{14}) (200)$")
  m[!is.na(m[, 1]), 2]
}

# id_ returns the raw archived bytes, which may be a gzip stream
maybe_gunzip <- function(path) {
  magic <- readBin(path, "raw", 2)
  if (length(magic) == 2 && identical(magic, as.raw(c(0x1f, 0x8b)))) {
    txt <- readLines(gzfile(path), warn = FALSE)
    writeLines(txt, path)
  }
}

run_backfill <- function(out_dir, queries, extra_stamps) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  stamps <- pmap(queries, function(url, from, to, collapse) {
    tibble(ts = cdx(url, from, to, collapse), url = url)
  }) |>
    list_rbind() |>
    bind_rows(extra_stamps) |>
    distinct(ts, url) |>
    arrange(ts)
  if (!nrow(stamps)) stop("CDX returned no captures for ", paste(queries$url, collapse = ", "),
                          "; refusing to run an empty worklist")
  cat(out_dir, ": captures to process:", nrow(stamps), "\n")

  existing <- list.files("sheets", pattern = "\\.xlsx$", recursive = TRUE,
                         full.names = TRUE, ignore.case = TRUE)
  existing_hashes <- vapply(
    existing,
    function(p) digest::digest(file = p, algo = "sha256"),
    character(1)
  )

  n_table <- 0; n_xlsx <- 0; n_skip <- 0; n_html <- 0

  for (i in seq_len(nrow(stamps))) {
    ts <- stamps$ts[i]
    page_url <- stamps$url[i]
    suffix <- url_suffix(page_url)
    raw_path <- file.path(out_dir, paste0(ts, suffix, ".html"))
    cap_url <- sprintf("https://web.archive.org/web/%sid_/%s", ts, page_url)

    if (!file.exists(raw_path)) {
      ok <- FALSE
      for (k in 1:3) {
        Sys.sleep(1)
        ok <- tryCatch({
          download.file(cap_url, raw_path, quiet = TRUE)
          # gunzip before the size floor: a gzip-served roster compresses below it
          maybe_gunzip(raw_path)
          file.size(raw_path) > 10000 &&
            any(str_detect(read_lines(raw_path, n_max = 400), regex("<html|<body|<table", ignore_case = TRUE)))
        }, error = function(e) FALSE)
        if (ok) break
        unlink(raw_path)
        Sys.sleep(4 * k)
      }
      if (!ok) { cat(ts, "fetch failed\n"); next }
      n_html <- n_html + 1
      maybe_gunzip(raw_path)
      append_manifest(out_dir, tibble(
        saved_path = raw_path,
        file_hash = digest::digest(file = raw_path, algo = "sha256"),
        url = cap_url,
        retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        capture_time = ts,
        note = "raw Wayback capture fetched by code/0-acquire-wayback.R"
      ))
    }
    maybe_gunzip(raw_path)

    html <- tryCatch(read_html(raw_path), error = function(e) NULL)
    if (is.null(html)) { cat(ts, "unparseable html\n"); next }

    # a page that carries the roster table is complete as saved: 1-read-sheets.R
    # reads the table from the raw capture
    if (!is.null(read_roster_html(raw_path))) { n_table <- n_table + 1; next }

    links <- html |> html_elements("a") |> html_attr("href")
    links <- links[!is.na(links)]
    xl <- links[str_detect(links, regex("participatingAgenc(?:y|ies)[^\"]*\\.xlsx|file-download/download/public/\\d+",
                                        ignore_case = TRUE))]
    if (length(xl) > 0) {
      xl_url <- xl[1]
      if (!str_detect(xl_url, "^https?://")) {
        xl_url <- paste0("https://www.ice.gov", xl_url)
      }
      if (!str_detect(xl_url, "web\\.archive\\.org")) {
        xl_url <- sprintf("https://web.archive.org/web/%sid_/%s", ts, xl_url)
      }
      tmp <- tempfile(fileext = ".xlsx")
      ok <- tryCatch({
        download.file(xl_url, tmp, quiet = TRUE, mode = "wb")
        TRUE
      }, error = function(e) FALSE)
      if (ok && file.size(tmp) > 5000) {
        h <- digest::digest(file = tmp, algo = "sha256")
        if (h %in% existing_hashes) { n_skip <- n_skip + 1; next }
        existing_hashes <- c(existing_hashes, h)
        out <- file.path(
          out_dir,
          sprintf("participatingAgencies_%s_%s.xlsx",
                  substr(ts, 1, 8), substr(h, 1, 12))
        )
        file.copy(tmp, out)
        n_xlsx <- n_xlsx + 1
        append_manifest(out_dir, tibble(
          saved_path = out,
          file_hash = h,
          url = xl_url,
          retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          capture_time = ts,
          note = sprintf("workbook linked from capture %s", ts)
        ))
        cat(ts, "xlsx ->", basename(out), "\n")
      } else {
        cat(ts, "xlsx link found but download failed\n")
      }
      next
    }
    cat(ts, "no table, no xlsx link\n")
  }

  cat(sprintf(
    "%s done: %d new html saved, %d carry the roster table, %d xlsx, %d skipped\n",
    out_dir, n_html, n_table, n_xlsx, n_skip
  ))
}

for (run in runs) {
  run_backfill(run$out_dir, run$queries, run$extra_stamps)
}
