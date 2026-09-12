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
  raw_dir <- file.path(out_dir, "raw")
  dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

  stamp_cache <- file.path(raw_dir, "cdx_stamps.txt")
  if (file.exists(stamp_cache)) {
    cached <- read_lines(stamp_cache)
    m <- str_match(cached, "^(\\d{14})(?:\\t(.*))?$")
    stamps <- tibble(
      ts = m[, 2],
      url = coalesce(m[, 3], queries$url[1])
    ) |>
      filter(!is.na(ts))
  } else {
    stamps <- pmap(queries, function(url, from, to, collapse) {
      tibble(ts = cdx(url, from, to, collapse), url = url)
    }) |>
      list_rbind() |>
      bind_rows(extra_stamps) |>
      distinct(ts, url) |>
      arrange(ts)
    write_lines(paste(stamps$ts, stamps$url, sep = "\t"), stamp_cache)
  }
  cat(out_dir, ": captures to process:", nrow(stamps), "\n")

  existing <- list.files("sheets", pattern = "\\.xlsx$", recursive = TRUE,
                         full.names = TRUE, ignore.case = TRUE)
  existing_hashes <- vapply(
    existing,
    function(p) digest::digest(file = p, algo = "sha256"),
    character(1)
  )

  seen_table_keys <- str_match(
    list.files(out_dir, pattern = "^html_\\d{8}_[0-9a-f]{12}\\.xlsx$"),
    "^html_(\\d{8})_([0-9a-f]{12})"
  )
  seen_table_keys <- paste(seen_table_keys[, 2], seen_table_keys[, 3])

  n_table <- 0; n_xlsx <- 0; n_skip <- 0; n_html <- 0

  for (i in seq_len(nrow(stamps))) {
    ts <- stamps$ts[i]
    page_url <- stamps$url[i]
    suffix <- url_suffix(page_url)
    raw_path <- file.path(raw_dir, paste0(ts, suffix, ".html"))
    cap_url <- sprintf("https://web.archive.org/web/%sid_/%s", ts, page_url)

    if (!file.exists(raw_path)) {
      ok <- FALSE
      for (k in 1:3) {
        Sys.sleep(1)
        ok <- tryCatch({
          download.file(cap_url, raw_path, quiet = TRUE)
          file.size(raw_path) > 10000
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
        note = "raw Wayback capture fetched by code/0-287g-wayback-backfill.R"
      ))
    }
    maybe_gunzip(raw_path)

    html <- tryCatch(read_html(raw_path), error = function(e) NULL)
    if (is.null(html)) { cat(ts, "unparseable html\n"); next }

    tabs <- tryCatch(html_table(html), error = function(e) list())
    # html_table only promotes <th>, so promote ICE's <td> header rows by hand
    tabs <- map(tabs, function(t) {
      n <- str_squish(str_to_upper(names(t)))
      if (!"LAW ENFORCEMENT AGENCY" %in% n && nrow(t) > 0) {
        first <- str_squish(str_to_upper(as.character(unlist(t[1, ]))))
        looks_header <-
          any(first %in% c("LAW ENFORCEMENT AGENCY", "MOA NAME", "AGENCY")) &&
          any(str_detect(coalesce(first, ""), "SIGNED|OFFICIAL APPROVAL"))
        if (looks_header) {
          blank <- is.na(first) | first == ""
          first[blank] <- paste0("X", seq_len(sum(blank)))
          names(t) <- make.unique(first)
          t <- t[-1, ]
        }
      }
      names(t) <- str_squish(str_to_upper(names(t)))
      names(t)[names(t) == "DATES SIGNED"] <- "SIGNED"
      # pre-2018: MOA NAME/OFFICIAL APPROVAL headers, state abbrevs, TFO/JEO
      legacy <- FALSE
      if ("MOA NAME" %in% names(t)) {
        names(t)[names(t) == "MOA NAME"] <- "LAW ENFORCEMENT AGENCY"
        legacy <- TRUE
      }
      if ("OFFICIAL APPROVAL" %in% names(t)) {
        names(t)[names(t) == "OFFICIAL APPROVAL"] <- "SIGNED"
        legacy <- TRUE
      }
      if (legacy && !"LAW ENFORCEMENT AGENCY" %in% names(t) &&
            "AGENCY" %in% names(t)) {
        names(t)[names(t) == "AGENCY"] <- "LAW ENFORCEMENT AGENCY"
      }
      if (legacy && !"SUPPORT TYPE" %in% names(t) && "TYPE" %in% names(t)) {
        names(t)[names(t) == "TYPE"] <- "SUPPORT TYPE"
      }
      if (legacy &&
            all(c("STATE", "LAW ENFORCEMENT AGENCY", "SUPPORT TYPE") %in%
                  names(t))) {
        ab <- c(setNames(state.name, state.abb),
                DC = "District of Columbia")
        st <- str_remove_all(
          str_squish(str_to_upper(as.character(t$STATE))), "[*†]+$"
        )
        t$STATE <- coalesce(ab[st], as.character(t$STATE))
        ty <- str_squish(str_to_upper(as.character(t$`SUPPORT TYPE`)))
        t$`SUPPORT TYPE` <- case_when(
          ty == "TFO" ~ "TASK FORCE",
          ty == "JEO" ~ "JAIL ENFORCEMENT",
          str_detect(ty, "JEO") & str_detect(ty, "TFO") ~
            "JAIL & TASK FORCE",
          TRUE ~ as.character(t$`SUPPORT TYPE`)
        )
      }
      t
    })
    good <- keep(tabs, function(t) {
      all(c("LAW ENFORCEMENT AGENCY", "SIGNED") %in% names(t))
    })

    if (length(good) > 0) {
      # some eras split off a second table of agencies without trained officers
      t <- map(good, function(g) {
        g |>
          # some eras append footnote markers to cells ("DELAWARE**")
          mutate(across(everything(),
                        ~ str_remove_all(str_squish(as.character(.x)),
                                         "[*†]+$"))) |>
          filter(`LAW ENFORCEMENT AGENCY` != "")
      }) |>
        bind_rows()
      # pre-2011 tables carry mm/dd/yyyy dates, later ones ISO
      t$SIGNED <- coerce_signed_date(t$SIGNED)
      content_hash <- substr(digest::digest(t), 1, 12)
      # repeats across captures still matter for timing; skip same-day only
      key <- paste(substr(ts, 1, 8), content_hash)
      if (key %in% seen_table_keys) { n_skip <- n_skip + 1; next }
      seen_table_keys <- c(seen_table_keys, key)
      out <- file.path(
        out_dir,
        sprintf("html_%s_%s.xlsx", substr(ts, 1, 8), content_hash)
      )
      writexl::write_xlsx(t, out)
      n_table <- n_table + 1
      append_manifest(out_dir, tibble(
        saved_path = out,
        file_hash = digest::digest(file = out, algo = "sha256"),
        url = cap_url,
        retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        note = sprintf(
          "table extracted from raw/%s%s.html by code/0-287g-wayback-backfill.R",
          ts, suffix
        )
      ))
      cat(ts, "table rows:", nrow(t), "->", basename(out), "\n")
      next
    }

    links <- html |> html_elements("a") |> html_attr("href")
    links <- links[!is.na(links)]
    xl <- links[str_detect(links, regex("participatingAgencies[^\"]*\\.xlsx",
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
    "%s done: %d new html saved, %d table snapshots, %d xlsx, %d skipped\n",
    out_dir, n_html, n_table, n_xlsx, n_skip
  ))
}

for (run in runs) {
  run_backfill(run$out_dir, run$queries, run$extra_stamps)
}
