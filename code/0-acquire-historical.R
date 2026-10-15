# One-time fetch of ICE's pre-2018 fact-sheet captures and signed MOAs from the Wayback Machine.

suppressPackageStartupMessages({
  library(httr); library(jsonlite)
})

dir_fs  <- file.path("sheets", "sheets_wayback_pre2018")
dir_moa <- file.path("agreements", "agreements_wayback_pre2018")
for (d in c(dir_fs, dir_moa)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

ua    <- user_agent("287g-historical-reconstruction (academic research)")
delay <- 1.5

max_tries  <- 6
backoff_s  <- 20

`%||%` <- function(a, b) if (is.null(a)) b else a

fetch <- function(url, dest, binary = TRUE, min_bytes = 2000, referer = NULL) {
  if (file.exists(dest) && file.info(dest)$size >= min_bytes) {
    message(sprintf("  skip   %s", basename(dest))); return(invisible(TRUE))
  }
  r <- NULL; back <- backoff_s
  for (try_i in seq_len(max_tries)) {
    hdrs <- if (is.null(referer)) NULL else add_headers(
      Referer = referer,
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      `Accept-Language` = "en-US,en;q=0.9")
    r <- try(GET(url, ua, hdrs, timeout(300), write_disk(dest, overwrite = TRUE)), silent = TRUE)
    Sys.sleep(delay)
    if (!inherits(r, "try-error") && status_code(r) == 200) break
    if (file.exists(dest)) unlink(dest)
    if (try_i < max_tries) {
      message(sprintf("  wait   %s (attempt %d, backing off %ds)", basename(dest), try_i, back))
      Sys.sleep(back); back <- min(back * 2, 120)
    }
  }
  if (inherits(r, "try-error") || status_code(r) != 200) {
    message(sprintf("  FAIL   %s  (%s)", basename(dest),
                    if (inherits(r, "try-error")) "connection refused after retries" else status_code(r)))
    if (file.exists(dest)) unlink(dest); return(invisible(FALSE))
  }
  sz <- file.info(dest)$size
  if (binary) {  # guard against HTML block pages saved as .pdf
    con <- file(dest, "rb"); magic <- rawToChar(readBin(con, "raw", 4)); close(con)
    if (!identical(magic, "%PDF")) {
      message(sprintf("  NOTPDF %s (%d B) — likely a block page", basename(dest), sz))
      unlink(dest); return(invisible(FALSE))
    }
  }
  message(sprintf("  ok     %-58s %8.0f KB", basename(dest), sz / 1024))
  invisible(TRUE)
}

wayback_base <- "https://web.archive.org/web/"

ice_snapshots <- rbind(
  data.frame(ts = c("20110220090259","20110818151202","20111020094214","20120120154424",
                    "20120304155702","20120606112224","20120801234737","20121007043419",
                    "20130118070325","20130402163405","20130601201323","20130904035853",
                    "20140209051553","20140414075008","20140910162129"),
             base = "http://www.ice.gov/news/library/factsheets/287g.htm"),
  data.frame(ts = c("20150106221351","20150213232336","20150331220344","20150725035210",
                    "20151009132322","20151229225456","20160119003252","20160317154302",
                    "20160611151543","20160820000823","20161121042821","20161211024641",
                    "20170129002131","20170305225317","20170502020801","20170604015118",
                    "20170702181319","20170801021304"),
             base = "http://www.ice.gov/factsheets/287g")
)

message("== ICE fact-sheet captures ==")
for (i in seq_len(nrow(ice_snapshots))) {
  ts <- ice_snapshots$ts[i]
  fetch(paste0(wayback_base, ts, "/", ice_snapshots$base[i]),
        file.path(dir_fs, paste0("ice_287g_", ts, ".html")),
        binary = FALSE, min_bytes = 5000)
}

message("== Enumerating archived MOA PDFs via Wayback CDX ==")
cdx <- paste0("https://web.archive.org/cdx/search/cdx",
              "?url=ice.gov/doclib/foia/memorandumsofAgreementUnderstanding*",
              "&output=text&fl=original,timestamp&filter=statuscode:200",
              "&collapse=urlkey&limit=1000")
r <- tryCatch(GET(cdx, ua, timeout(300)), error = function(e) NULL)
if (is.null(r) || status_code(r) != 200) stop("Could not reach the Wayback CDX API: ", cdx)
idx <- read.table(text = content(r, as = "text", encoding = "UTF-8"), sep = " ",
                  stringsAsFactors = FALSE, col.names = c("original", "timestamp"))
idx$file <- basename(idx$original)
# id_ returns the raw archived bytes, with no Wayback toolbar injected
idx$archived_url <- sprintf("%s%sid_/%s", wayback_base, idx$timestamp, idx$original)
message(sprintf("  %d archived MOA PDFs listed", nrow(idx)))

message("== Downloading MOA PDFs (expect ~200 files, several hundred MB) ==")
for (i in seq_len(nrow(idx)))
  fetch(idx$archived_url[i], file.path(dir_moa, idx$file[i]), binary = TRUE, min_bytes = 5000)

message("\nDone. See inputs/SOURCES.csv for provenance of every file.")
