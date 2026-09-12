# One-time fetch of the pre-2018 sources: captures, MOA pdfs, reports, panel.

suppressPackageStartupMessages({
  library(httr); library(jsonlite)
})

ROOT    <- "."
SRC     <- file.path(ROOT, "inputs", "pre2018")
DIR_FS  <- file.path(ROOT, "sheets", "sheets_wayback_pre2018", "raw")
DIR_MOA <- file.path(ROOT, "agreements", "agreements_wayback_pre2018")
DIR_REP <- file.path(SRC, "reports")
DIR_PAN <- file.path(SRC, "panels")
for (d in c(DIR_FS, DIR_MOA, DIR_REP, DIR_PAN)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

UA    <- user_agent("287g-historical-reconstruction (academic research)")
DELAY <- 1.5

# archive.org refuses connections after ~20 quick requests: stay serial
MAX_TRIES  <- 6
BACKOFF_S  <- 20

`%||%` <- function(a, b) if (is.null(a)) b else a

fetch <- function(url, dest, binary = TRUE, min_bytes = 2000, referer = NULL) {
  if (file.exists(dest) && file.info(dest)$size >= min_bytes) {
    message(sprintf("  skip   %s", basename(dest))); return(invisible(TRUE))
  }
  r <- NULL; back <- BACKOFF_S
  for (try_i in seq_len(MAX_TRIES)) {
    hdrs <- if (is.null(referer)) NULL else add_headers(
      Referer = referer,
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      `Accept-Language` = "en-US,en;q=0.9")
    r <- try(GET(url, UA, hdrs, timeout(300), write_disk(dest, overwrite = TRUE)), silent = TRUE)
    Sys.sleep(DELAY)
    if (!inherits(r, "try-error") && status_code(r) == 200) break
    if (file.exists(dest)) unlink(dest)
    if (try_i < MAX_TRIES) {
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

WB <- "https://web.archive.org/web/"

ICE_SNAPSHOTS <- rbind(
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
for (i in seq_len(nrow(ICE_SNAPSHOTS))) {
  ts <- ICE_SNAPSHOTS$ts[i]
  fetch(paste0(WB, ts, "/", ICE_SNAPSHOTS$base[i]),
        file.path(DIR_FS, paste0("ice_287g_", ts, ".html")),
        binary = FALSE, min_bytes = 5000)
}
write.csv(ICE_SNAPSHOTS, file.path(DIR_FS, "_snapshot_manifest.csv"), row.names = FALSE)

# only Yale's roster page is recoverable; its MOU pdfs 404 in every capture
message("== Yale WIRAC FOIA page ==")
fetch(paste0(WB, "20080807125556/http://islandia.law.yale.edu/wirc/287g_foia.html"),
      file.path(SRC, "yale_wirac_287g_foia_2008.html"), binary = FALSE, min_bytes = 2000)

message("== Enumerating archived MOA PDFs via Wayback CDX ==")
cdx <- paste0("https://web.archive.org/cdx/search/cdx",
              "?url=ice.gov/doclib/foia/memorandumsofAgreementUnderstanding*",
              "&output=text&fl=original,timestamp&filter=statuscode:200",
              "&collapse=urlkey&limit=1000")
idx_file <- file.path(DIR_MOA, "_cdx_index.tsv")
if (!file.exists(idx_file)) {
  r <- try(GET(cdx, UA, timeout(300), write_disk(idx_file, overwrite = TRUE)), silent = TRUE)
  if (inherits(r, "try-error") || status_code(r) != 200)
    stop("Could not reach the Wayback CDX API. Open the URL above in a browser, ",
         "save the output as sources/moa_pdfs/_cdx_index.tsv, and re-run.")
}
idx <- read.table(idx_file, sep = " ", stringsAsFactors = FALSE,
                  col.names = c("original", "timestamp"))
idx$file <- basename(idx$original)
# id_ returns the raw archived bytes, with no Wayback toolbar injected
idx$archived_url <- sprintf("%s%sid_/%s", WB, idx$timestamp, idx$original)
write.csv(idx, file.path(DIR_MOA, "_moa_manifest.csv"), row.names = FALSE)
message(sprintf("  %d archived MOA PDFs listed", nrow(idx)))

message("== Downloading MOA PDFs (expect ~200 files, several hundred MB) ==")
for (i in seq_len(nrow(idx)))
  fetch(idx$archived_url[i], file.path(DIR_MOA, idx$file[i]), binary = TRUE, min_bytes = 5000)

# oig.dhs.gov and gao.gov block scripted access; TRAC mirrors the same pdfs
REPORTS <- c(
  "GAO-09-109_Jan2009.pdf"                            = "https://tracreports.org/tracker/dynadata/2009_03/d09109.2.pdf",
  "DHS-OIG-10-63_Mar2010.pdf"                         = "https://tracreports.org/tracker/dynadata/2010_04/OIG_10-63_Mar10.pdf",
  "DHS-OIG-10-124_Sep2010.pdf"                        = "https://tracreports.org/tracker/dynadata/2010_10/OIG_10-124_Sep10.pdf",
  "DHS-OIG-11-119_Sep2011.pdf"                        = "https://tracreports.org/tracker/dynadata/2011_11/OIG_11-119_Sep11.pdf",
  "DHS-OIG-12-130_Sep2012.pdf"                        = "https://tracreports.org/tracker/dynadata/2012_10/OIG_12-130_Sep12.pdf",
  "CRS-RL32270_2009-03-11.pdf"                        = "https://www.everycrsreport.com/files/20090311_RL32270_a7bbe8763684424b48f0d4b1d61c92412ac50d0c.pdf",
  "CRS-RL32270_2007-08-30.pdf"                        = "https://www.everycrsreport.com/files/20070830_RL32270_137cbfcfdb2783a66987c265ab32bdf06fb9e40b.pdf",
  "CHRG-111hhrg49374_House-Homeland_2009-03-04.pdf"   = "https://www.govinfo.gov/content/pkg/CHRG-111hhrg49374/pdf/CHRG-111hhrg49374.pdf",
  "MPI_Delegation-and-Divergence_Jan2011.pdf"         = "https://www.migrationpolicy.org/sites/default/files/publications/287g-divergence.pdf",
  "MPI_Program-in-Flux_Mar2010.pdf"                   = "https://www.migrationpolicy.org/sites/default/files/publications/287g-March2010.pdf",
  "NCLR_Lacayo_2010.pdf"                              = "https://unidosus.org/wp-content/uploads/2021/07/287g_issuebrief_pubstore.pdf",
  "CIS_Vaughan-Edwards_Oct2009.pdf"                   = "https://cis.org/sites/cis.org/files/articles/2009/287g.pdf",
  "Kostandini-et-al_AJAE_2014.pdf"                    = "https://www.ncaeonline.org/wp-content/uploads/2021/03/Konstandini-et-al.-2014-Impact-of-Immigration-Enforcement-on-U.S.-Farming.pdf",
  "NIJ_Capellan-Sorg_2022.pdf"                        = "https://www.ojp.gov/pdffiles1/nij/grants/305488.pdf",
  "ACLU-NC_UNC_Feb2009.pdf"                           = "https://law.unc.edu/wp-content/uploads/2019/10/287gpolicyreview.pdf"
)
message("== Reports ==")
for (nm in names(REPORTS)) fetch(REPORTS[[nm]], file.path(DIR_REP, nm), binary = TRUE)

# cato.org returns a 212-byte HTML stub to a request without a Referer
fetch("https://www.cato.org/sites/cato.org/files/pubs/pdf/working-paper-52-updated.pdf",
      file.path(DIR_REP, "Cato-WP52_Forrester-Nowrasteh_2018.pdf"), binary = TRUE,
      referer = "https://www.cato.org/working-paper/do-immigration-enforcement-programs-reduce-crime")

message("== East et al. county-month panel ==")
dta <- file.path(DIR_PAN, "287g_SC_EVerify_5_13_22.dta")
if (!file.exists(dta)) {
  fetch(paste0("https://raw.githubusercontent.com/cneast/East_etal_2022/main/",
               "data/287g_SC_EVerify_5_13_22.dta"), dta, binary = FALSE, min_bytes = 1e6)
} else {
  message("  skip   287g_SC_EVerify_5_13_22.dta")
}

message("\nDone. See ../README.md for provenance of every file.")
