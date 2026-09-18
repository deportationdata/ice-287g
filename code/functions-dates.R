# Dates of archived sheets.

# ICE's own date for a sheet, from its filename (participatingAgenciesMMDDYYYY,
# participatingAgencies_YYYYMMDD, 287gParticipatingAgenciesMMDDYY since 2026-09-15,
# or ParticipatingAgencyMMDDYY since 2026-09-18);
# ICE typos years ("...03113025am" is year 3025), and an impossible date is no date
ice_filename_date <- function(paths) {
  name <- basename(paths)
  # a four-digit year may run into a typo'd suffix ("...041720263pm"); a two-digit year may not
  mdy <- str_match(name, regex("^(?:287g)?participatingAgenc(?:y|ies)(\\d{2})(\\d{2})(\\d{4}|\\d{2}(?!\\d))", ignore_case = TRUE))
  iso <- str_match(name, regex("^(?:287g)?participatingAgenc(?:y|ies)_(\\d{4})(\\d{2})(\\d{2})", ignore_case = TRUE))
  # a two-digit year ("287gParticipatingAgencies091526pm", first seen 2026-09-15) is 20yy
  year <- if_else(nchar(mdy[, 4]) == 2, paste0("20", mdy[, 4]), mdy[, 4])
  out <- coalesce(as.Date(paste(year, mdy[, 2], mdy[, 3], sep = "-"), format = "%Y-%m-%d"),
                  as.Date(paste(iso[, 2], iso[, 3], iso[, 4], sep = "-"), format = "%Y-%m-%d"))
  out[!is.na(out) & (out > Sys.Date() + 30 | out < as.Date("2005-01-01"))] <- NA
  out
}

# the part of day ICE appends when it posts more than one list on a date
ice_filename_part <- function(paths) {
  str_to_lower(str_match(basename(paths), regex("^(?:287g)?participatingAgenc(?:y|ies)(?:\\d{8}|\\d{6})(am|mid|pm)", ignore_case = TRUE))[, 2])
}

# SIGNED arrives as raw text because ICE mixes native dates, text dates and
# typo'd text dates in one column; letting readxl guess drops the typo'd rows
coerce_signed_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(round(x), origin = "1899-12-30"))
  chr <- str_squish(as.character(x))
  # repair ICE's five-digit years ("5/31/20026") and dropped slashes ("7/72025")
  chr <- str_replace(chr, "/200(\\d{2})$", "/20\\1")
  chr <- str_replace(chr, "^(\\d{1,2})/(\\d{1,2})(\\d{4})$", "\\1/\\2/\\3")
  # window-check per format so a near-parse cannot shadow the right one; base
  # as.Date, not mdy, which trains on the whole vector and NAs it all
  in_window <- function(d) {
    d[!is.na(d) & (d < as.Date("2000-01-01") | d > Sys.Date() + 365)] <- NA
    d
  }
  serial <- suppressWarnings(as.numeric(chr))
  serial[!is.na(serial) & (serial < 20000 | serial > 60000)] <- NA
  # a two-digit year is only ever read from a two-digit string, so an
  # out-of-window four-digit year cannot be truncated into a plausible date
  two_digit <- if_else(str_detect(chr, "^\\d{1,2}/\\d{1,2}/\\d{2}$"), chr, NA_character_)
  out <- coalesce(
    in_window(as.Date(round(serial), origin = "1899-12-30")),
    in_window(as.Date(chr, format = "%m/%d/%Y")),
    in_window(as.Date(chr, format = "%Y-%m-%d")),
    in_window(as.Date(two_digit, format = "%m/%d/%y"))
  )
  bad <- unique(chr[is.na(out) & !is.na(chr) & nzchar(chr)])
  if (length(bad)) warning("unparsed signed values dropped: ", paste(bad, collapse = ", "))
  out
}
