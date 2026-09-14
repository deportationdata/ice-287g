# Acquisition helpers shared by the scraper and the MOA revalidation pass; needs httr, stringr, fs.

sanitize_path_component <- function(x, fallback = "unnamed") {
  x <- str_squish(as.character(x))
  if (is.na(x) || x == "" || x == "NA") x <- fallback
  x <- str_replace_all(x, "[[:space:]]+", "_")
  x <- fs::path_sanitize(x, replacement = "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_remove_all(x, "^_+|_+$")
  if (is.na(x) || x == "") x <- fallback
  x
}

sanitize_download_filename <- function(file_name, fallback) {
  file_name <- sanitize_path_component(file_name, fallback = fallback)
  if (!str_detect(file_name, fixed("."))) file_name <- paste0(file_name, ".xlsx")
  file_name
}

make_unique_file_path <- function(folder, file_name) {
  candidate <- file.path(folder, file_name)
  if (!file.exists(candidate)) return(candidate)
  extension <- tools::file_ext(file_name)
  stem <- if (extension == "") file_name else str_remove(file_name, paste0("\\.", extension, "$"))
  i <- 2
  repeat {
    candidate <- file.path(folder, if (extension == "") paste0(stem, "_", i) else paste0(stem, "_", i, ".", extension))
    if (!file.exists(candidate)) return(candidate)
    i <- i + 1
  }
}

ice_get <- function(url, ...) {
  RETRY(
    "GET", url,
    user_agent("Mozilla/5.0"), timeout(60),
    times = 4, pause_base = 2, pause_cap = 30,
    terminate_on = c(400, 401, 403, 404),
    ...
  )
}

# the filename the server names, else the url's; fallback when neither has an extension
response_filename <- function(response, url, fallback) {
  cd <- headers(response)[["content-disposition"]]
  name <- if (!is.null(cd) && grepl("filename=", cd)) {
    str_split(cd, "filename=", n = 2)[[1]][2] |>
      str_remove(";.*$") |> str_trim() |> str_remove_all("^[\"']|[\"']$")
  } else {
    basename(str_split(url, "\\?")[[1]][1])
  }
  if (is.na(name) || name == "" || !grepl("\\.", name)) name <- fallback
  name
}

# validators verbatim: If-Modified-Since must echo the origin's Last-Modified string
response_validators <- function(response) {
  h <- headers(response)
  pick <- function(k) if (is.null(h[[k]])) NA_character_ else h[[k]]
  c(etag = pick("etag"), last_modified = pick("last-modified"))
}

# xlsx and pdf are the only bodies we save; a 200 that is really an HTML block
# page must not be archived as either
body_has_magic <- function(raw_bytes, kind = c("zip", "pdf")) {
  kind <- match.arg(kind)
  magic <- if (kind == "zip") as.raw(c(0x50, 0x4b)) else charToRaw("%PDF")
  length(raw_bytes) >= length(magic) && identical(raw_bytes[seq_along(magic)], magic)
}
