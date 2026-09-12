xlsx_hyperlinks <- function(path, sheet = 1) {
  files <- sprintf(
    c("xl/worksheets/sheet%d.xml", "xl/worksheets/_rels/sheet%d.xml.rels"),
    sheet
  )
  tmp <- tempfile()
  unzip(path, files = files, exdir = tmp)

  cells <- xml2::xml_find_all(
    xml2::read_xml(file.path(tmp, files[1])),
    ".//d1:hyperlink"
  )
  rels <- xml2::xml_find_all(
    xml2::read_xml(file.path(tmp, files[2])),
    ".//d1:Relationship"
  )

  tibble(
    ref = xml2::xml_attr(cells, "ref"),
    id = xml2::xml_attr(cells, "id")
  ) |>
    left_join(
      tibble(
        id = xml2::xml_attr(rels, "Id"),
        url = xml2::xml_attr(rels, "Target")
      ),
      by = "id"
    ) |>
    # an internal-anchor link has no relationship id and no external url
    filter(!is.na(url)) |>
    # a ref can be a range ("G5:G6"); take every row it covers, or the url
    # lands on the wrong agreement
    mutate(
      col = str_extract(ref, "^[A-Z]+"),
      row_start = as.integer(str_match(ref, "^[A-Z]+(\\d+)")[, 2]),
      row_end = as.integer(str_match(ref, ":[A-Z]+(\\d+)$")[, 2]),
      row_end = coalesce(row_end, row_start)
    ) |>
    mutate(row = map2(row_start, row_end, seq)) |>
    unnest(row) |>
    transmute(col, row = as.integer(row), url)
}

# MOA links are hand-pasted: unwrap Outlook safelinks and repair paste typos
clean_moa_urls <- function(url) {
  url |>
    map_chr(\(u) {
      if (is.na(u)) {
        NA_character_
      } else if (str_detect(u, "safelinks\\.protection\\.outlook\\.com")) {
        inner <- str_extract(u, "(?<=[?&]url=)[^&]+")
        if (is.na(inner)) u else URLdecode(inner)
      } else {
        u
      }
    }) |>
    str_remove("^chrome-extension://[a-z]+/") |>
    str_replace("^https?:/(?=[^/])", "https://") |>
    str_replace("^http://", "https://")
}

snap_state_name <- function(state, valid_states, max_dist = 2) {
  key <- norm_state(state)
  valid_key <- norm_state(valid_states)
  vapply(
    seq_along(key),
    function(i) {
      if (is.na(key[i]) || key[i] %in% valid_key) {
        return(state[i])
      }
      d <- stringdist::stringdist(key[i], valid_key, method = "osa")
      hits <- which(d == min(d))
      if (min(d) <= max_dist && length(hits) == 1) {
        valid_states[hits]
      } else {
        state[i]
      }
    },
    character(1)
  )
}

read_parquet_retry <- function(path, times = 4, timeout_seconds = 300) {
  old_timeout <- getOption("timeout")
  options(timeout = max(old_timeout, timeout_seconds))
  on.exit(options(timeout = old_timeout), add = TRUE)

  last_error <- NULL

  for (attempt in seq_len(times)) {
    result <- tryCatch(
      arrow::read_parquet(path),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(result)) {
      return(result)
    }

    if (attempt < times) {
      pause <- min(10 * attempt, 60)
      message(
        "Failed to read parquet on attempt ",
        attempt,
        " of ",
        times,
        "; retrying in ",
        pause,
        " seconds: ",
        conditionMessage(last_error)
      )
      Sys.sleep(pause)
    }
  }

  stop(
    "Failed to read parquet after ",
    times,
    " attempts: ",
    conditionMessage(last_error),
    call. = FALSE
  )
}

norm_key <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("\\bst\\.?\\b", "saint") |>
    str_replace_all("\\bpd\\b", " ") |>
    str_replace_all(
      "\\b(county|city|town|village|borough|township|municipality)\\b",
      " "
    ) |>
    str_replace_all(
      "\\b(police|dept|department|public|safety|office)\\b",
      " "
    ) |>
    str_replace_all("\\b(of|the|and|for)\\b", " ") |>
    str_replace_all("[^a-z0-9]", "") |>
    str_squish()
}

# match tiers, strongest first: own-county name, then statewide exact full
# name, then the aggressive key that drops jurisdiction words
match_tier_rank <- function(match_type) {
  case_when(
    match_type == "exact_state_county_agency_name" ~ 1L,
    match_type == "unique_state_full_name" ~ 2L,
    match_type == "unique_state_agency_name" ~ 3L,
    TRUE ~ 4L
  )
}

read_sf_parquet <- function(path, crs = 4326) {
  if (requireNamespace("sfarrow", quietly = TRUE)) {
    return(sfarrow::st_read_parquet(path))
  }

  x <- arrow::read_parquet(path)
  if (!"geometry" %in% names(x)) {
    stop("No geometry column found in ", path)
  }

  geom <- sf::st_as_sfc(x$geometry, EWKB = FALSE, crs = crs)
  x$geometry <- NULL
  sf::st_as_sf(x, sf_column_name = "geometry", geometry = geom)
}

write_sf_parquet <- function(x, path) {
  if (requireNamespace("sfarrow", quietly = TRUE)) {
    return(sfarrow::st_write_parquet(x, path))
  }

  geom <- sf::st_as_binary(sf::st_geometry(x), EWKB = FALSE)
  out <- sf::st_drop_geometry(x)
  out$geometry <- structure(
    as.list(geom),
    class = c("arrow_binary", "blob", "vctrs_list_of", "vctrs_vctr", "list")
  )
  arrow::write_parquet(out, path)
}

norm_state <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z]", "") |>
    str_squish()
}

# delete apostrophes rather than space them, and expand "ste" before "st"
norm_place <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("'", "") |>
    str_replace_all("\\bste\\.?\\b", "sainte") |>
    str_replace_all("\\bst\\.?\\b", "saint") |>
    str_replace_all("\\btwp\\.?\\b", "township") |>
    str_replace_all(
      "\\b(county|city|town|village|borough|township|municipality)\\b",
      " "
    ) |>
    # a remaining -borough belongs to the name ("Middlesborough" vs "Middlesboro")
    str_replace_all("borough\\b", "boro") |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    # a bare "n" is "and" once quoting is stripped (Cut "N" Shoot)
    str_replace_all("\\bn\\b", "and") |>
    str_squish()
}

# rosters drop the "Parish" suffix ICE carries, so strip it; "#N/A" -> NA so
# sentinels never key-match
norm_ori_county <- function(x) {
  x <- if_else(
    str_to_lower(str_squish(x)) %in% c("#na", "#n/a", "na", "n/a", ""),
    NA_character_,
    x
  )
  x |>
    norm_place() |>
    str_replace_all("\\bparish\\b", " ") |>
    str_squish()
}

# LEAIC/NCIC abbreviates heavily, so expand before keying; transliterate first
# so curly-apostrophe possessives reach the singularization
expand_leaic_abbrev <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("\\bpct\\.?\\s*", " precinct ") |>
    str_replace_all("\\bco\\.?\\b", " county ") |>
    str_replace_all("\\bregl\\.?\\b", " regional ") |>
    str_replace_all("\\btwp\\.?\\b", " township ") |>
    str_replace_all("\\bboro\\.?\\b", " borough ") |>
    str_replace_all("\\bhwy\\.?\\b", " highway ") |>
    str_replace_all("\\bdept\\.?\\b", " department ") |>
    # "departement" is an ICE typo; "departmen" survives LEAIC's 50-char truncation
    str_replace_all("\\bdepartement\\b", " department ") |>
    str_replace_all("\\bdepartmen\\b", " department ") |>
    str_replace_all("\\bpd\\b", " police department ") |>
    str_replace_all("\\buniv\\.?\\b", " university ") |>
    str_replace_all("\\b(sheriff|constable|marshal)'?s?\\b", "\\1 ") |>
    # fuse, or norm_key collapses "Arkansas Department of Public Safety" onto
    # "Arkansas City PD"
    str_replace_all("\\bpublic safety\\b", " publicsafety ")
}

# aggressive key: drops jurisdiction-type and filler words
norm_ori_agency <- function(x) {
  x |>
    expand_leaic_abbrev() |>
    norm_key()
}

# keeps every word, so "Melbourne PD" and "Melbourne Village PD" stay distinct
norm_ori_fullname <- function(x) {
  x |>
    expand_leaic_abbrev() |>
    str_replace_all("[^a-z0-9]", "")
}

# parish -> county, then drop the type word from both sides; keep "city" so
# Virginia independent cities stay distinct from namesake counties
norm_county <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("'", "") |>
    str_replace_all("\\bste\\.?\\b", "sainte") |>
    str_replace_all("\\bst\\.?\\b", "saint") |>
    str_replace_all("\\bparish\\b", "county") |>
    str_replace_all("\\bcounty\\b", " ") |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish()
}

extract_city_guess <- function(x) {
  s <- str_squish(x)
  s <- str_remove(s, regex("(?i)^\\s*(city|town|village)\\s+of\\s+"))
  s <- str_remove(
    s,
    regex(
      "(?i)\\b(police|pd|police dept\\.?|police department|department|dept|public safety|office|marshal['’]?s?)\\b.*$"
    )
  )
  s <- str_squish(s)
  s <- na_if(s, "")
  str_to_title(s)
}

extract_facility_guess <- function(x) {
  s <- str_squish(x)

  s <- str_replace(
    s,
    regex("(?i)^(.+?)\\s+Sheriff['’]?s\\s+Office$"),
    "\\1 Jail"
  )

  s <- str_replace(
    s,
    regex("(?i)^(.+?)\\s+Police\\s+Department$"),
    "\\1 City Jail"
  )

  s <- str_replace(
    s,
    regex(
      "(?i)^(.+?)\\s+Board\\s+of\\s+County\\s+Commissioners\\s*/?\\s*(Department\\s+of\\s+Corrections|Detention\\s+Facility|Corrections)?$"
    ),
    "\\1 Jail"
  )

  s <- str_replace(
    s,
    regex("(?i)^(.+?)\\s+Department\\s+of\\s+Corrections$"),
    "\\1 Department of Corrections"
  )

  s <- str_replace_all(
    s,
    regex("(?i)corrections department"),
    "Department of Corrections"
  )

  s |>
    str_squish() |>
    str_to_title()
}

norm_match_phrase <- function(x) {
  x |>
    # fold curly apostrophes BEFORE stripping punctuation, or "St. John’s"
    # splits into "john s" while "St. John's" yields "johns"
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("\\bst\\.?\\b", "saint") |>
    str_replace_all("'", "") |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish() |>
    str_replace_all("\\s+", " ")
}

exact_scope_root <- function(x) {
  x |>
    norm_match_phrase() |>
    str_replace_all(
      "\\b(county|parish|city|town|village|borough|township|municipality)\\b",
      " "
    ) |>
    str_squish() |>
    str_replace_all("\\s+", " ")
}

exact_county_suffix_pattern <- function() {
  paste(
    "sheriffs? office",
    "sheriffs? department",
    # possessives too: "Culberson County Sheriff's" names the jail
    "sheriffs?",
    "county jail",
    "parish jail",
    "jail",
    "detention center",
    "detention facility",
    "adult detention center",
    "adult detention facility",
    "correctional facility",
    "correctional center",
    "correctional institution",
    "correctional complex",
    "law enforcement center",
    "justice center",
    "public safety complex",
    sep = "|"
  )
}

is_exact_county_pattern <- function(name, county) {
  root <- exact_scope_root(county)
  phrase <- norm_match_phrase(name)
  suffixes <- exact_county_suffix_pattern()

  !is.na(root) &
    root != "" &
    str_detect(
      phrase,
      paste0("^", root, "\\s+(county|parish)\\s+(", suffixes, ")\\b")
    )
}

is_exact_municipal_pattern <- function(name, city) {
  root <- exact_scope_root(city)
  phrase <- norm_match_phrase(name)
  suffixes <- paste(
    "city jail",
    "jail",
    "police department",
    "police dept",
    "pd",
    sep = "|"
  )

  !is.na(root) &
    root != "" &
    str_detect(
      phrase,
      paste0("^", root, "\\s+(", suffixes, ")$")
    )
}

extract_university_guess <- function(x) {
  s <- str_squish(x)

  s <- str_remove(
    s,
    regex("(?i)^\\s*(district\\s+)?board\\s+of\\s+trustees\\s+of\\s+")
  )

  s <- str_remove(
    s,
    regex("(?i)\\s+board\\s+of\\s+trustees\\s*$")
  )

  s <- str_remove(
    s,
    regex(
      "(?i)\\s+((campus\\s+)?police(\\s+department)?|pd|department\\s+of\\s+public\\s+safety|public\\s+safety|security)\\s*$"
    )
  )

  s |>
    str_remove(regex("(?i)^\\s*the\\s+")) |>
    str_squish() |>
    str_to_title()
}

pa_constable_ordinal_number <- function(x) {
  s <- str_to_lower(str_squish(as.character(x)))
  dplyr::case_when(
    str_detect(s, "^[0-9]+") ~ as.integer(str_extract(s, "^[0-9]+")),
    s %in% c("first", "one") ~ 1L,
    s %in% c("second", "two") ~ 2L,
    s %in% c("third", "three") ~ 3L,
    s %in% c("fourth", "four") ~ 4L,
    s %in% c("fifth", "five") ~ 5L,
    s %in% c("sixth", "six") ~ 6L,
    s %in% c("seventh", "seven") ~ 7L,
    s %in% c("eighth", "eight") ~ 8L,
    s %in% c("ninth", "nine") ~ 9L,
    s %in% c("tenth", "ten") ~ 10L,
    TRUE ~ NA_integer_
  )
}

pa_constable_clean_municipality <- function(x) {
  x |>
    str_squish() |>
    str_replace_all(regex("\\btwp\\.?\\b", ignore_case = TRUE), "Township") |>
    str_replace_all(regex("\\bboro\\.?\\b", ignore_case = TRUE), "Borough") |>
    str_replace_all(regex("\\bSo\\.?\\b", ignore_case = TRUE), "South") |>
    str_replace_all(
      regex("\\bSouthhampton\\b", ignore_case = TRUE),
      "Southampton"
    ) |>
    str_replace_all(
      regex("\\bEast Pennsylvania Township\\b", ignore_case = TRUE),
      "East Pennsboro Township"
    ) |>
    str_replace_all(regex("\\bCumberland City\\b", ignore_case = TRUE), "") |>
    str_squish() |>
    str_to_title()
}

extract_pa_constable_parts <- function(x) {
  purrr::map_dfr(as.character(x), function(agency) {
    s <- str_squish(agency)

    ward_token <- str_match(
      s,
      regex(
        "\\b([0-9]+(?:st|nd|rd|th)?|first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth)\\s+ward\\b",
        ignore_case = TRUE
      )
    )[, 2]
    ward_number <- pa_constable_ordinal_number(ward_token)

    precinct_token <- str_match(
      s,
      regex("\\b(?:precinct|pct\\.?)[[:space:]]*([0-9]+)", ignore_case = TRUE)
    )[, 2]
    precinct_number <- pa_constable_ordinal_number(precinct_token)

    municipality_type_hint <- dplyr::case_when(
      str_detect(s, regex("\\b(township|twp\\.?)\\b", ignore_case = TRUE)) ~
        "township",
      str_detect(s, regex("\\b(borough|boro\\.?)\\b", ignore_case = TRUE)) ~
        "borough",
      str_detect(s, regex("\\bcity\\b", ignore_case = TRUE)) ~ "city",
      TRUE ~ NA_character_
    )

    municipality_guess <- s |>
      str_remove(regex(
        "^\\s*Pennsylvania\\s+State\\s+Constable'?s?\\s+Office,?\\s*",
        ignore_case = TRUE
      )) |>
      str_remove(regex("\\bPA\\s+State\\s+Constable\\b", ignore_case = TRUE)) |>
      str_remove(regex("\\bConstable'?s?\\s+Office\\b", ignore_case = TRUE)) |>
      str_remove(regex("\\bConstables\\s+Office\\b", ignore_case = TRUE)) |>
      str_remove(regex("\\bConstable\\b", ignore_case = TRUE)) |>
      str_remove(regex(
        "\\b([0-9]+(?:st|nd|rd|th)?|first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth)\\s+ward\\b",
        ignore_case = TRUE
      )) |>
      str_remove(regex(
        "\\b(?:precinct|pct\\.?)[[:space:]]*[0-9]+\\b",
        ignore_case = TRUE
      )) |>
      str_remove(regex("\\bOffice\\b", ignore_case = TRUE)) |>
      str_replace_all(",", " ") |>
      pa_constable_clean_municipality()

    tibble::tibble(
      municipality_guess = municipality_guess,
      municipality_type_hint = municipality_type_hint,
      ward_number = ward_number,
      precinct_number = precinct_number,
      pa_constable_jurisdiction = dplyr::case_when(
        !is.na(ward_number) ~ "ward",
        !is.na(precinct_number) ~ "precinct",
        TRUE ~ "municipality"
      )
    )
  })
}

# observation time from the path: four filename conventions, ours and ICE's
appearance_seen_at <- function(paths) {
  folder <- str_match(paths, "sheets_(\\d{8})_(\\d{6})")
  t_folder <- as.POSIXct(
    paste0(folder[, 2], folder[, 3]),
    format = "%Y%m%d%H%M%S", tz = "UTC"
  )
  html <- str_match(basename(paths), "^html_(\\d{8})")
  t_html <- as.POSIXct(html[, 2], format = "%Y%m%d", tz = "UTC")
  ice <- str_match(
    basename(paths),
    regex("^participatingAgencies(\\d{2})(\\d{2})(\\d{4})", ignore_case = TRUE)
  )
  t_ice <- as.POSIXct(
    paste0(ice[, 4], ice[, 2], ice[, 3]),
    format = "%Y%m%d", tz = "UTC"
  )
  iso <- str_match(
    basename(paths),
    regex("^participatingAgencies_(\\d{8})", ignore_case = TRUE)
  )
  t_iso <- as.POSIXct(iso[, 2], format = "%Y%m%d", tz = "UTC")
  out <- coalesce(t_folder, t_html, t_ice, t_iso)
  # ICE typos years in filenames too ("...03113025am" is year 3025); drop an
  # impossible future time rather than let it poison last_appeared
  out[!is.na(out) & out > Sys.time() + 30 * 86400] <- NA
  out
}

# SIGNED arrives as raw text because ICE mixes native dates, text dates and
# typo'd text dates in one column; letting readxl guess drops the typo'd rows
coerce_signed_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(round(x), origin = "1899-12-30"))
  chr <- str_squish(as.character(x))
  # repair ICE's five-digit years ("5/31/20026") before parsing
  chr <- str_replace(chr, "/200(\\d{2})$", "/20\\1")
  # window-check per format so a near-parse cannot shadow the right one; base
  # as.Date, not mdy, which trains on the whole vector and NAs it all
  in_window <- function(d) {
    d[!is.na(d) & (d < as.Date("2000-01-01") | d > Sys.Date() + 365)] <- NA
    d
  }
  serial <- suppressWarnings(as.numeric(chr))
  serial[!is.na(serial) & (serial < 20000 | serial > 60000)] <- NA
  coalesce(
    in_window(as.Date(round(serial), origin = "1899-12-30")),
    in_window(as.Date(chr, format = "%m/%d/%Y")),
    in_window(as.Date(chr, format = "%Y-%m-%d")),
    in_window(as.Date(chr, format = "%m/%d/%y"))
  )
}

# One archived sheet -> observation rows; TYPE/COUNTY/MOA are optional because
# the 2022-2024 page-table era carried fewer columns
read_appearance_rows <- function(path) {
  tryCatch(
    {
      tabs <- readxl::excel_sheets(path)
      # 2021-era workbooks name the data tab "Data"; modern files "Sheet1"
      tab <- grep("^data$|^sheet", tabs, ignore.case = TRUE, value = TRUE)[1]
      if (is.na(tab)) tab <- tabs[1]
      # text, so coerce_signed_date sees raw cells instead of readxl's NAs
      d <- readxl::read_excel(
        path,
        sheet = tab,
        col_types = "text",
        progress = FALSE
      )
      names(d) <- str_squish(str_to_upper(names(d)))
      need <- c("STATE", "LAW ENFORCEMENT AGENCY", "SUPPORT TYPE", "SIGNED")
      if (!all(need %in% names(d))) return(NULL)
      opt <- function(col) {
        if (col %in% names(d)) as.character(d[[col]]) else NA_character_
      }
      tibble(
        raw_state = as.character(d[["STATE"]]),
        raw_agency = as.character(d[["LAW ENFORCEMENT AGENCY"]]),
        raw_support = as.character(d[["SUPPORT TYPE"]]),
        raw_type = opt("TYPE"),
        raw_county = opt("COUNTY"),
        raw_moa = opt("MOA"),
        signed = coerce_signed_date(d[["SIGNED"]])
      ) |>
        filter(!is.na(raw_state), !is.na(raw_agency), !is.na(signed))
    },
    error = function(e) NULL
  )
}

# identity key normalization shared by the history scan and 1-read's joins
appearance_norm <- function(x) {
  str_squish(str_to_upper(str_replace_all(x, "[’‘]", "'")))
}

# map ICE's historical model names onto the modern ones, or an agreement that
# bridged the eras reads as a false 2017 removal; keys only, never display
norm_support_key <- function(x) {
  k <- appearance_norm(x)
  case_when(
    k == "JAIL ENFORCEMENT" ~ "JAIL ENFORCEMENT MODEL",
    k == "TASK FORCE" ~ "TASK FORCE MODEL",
    TRUE ~ k
  )
}

# one manifest.csv per snapshot folder; 0-287g-build-manifests.R backfills legacy ones
manifest_columns <- c(
  "saved_path", "file_hash", "url", "retrieved_at",
  "state", "agency", "original_filename", "note"
)

append_manifest <- function(folder, rows) {
  path <- file.path(folder, "manifest.csv")
  for (col in manifest_columns) {
    if (!col %in% names(rows)) rows[[col]] <- NA_character_
  }
  rows <- rows[manifest_columns]
  if (file.exists(path)) {
    existing <- readr::read_csv(
      path,
      col_types = readr::cols(.default = "c")
    )
    rows <- dplyr::bind_rows(existing, rows) |>
      dplyr::distinct(saved_path, .keep_all = TRUE)
  }
  readr::write_csv(rows, path, na = "")
}

# cross-era agency key for partnership-level joins: collapses abbreviation and
# punctuation variants
norm_agency <- function(x) {
  x |> tolower() |>
    str_replace_all("&", "and") |>
    str_replace_all("\\bdept\\b\\.?", "department") |>
    str_replace_all("\\bco\\b\\.?", "county") |>
    str_replace_all("\\bso\\b", "sheriffs office") |>
    str_replace_all("\\bpd\\b", "police department") |>
    str_replace_all("sheriff'?s? office", "sheriffs office") |>
    str_replace_all("department of corrections?", "department of corrections") |>
    str_replace_all("corrections department", "department of corrections") |>
    str_replace_all("^city of ", "") |>
    str_replace_all("[^a-z0-9]", "")
}
