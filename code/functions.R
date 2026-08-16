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
    transmute(
      col = str_extract(ref, "^[A-Z]+"),
      row = as.integer(str_extract(ref, "\\d+$")),
      url
    )
}

# MOA links are hand-pasted into the sheet, so unwrap Outlook safelinks
# wrappers and fix the pasted-URL typos seen so far (chrome-extension
# prefixes, https:/ with one slash, http) to recover the real ice.gov url
clean_moa_urls <- function(url) {
  url |>
    map_chr(\(u) {
      if (str_detect(u, "safelinks\\.protection\\.outlook\\.com")) {
        URLdecode(str_extract(u, "(?<=[?&]url=)[^&]+"))
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

norm_key <- function(x) {
  x |>
    # deal with ~ and accents and curly apostrophes correctly
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

normalize_agencies_all <- function(x) {
  if (!"agency" %in% names(x) && "LAW ENFORCEMENT AGENCY" %in% names(x)) {
    x <- x |>
      rename(agency = `LAW ENFORCEMENT AGENCY`)
  }

  if (!"support_type" %in% names(x) && "SUPPORT TYPE" %in% names(x)) {
    x <- x |>
      mutate(support_type = `SUPPORT TYPE`)
  }

  for (col in c(
    "ORI9",
    "FSTATE",
    "FCOUNTY",
    "FPLACE",
    "leaic_name",
    "leaic_match_type",
    "crime_ori",
    "crime_agency_name",
    "crime_match_type",
    "ori_source"
  )) {
    if (!col %in% names(x)) {
      x[[col]] <- NA
    }
  }

  x
}

norm_state <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z]", "") |>
    str_squish()
}

# apostrophes are deleted (not spaced) so "Jackson's Gap" keys as
# "jacksons gap"; St./Ste. are spelled out before the period is stripped so
# "St. Ann" and "Saint Ann" share a key (ste must run first: \bst\b does not
# match inside "ste")
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
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish()
}

# county key for the ORI crosswalk and facility joins: LEAIC/HIFLD county
# names drop the "Parish" suffix that the ICE sheets carry ("BEAUREGARD" vs
# "Beauregard Parish"), so parish is stripped alongside norm_place's
# county/city/... list; "#N/A"-style sentinels in the ICE sheets become NA
# so they never key-match anything
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

# LEAIC/NCIC names abbreviate heavily ("CONSTABLE PCT. 3", "NORTHERN YORK
# CO. REGIONAL", "BESSEMER BORO"), so expand before keying; transliteration
# runs first so curly-apostrophe possessives ("Sheriff’s") hit the
# singularization, which makes sheriff's/sheriffs/sheriff share a key
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
    # "departement" is an ICE-sheet typo; "departmen" is what survives
    # LEAIC's 50-character name truncation
    str_replace_all("\\bdepartement\\b", " department ") |>
    str_replace_all("\\bdepartmen\\b", " department ") |>
    str_replace_all("\\bpd\\b", " police department ") |>
    str_replace_all("\\buniv\\.?\\b", " university ") |>
    str_replace_all("\\b(sheriff|constable|marshal)'?s?\\b", "\\1 ") |>
    # fuse so norm_key does not strip the words separately: "Department of
    # Public Safety" must not collapse to the bare place/state name, which
    # is how "Arkansas Department of Public Safety" would otherwise key
    # identically to "Arkansas City Police Department"
    str_replace_all("\\bpublic safety\\b", " publicsafety ")
}

# agency key for the ORI crosswalk join: aggressive — drops the
# jurisdiction-type and police/department filler words via norm_key
norm_ori_agency <- function(x) {
  x |>
    expand_leaic_abbrev() |>
    norm_key()
}

# full-name key: same abbreviation expansion but keeps every word, so
# "Melbourne Police Department" and "Melbourne Village Police Department"
# stay distinct where norm_ori_agency collapses both to "melbourne"
norm_ori_fullname <- function(x) {
  x |>
    expand_leaic_abbrev() |>
    str_replace_all("[^a-z0-9]", "")
}

# parish -> county makes ICE's "Richland County" match Louisiana's "Richland
# Parish"; "city" is deliberately untouched so Virginia independent cities
# (Fairfax city) stay distinct from their namesake counties
norm_county <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("'", "") |>
    str_replace_all("\\bste\\.?\\b", "sainte") |>
    str_replace_all("\\bst\\.?\\b", "saint") |>
    str_replace_all("\\bparish\\b", "county") |>
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
    regex("(?i)^(.+?)\\s+Sheriff'?s\\s+Office$"),
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
    "sheriff",
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

  !is.na(root) & root != "" &
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

  !is.na(root) & root != "" &
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
    str_replace_all(regex("\\bSouthhampton\\b", ignore_case = TRUE), "Southampton") |>
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
      str_remove(regex("^\\s*Pennsylvania\\s+State\\s+Constable'?s?\\s+Office,?\\s*", ignore_case = TRUE)) |>
      str_remove(regex("\\bPA\\s+State\\s+Constable\\b", ignore_case = TRUE)) |>
      str_remove(regex("\\bConstable'?s?\\s+Office\\b", ignore_case = TRUE)) |>
      str_remove(regex("\\bConstables\\s+Office\\b", ignore_case = TRUE)) |>
      str_remove(regex("\\bConstable\\b", ignore_case = TRUE)) |>
      str_remove(regex(
        "\\b([0-9]+(?:st|nd|rd|th)?|first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth)\\s+ward\\b",
        ignore_case = TRUE
      )) |>
      str_remove(regex("\\b(?:precinct|pct\\.?)[[:space:]]*[0-9]+\\b", ignore_case = TRUE)) |>
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
