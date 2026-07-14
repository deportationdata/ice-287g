# readxl drops embedded hyperlinks, so pull them out of the xlsx internals:
# each linked cell carries a relationship id that maps to the target url in
# the sheet's .rels file. Returns one row per linked cell: col (letter),
# row (excel row, header = row 1), url.
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

# attach each sheet row's MOA hyperlink as moa_link (NA where the row has
# none, e.g. pending agreements)
add_moa_links <- function(x, path) {
  moa_col <- LETTERS[match("MOA", names(x))]

  if (is.na(moa_col)) {
    stop("Participating agencies sheet has no MOA column; layout changed?")
  }

  x |>
    mutate(excel_row = row_number() + 1) |> # sheet rows sit one below the header row
    left_join(
      xlsx_hyperlinks(path) |>
        filter(col == moa_col) |>
        transmute(excel_row = row, moa_link = clean_moa_urls(url)),
      by = "excel_row"
    ) |>
    select(-excel_row)
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
    str_replace_all("[^a-z0-9]", "")
}

read_sf_parquet <- function(path) {
  sfarrow::st_read_parquet(path)
}

write_sf_parquet <- function(x, path) {
  sfarrow::st_write_parquet(x, path)
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

# collapse a set of optional flags into a "; "-separated string, dropping
# NA/empty entries (used by the facility and QA review sheets)
flag_list <- function(...) {
  flags <- c(...)
  flags <- flags[!is.na(flags) & flags != ""]
  paste(flags, collapse = "; ")
}

# PA constables are routed to 3-make-pa-constable-sf.R and must be excluded
# from the municipal layer; keep the predicate in one place
is_pa_constable_agency <- function(state, agency) {
  state == "Pennsylvania" &
    str_detect(str_to_lower(agency), "\\bconstables?\\b")
}

norm_state <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z]", "")
}

# snap a malformed state name to the closed 56-name vocabulary ("New
# Hamsphire" -> "New Hampshire"); only fires when the name is not already
# valid, the best candidate is unique, and the distance is tiny
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
    str_replace_all(
      "\\b(county|city|town|village|borough|township|municipality)\\b",
      " "
    ) |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish()
}

# county key for the ORI crosswalk join: LEAIC county names drop the
# "Parish" suffix that the ICE sheets carry ("BEAUREGARD" vs "Beauregard
# Parish"), so parish is stripped alongside norm_place's county/city/...
# list; "#N/A"-style sentinels in the ICE sheets become NA so they never
# key-match anything
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

# fuzzy-ORI guard: precinct/ward/troop numbers must agree exactly — a fuzzy
# hit that turns "Constable Precinct 3" into "CONSTABLE PCT. 1" is a
# different agency with its own ORI
key_digits <- function(x) {
  vapply(str_extract_all(x, "[0-9]+"), paste, character(1), collapse = "-")
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

# for agreements whose exact key join found no polygon: score all candidates
# in the same state's lookup by Jaro-Winkler distance on the normalized key
# (mirrors the stringdist cascade in 6-make-facility-sf.R), rank by distance
# then same-county preference then src_rank, and keep the top 3 per agreement
# with the diagnostics accept/suggest decisions need
fuzzy_polygon_candidates <- function(unmatched, lookup, max_dist) {
  lookup_tbl <- lookup |>
    sf::st_drop_geometry() |>
    rename(cand_match_key = match_key)

  unmatched |>
    as_tibble() |>
    select(.row_id, state_key, match_key, county_key_agency) |>
    filter(!is.na(match_key), match_key != "") |>
    inner_join(lookup_tbl, by = "state_key", relationship = "many-to-many") |>
    mutate(
      match_dist = stringdist::stringdist(
        match_key,
        cand_match_key,
        method = "jw",
        p = 0.1
      )
    ) |>
    filter(!is.na(match_dist), match_dist <= max_dist) |>
    group_by(.row_id) |>
    mutate(
      n_within_threshold = n_distinct(geoid),
      county_pref = if_else(
        !is.na(cand_county_key) &
          !is.na(county_key_agency) &
          cand_county_key == county_key_agency,
        0L,
        1L
      ),
      unique_best = n_distinct(geoid[match_dist == min(match_dist)]) == 1
    ) |>
    arrange(match_dist, county_pref, src_rank, geoid, .by_group = TRUE) |>
    # the places lookup carries one row per intersecting county; keep the
    # best-ranked row per candidate polygon
    distinct(geoid, .keep_all = TRUE) |>
    slice_head(n = 3) |>
    mutate(cand_rank = row_number()) |>
    ungroup()
}

extract_city_guess <- function(x) {
  s <- str_squish(x)
  s <- str_remove(s, regex("(?i)^\\s*city\\s+of\\s+"))
  s <- str_remove(
    s,
    regex(
      "(?i)\\b(police|pd|police dept\\.?|police department|department|dept|public safety|office)\\b.*$"
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
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("\\bst\\.?\\b", "saint") |>
    str_replace_all("'", "") |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish()
}

exact_scope_root <- function(x) {
  x |>
    norm_match_phrase() |>
    str_replace_all(
      "\\b(county|parish|city|town|village|borough|township|municipality)\\b",
      " "
    ) |>
    str_squish()
}

exact_county_suffix_pattern <- function() {
  paste(
    "sheriffs? office",
    "sheriffs? department",
    "sheriffs? jail",
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
