# Key normalization: agency, place, county, state, ORI and pattern helpers.

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
    str_replace_all("\\bpublic safety\\b", " publicsafety ") |>
    # styling that differs between ICE and the rosters (each tested over every agreement
    # and roster on 2026-09-13 with no wrong match): "Metro"/"Metropolitan" is a prefix
    # (Metropolitan Moore County Sheriff's Office is Moore County's), a state highway patrol
    # is its highway patrol, and a college's board of trustees signs for the college police
    str_replace_all("\\bmetro(politan)?\\b", " ") |>
    str_replace_all("\\bstate highway patrol\\b", " highway patrol ") |>
    str_replace_all("\\b(district )?board of trustees( of)?\\b", " ")
}

# aggressive key: drops jurisdiction-type and filler words
norm_ori_agency <- function(x) {
  x |>
    expand_leaic_abbrev() |>
    norm_key()
}

# the body that runs a jail, as the jails census names operators: the RSW Regional Jail
# Authority's jails carry the authority as operator, and "authority" is the styling
norm_operator_key <- function(x) {
  x |>
    str_to_lower() |>
    str_remove_all("\\bauthority\\b") |>
    norm_ori_agency()
}

# a town marshal's office is the town's police, except in Louisiana, whose city marshals
# are court officers distinct from the police department
marshal_as_police <- function(name, state_key) {
  if_else(state_key == "louisiana", name,
          str_replace_all(name, regex("\\bmarshal['’]?s?\\b", ignore_case = TRUE), "Police"))
}

# a town's department of public safety is its police
public_safety_as_police <- function(name, jurisdiction_level) {
  if_else(coalesce(jurisdiction_level == "Municipal", FALSE),
          str_replace(name, regex("\\b(department|division) of public safety\\b.*$", ignore_case = TRUE), "Police Department"),
          name)
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
      "(?i)\\b(police|pd|police dept\\.?|police department|department|dept|division|public safety|office|marshal['’]?s?)\\b.*$"
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
  case_when(
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
  map_dfr(as.character(x), function(agency) {
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

    municipality_type_hint <- case_when(
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

    tibble(
      municipality_guess = municipality_guess,
      municipality_type_hint = municipality_type_hint,
      ward_number = ward_number,
      precinct_number = precinct_number,
      pa_constable_jurisdiction = case_when(
        !is.na(ward_number) ~ "ward",
        !is.na(precinct_number) ~ "precinct",
        TRUE ~ "municipality"
      )
    )
  })
}

# ORIs are keyed across four rosters; only a well-formed one may ship
clean_ori <- function(x) {
  o <- str_to_upper(str_squish(x))
  if_else(str_detect(o, "^[A-Z]{2}[A-Z0-9]{7}$"), o, NA_character_)
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
    k %in% c("TASK FORCE", "TASK FORCE OFFICER") ~ "TASK FORCE MODEL",
    k %in% c("WARRANT SERVICES OFFICER", "WARRANT SERVICE OFFICE") ~ "WARRANT SERVICE OFFICER",
    TRUE ~ k
  )
}

# ---- identity family: folds spelling, never jurisdiction ---------------------

# a redundant own-state token carries no information inside a state-scoped key:
# "MO State Highway Patrol" is Missouri's, "Washington County Sheriff Office AR"
# is Arkansas's. A leading state NAME is dropped only before an agency-type word,
# so "Virginia Beach Police" and "Kansas City PD" keep their place names.
expand_own_state_token <- function(x, state_abbr, state_full) {
  # the names become regex, so a footnote-marked "DELAWARE**" must not break them
  ab <- str_remove_all(str_to_lower(state_abbr), "[^a-z]")
  full <- str_remove_all(str_to_lower(state_full), "[^a-z ]")
  x <- str_remove(x, paste0("\\s+(", ab, "|", full, ")$"))
  x <- str_remove(x, paste0(
    "^", full,
    "\\s+(?=(department|state|highway|office|bureau|division|attorney|corrections?|police|patrol|fish|game|wildlife)\\b)"
  ))
  str_squish(x)
}

canonical_agency <- function(agency, state_abbr, state_full) {
  agency |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_remove_all("'") |>
    # a leading own-state abbreviation goes first, or "CO Dept." reads as "county"
    str_replace(paste0("^", str_remove_all(str_to_lower(state_abbr), "[^a-z]"), "\\b"),
                str_remove_all(str_to_lower(state_full), "[^a-z ]")) |>
    str_replace_all("&", " and ") |>
    str_replace_all("\\bste\\.?\\b", " sainte ") |>
    str_replace_all("\\bst\\.?\\b", " saint ") |>
    str_replace_all("\\bft\\.?\\b", " fort ") |>
    str_replace_all("\\bmt\\.?\\b", " mount ") |>
    str_replace_all("\\b(co|cnty)\\.?\\b", " county ") |>
    str_replace_all("\\b(dept|departement|departmen)\\.?\\b", " department ") |>
    str_replace_all("\\bso\\b", " sheriffs office ") |>
    str_replace_all("\\bpd\\b", " police department ") |>
    str_replace_all("\\btwp\\.?\\b", " township ") |>
    str_replace_all("\\bboro\\.?\\b", " borough ") |>
    str_replace_all("\\bhwy\\.?\\b", " highway ") |>
    str_replace_all("\\buniv\\.?\\b", " university ") |>
    str_replace_all("\\bpct\\.?\\b", " precinct ") |>
    # ICE abbreviates a county commission and a public-safety department in a few rows
    str_replace_all("\\bbocc\\b", " board of county commissioners ") |>
    str_replace_all("\\bdps\\b", " department of public safety ") |>
    str_replace_all("[^a-z0-9]", " ") |>
    str_squish() |>
    expand_own_state_token(state_abbr, state_full) |>
    str_remove("^(city|town|village|borough|township|county) of ") |>
    str_replace_all("\\bsheriffs?\\b", "sheriff") |>
    str_replace_all("\\bmarshals?\\b", "marshal") |>
    str_replace_all("\\bconstables?\\b", "constable") |>
    str_replace_all("\\bservices\\b", "service") |>
    str_replace_all("\\bcorrections?\\b", "correction") |>
    # a county has one sheriff, so its "department" and its "office" are one agency
    str_replace_all("\\bsheriff department\\b", "sheriff office") |>
    str_replace_all("\\bcorrection department\\b", "department of correction") |>
    # a trailing addendum marker names a document, not an agency
    str_remove("\\s+(addendum|amendment)$") |>
    str_squish()
}

# exhaustive over the nine support strings ICE has ever printed
canonical_support <- function(x) norm_support_key(x)

support_abbr <- function(support_key) {
  case_when(
    support_key == "JAIL ENFORCEMENT MODEL" ~ "JEM",
    support_key == "TASK FORCE MODEL" ~ "TFM",
    support_key == "WARRANT SERVICE OFFICER" ~ "WSO",
    support_key == "JAIL & TASK FORCE" ~ "JTF",
    TRUE ~ slug(support_key)
  )
}

slug <- function(x) {
  str_to_lower(x) |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_remove_all("^-|-$")
}

# county keys: typed keeps the jurisdiction word (geometry joins need "Hopewell
# city" ≠ "Hopewell County"); bare is typed minus type words (roster joins), so
# the two can never disagree on anything else
county_key_typed <- function(x) {
  x <- if_else(str_to_lower(str_squish(x)) %in% c("#na", "#n/a", "na", "n/a", ""), NA_character_, x)
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_remove_all("'") |>
    str_replace_all("\\bste\\.?\\b", "sainte") |>
    str_replace_all("\\bst\\.?\\b", "saint") |>
    str_replace_all("\\bparish\\b", "county") |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish()
}

county_key_bare <- function(x) {
  county_key_typed(x) |>
    str_remove_all("\\b(county|city|town|village|borough|township|municipality)\\b") |>
    str_squish()
}

# the committed alias table, keyed the way the identity tiers key observations
read_agency_aliases <- function(xwalk, path = "inputs/agency-aliases.csv") {
  cols <- c("state", "alias_agency", "canonical_agency", "relation", "scope", "evidence", "note")
  aliases <- if (file.exists(path)) {
    read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE)
  } else {
    as_tibble(setNames(rep(list(character()), length(cols)), cols))
  }
  aliases |>
    left_join(xwalk |> select(state = state_full, state_abbr), by = "state") |>
    mutate(state_key = norm_state(state),
                  alias_key = canonical_agency(alias_agency, coalesce(state_abbr, ""), state),
                  target_key = if_else(relation == "same",
                                              canonical_agency(canonical_agency, coalesce(state_abbr, ""), state),
                                              NA_character_))
}

# ICE's sheets carried no TYPE column before 2025; the name says what the sheet would. A
# leading state name before an agency word wins (Utah County Sheriff's Office is a county's),
# then a county or parish in the name (Hudson County Department of Corrections is a county
# jail, not a state one), then the state-agency words; a public-safety department that does
# not open with its state's name is a town's (Jupiter Island Department of Public Safety)
agency_level_from_name <- function(agency, state) {
  a <- str_to_lower(agency)
  agency_word <- "(department|state|highway|office|bureau|division|attorney|corrections?|police|patrol|fish|game|wildlife|national guard|military)"
  case_when(
    str_detect(agency, "^[A-Z]{2} (State|Department|Dept|Highway|Bureau|Division)\\b") |
      str_detect(a, paste0("^", str_to_lower(state), "\\s+", agency_word, "\\b")) |
      str_detect(a, "^department of public safety\\b") ~ "state",
    # a sheriff is a county officer even where the name omits the county (Jacksonville Sheriff's
    # Office serves consolidated Duval County)
    str_detect(a, "\\b(county|parish|sheriff)\\b") ~ "county",
    str_detect(a, "\\b(state police|highway patrol|department of corrections?|department of safety|department of law enforcement|bureau of investigation|attorney general)\\b") ~ "state",
    str_detect(a, "\\b(police|marshal|constable|town|city|village|borough|township|public safety)\\b") ~ "municipal",
    TRUE ~ "unknown"
  )
}

# the eight jurisdiction levels, as published
JURISDICTION_LEVELS <- c("State", "County", "Municipal", "Regional", "Campus", "Port", "Constable District", "Judicial District")

# an educational institution's own police (a university, college or school district),
# whatever TYPE ICE gives it
is_campus_agency <- function(agency) {
  str_detect(str_to_lower(agency), "university|college|campus|board of trustees|\\bschools?\\b|\\bschool district\\b|\\bisd\\b")
}

# name rules for the levels the hand list otherwise supplies, each tested over every
# agency on 2026-09-13 with no false positive: a body several jurisdictions formed, a
# state office serving a multi-county judicial district, an airport or port authority's police
is_regional_agency <- function(agency) str_detect(str_to_lower(agency), "\\bregional\\b")
is_judicial_district_agency <- function(agency) {
  str_detect(str_to_lower(agency), "\\bjudicial\\b|\\bdistrict attorney\\b.*\\bdistrict\\b")
}
is_port_agency <- function(agency) str_detect(str_to_lower(agency), "\\bairport\\b|\\bport authority\\b")

# a constable whose office is a division of the county: a Texas justice precinct, or a
# Mississippi justice court district. Tennessee's constables are elected by district but
# hold county-wide jurisdiction, so they are county officers like a constable named for the
# county alone; Pennsylvania's serve a borough, township or ward and stay municipal
is_constable_district <- function(agency, state) {
  a <- str_to_lower(agency)
  str_detect(a, "\\bconstable") & state %in% c("Texas", "Mississippi") &
    str_detect(a, "\\b(precinct|pct|district|dist)\\b") &
    str_detect(a, "\\d|\\b(northern|southern|eastern|western)\\s+district\\b")
}
is_county_constable <- function(agency, state) {
  str_detect(str_to_lower(agency), "\\bconstable") & state != "Pennsylvania" & !is_constable_district(agency, state)
}

# the identity family's agency key for any (state, agency) string
agency_id_of <- function(state, agency, state_abbr, aliases) {
  key <- canonical_agency(agency, coalesce(state_abbr, ""), coalesce(state, ""))
  same <- aliases |> filter(relation == "same") |> distinct(state_key, alias_key, target_key)
  target <- same$target_key[match(paste(norm_state(state), key), paste(same$state_key, same$alias_key))]
  paste0(coalesce(state_abbr, "XX"), "-", slug(coalesce(target, key)))
}
