# Source claims: every non-sheet source is reduced to typed claims about a
# agency (listed, pending, signed, model, moa_file, rescinded), and each
# claim resolves to an agency by rule or is left visible as unresolved.

claim_columns <- c("source_id", "agency_id", "resolution", "state", "agency_raw",
                   "field", "value", "value_date", "as_of", "evidence")

read_sources <- function(path = "inputs/historical/source-registry.csv") {
  read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE) |>
    mutate(as_of = as.Date(as_of))
}

# old-style and postal abbreviations a report may print for a state
state_abbreviations <- c(
  setNames(state.name, state.abb),
  Ala = "Alabama", Ariz = "Arizona", Ark = "Arkansas", Calif = "California", Colo = "Colorado",
  Conn = "Connecticut", Del = "Delaware", Fla = "Florida", Ga = "Georgia", Ill = "Illinois", Ind = "Indiana",
  Kan = "Kansas", Ky = "Kentucky", La = "Louisiana", Mass = "Massachusetts", Md = "Maryland", Mich = "Michigan",
  Minn = "Minnesota", Miss = "Mississippi", Mo = "Missouri", Mont = "Montana", Neb = "Nebraska", Nev = "Nevada",
  Okla = "Oklahoma", Ore = "Oregon", Pa = "Pennsylvania", Tenn = "Tennessee", Tex = "Texas", Va = "Virginia",
  Vt = "Vermont", Wash = "Washington", Wis = "Wisconsin", Wyo = "Wyoming",
  NC = "North Carolina", SC = "South Carolina", NH = "New Hampshire", NM = "New Mexico", NJ = "New Jersey",
  NY = "New York", ND = "North Dakota", SD = "South Dakota", WV = "West Virginia", RI = "Rhode Island", DC = "District of Columbia"
)
state_from_token <- function(token, valid_states) {
  t <- str_squish(str_remove_all(coalesce(token, ""), "\\."))
  abbr <- state_abbreviations[str_replace_all(t, "\\s", "")]
  abbr <- coalesce(abbr, state_abbreviations[str_to_upper(str_replace_all(t, "\\s", ""))])
  full <- snap_state_name(str_to_title(t), valid_states)
  out <- coalesce(unname(abbr), if_else(full %in% valid_states, full, NA_character_))
  if_else(nzchar(t), out, NA_character_)
}

# "Alamance County Sheriff's Office (North Carolina)", "Barnstable County,
# Massachusetts", "Morristown, NJ", "Manassas Park, City of" and "Alabama
# Department of Public Safety" all carry their state in the name; the state
# column wins when set, and a marker leaves the name either way
split_state_marker <- function(agency, state, valid_states) {
  agency <- str_squish(coalesce(agency, ""))
  # ", City of" / ", State of" is a library-catalogue inversion of "City of X"
  inverted <- str_match(agency, "^(.*?),\\s*((City|Town|County|State) of)$")
  agency <- if_else(!is.na(inverted[, 1]), paste(inverted[, 3], inverted[, 2]), agency)
  paren <- str_match(agency, "\\(([A-Za-z .]+)\\)")[, 2]
  comma <- str_match(agency, ",\\s*([A-Za-z .]+)$")[, 2]
  from_paren <- state_from_token(paren, valid_states)
  from_comma <- state_from_token(comma, valid_states)
  lead <- str_match(agency, sprintf("^(%s)\\b", paste(valid_states, collapse = "|")))[, 2]
  found <- coalesce(from_paren, from_comma, lead)
  cleaned <- agency
  cleaned <- if_else(!is.na(from_paren), str_remove(cleaned, "\\s*\\([A-Za-z .]+\\)"), cleaned)
  cleaned <- if_else(!is.na(from_comma) & is.na(from_paren), str_remove(cleaned, ",\\s*[A-Za-z .]+$"), cleaned)
  tibble(state = coalesce(state, found), agency = str_squish(cleaned))
}

# a name that is a url fragment, a hyphen-truncated line or a footnote cannot
# resolve; a single-word place name may resolve by containment but never mints
validate_agency_shape <- function(agency) {
  a <- coalesce(agency, "")
  !(str_detect(a, "https?://") | str_detect(a, "-$") | str_detect(a, "<U\\+|\\*|†") |
      str_count(a, "[A-Za-z]{2,}") < 1)
}

# the registry of keys that name a published agency: its canonical spelling,
# every spelling ICE printed for it, and every alias verdict
agency_registry <- function(agencies, spellings, aliases) {
  bind_rows(
    agencies |> transmute(state_key, key = canonical_agency, agency_id, tier = "canonical"),
    spellings |> filter(n_agencies == 1) |>
      distinct(state_key, key = observed_agency, agency_id) |> mutate(tier = "spelling"),
    aliases |> filter(relation == "same") |>
      inner_join(agencies |> select(state_key, target_key = canonical_agency, agency_id),
                        by = c("state_key", "target_key")) |>
      distinct(state_key, key = alias_key, agency_id) |> mutate(tier = "alias")
  ) |>
    arrange(state_key, key, match(tier, c("canonical", "alias", "spelling"))) |>
    distinct(state_key, key, .keep_all = TRUE)
}

# resolve (state, agency[, signed]) rows to agency ids, one row in, one row
# out, by tiers: the registry within the state, a name that starts exactly one
# agency in the state, a name unique across the country, a signing date
# unique in the state, and, for a source that may mint, a new agency
resolve_agency <- function(state, agency, may_mint, registry, agencies, identities, xwalk, aliases,
                                signed = as.Date(rep(NA, length(agency)))) {
  rows <- tibble(state, agency, may_mint, signed) |>
    mutate(.row = row_number(),
                  state_full = snap_state_name(str_to_title(str_squish(coalesce(state, ""))), xwalk$state_full),
                  state_full = if_else(state_full %in% xwalk$state_full, state_full, NA_character_))
  rows <- bind_cols(
    rows |> select(-state),
    split_state_marker(rows$agency, rows$state_full, xwalk$state_full) |>
      mutate(state = coalesce(state, rows$state_full)) |>
      select(state, agency_clean = agency)
  ) |>
    left_join(xwalk |> select(state = state_full, state_abbr), by = "state") |>
    mutate(state_key = norm_state(state),
                  shape_ok = validate_agency_shape(agency_clean),
                  key = canonical_agency(agency_clean, coalesce(state_abbr, ""), coalesce(state, ""))) |>
    left_join(registry, by = c("state_key", "key"))
  # an alias may turn a bare place name into an agency name, so mintability is
  # judged on the key the alias table leaves
  same <- aliases |> filter(relation == "same") |> distinct(state_key, alias_key, target_key)
  rows <- rows |>
    left_join(same, by = c("state_key", "key" = "alias_key")) |>
    mutate(mint_key = coalesce(target_key, key),
                  mintable = shape_ok & str_count(mint_key, "\\S+") >= 2) |>
    select(-target_key)

  # a name that begins exactly one agency's canonical name in the state, or
  # failing that exactly one in the country
  prefix_all <- rows |>
    filter(is.na(agency_id), shape_ok, nchar(key) >= 4) |>
    select(.row, state_key, key) |>
    cross_join(agencies |> select(cand_state = state_key, cand_key = canonical_agency, cand_id = agency_id)) |>
    filter(str_starts(cand_key, paste0(key, "( |$)")))
  prefix_hits <- prefix_all |>
    summarise(
      prefix_id = if_else(n_distinct(cand_id[cand_state == state_key]) == 1,
                                 dplyr::first(cand_id[cand_state == state_key]), NA_character_),
      prefix_ambiguous = n_distinct(cand_id[cand_state == state_key]) > 1,
      prefix_national_id = if_else(n_distinct(cand_id) == 1, dplyr::first(cand_id), NA_character_),
      .by = .row)
  # a name unique across every state, or a name and signing date unique across
  # every state: the source printed the wrong state
  national <- registry |>
    distinct(key, agency_id) |>
    add_count(key, name = "n_states") |>
    filter(n_states == 1) |>
    select(key, national_id = agency_id)
  national_dated <- identities |>
    distinct(key = canonical_agency, signed, national_date_id = agency_id) |>
    add_count(key, signed, name = "n") |>
    filter(n == 1) |>
    select(key, signed, national_date_id)
  # a signing date unique in the state
  dated <- identities |>
    distinct(state_key, signed, date_id = agency_id) |>
    add_count(state_key, signed, name = "n_dated") |>
    filter(n_dated == 1) |>
    select(state_key, signed, date_id)

  rows |>
    left_join(prefix_hits, by = ".row") |>
    left_join(national, by = "key") |>
    left_join(national_dated, by = c("key", "signed")) |>
    left_join(dated, by = c("state_key", "signed")) |>
    mutate(
      resolution = case_when(
        !shape_ok ~ "unresolved_shape",
        !is.na(tier) ~ tier,
        !is.na(prefix_id) ~ "prefix_containment",
        !is.na(national_id) | !is.na(prefix_national_id) ~ "name_national",
        !is.na(national_date_id) ~ "name_date_national",
        !is.na(date_id) ~ "date_join",
        prefix_ambiguous ~ "unresolved_ambiguous",
        is.na(state) ~ "unresolved_state",
        mintable & may_mint ~ "minted",
        TRUE ~ "unresolved_absent"
      ),
      agency_id = case_when(
        resolution == "prefix_containment" ~ prefix_id,
        resolution == "name_national" ~ coalesce(national_id, prefix_national_id),
        resolution == "name_date_national" ~ national_date_id,
        resolution == "date_join" ~ date_id,
        resolution == "minted" ~ agency_id_of(state, agency_clean, state_abbr, aliases),
        TRUE ~ agency_id
      )
    ) |>
    arrange(.row) |>
    select(agency_id, resolution, state, agency_clean)
}
