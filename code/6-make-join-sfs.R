library(tidyverse)
library(sf)
library(stringdist)
library(arrow)
library(sfarrow)

source("code/functions.R")

restore_agency_name <- function(x) {
  names(x)[names(x) == "LAW.ENFORCEMENT.AGENCY"] <- "LAW ENFORCEMENT AGENCY"
  x
}

state_agreements_sf <- sfarrow::st_read_parquet(
  "data/processed/state_agreements_sf.parquet"
) |>
  restore_agency_name()

county_agreements_sf <- sfarrow::st_read_parquet(
  "data/processed/county_agreements_sf.parquet"
) |>
  restore_agency_name()

municipal_agreements_sf <- sfarrow::st_read_parquet(
  "data/processed/municipal_agreements_sf.parquet"
) |>
  restore_agency_name()

university_agreements_sf <- sfarrow::st_read_parquet(
  "data/processed/university_agreements_sf.parquet"
) |>
  restore_agency_name()

facilities <- sfarrow::st_read_parquet("data/processed/facilities.parquet")

hifld_tbl <- arrow::read_parquet("data/processed/hifld_tbl.parquet")
# leaic_tbl <- arrow::read_parquet("data/processed/leaic_tbl.parquet")
# crime_lookup <- arrow::read_parquet("data/processed/crime_lookup.parquet")
agencies_all <- arrow::read_parquet("data/processed/agencies_all.parquet")
manual_points <- arrow::read_parquet("data/processed/manual_points.parquet") |>
  mutate(
    across(
      c(
        county,
        manual_address,
        manual_city,
        manual_county,
        manual_zip,
        manual_note
      ),
      as.character
    )
  )

# facility source tables -------------------------------------------------

facilities_tbl <- facilities |>
  st_drop_geometry() |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  transmute(
    source = "facilities",
    source_rank = 1L,
    detention_facility_code = as.character(detention_facility_code),
    facility_name = str_squish(name),
    facility_address = address,
    facility_city = str_squish(city),
    facility_county = str_to_title(str_squish(county)),
    facility_county_fips = as.character(county_fips_code),
    facility_state = str_to_title(str_squish(state)),
    facility_state_fips = as.character(state_fips_code),
    facility_zip = zip,
    facility_address_full = address_full,
    facility_latitude = latitude,
    facility_longitude = longitude,
    facility_field_office = field_office,
    geometry
  ) |>
  mutate(
    state_key = norm_state(facility_state),
    county_key = norm_place(facility_county),
    facility_key = norm_key(facility_name)
  )

hifld_facility_tbl <- hifld_tbl |>
  transmute(
    source = src_dataset,
    source_rank = case_when(
      src_dataset == "hifld" ~ 2L,
      src_dataset == "hifld_prisons" ~ 3L,
      src_dataset == "jails_prisons" ~ 4L,
      TRUE ~ 99L
    ),
    detention_facility_code = src_id,
    facility_name = src_name,
    facility_address = src_address,
    facility_city = src_city,
    facility_county = NA_character_,
    facility_county_fips = NA_character_,
    facility_state = src_state,
    facility_state_fips = NA_character_,
    facility_zip = src_zip,
    facility_address_full = NA_character_,
    facility_latitude = src_latitude,
    facility_longitude = src_longitude,
    facility_field_office = NA_character_,
    state_key,
    county_key,
    facility_key = agency_key_src
  )

facility_sources_exact <- bind_rows(
  facilities_tbl,
  hifld_facility_tbl
)

# 287(g) facility-model agencies -----------------------------------------

fac_287g <- agencies_all |>
  filter(geom_class == "facility_point") |>
  transmute(
    state,
    county,
    agency = `LAW ENFORCEMENT AGENCY`,
    agency_level,
    needs_review,
    support_clean,
    has_addendum,
    moa_pending
  ) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_place(county),
    agency_key = norm_key(agency),
    facility_guess = extract_facility_guess(agency),
    facility_guess_key = norm_key(facility_guess)
  )

# state DOC agencies need 1-to-many matching logic -----------------------

doc_pattern <- "department of corrections|correctional services|public safety & corrections|division of corrections|department of public safety"

fac_287g_doc <- fac_287g |>
  filter(
    str_detect(str_to_lower(agency), doc_pattern) & agency_level == "state"
  )

fac_287g_non_doc <- fac_287g |>
  filter(
    !(str_detect(str_to_lower(agency), doc_pattern) & agency_level == "state")
  )

# match detention facilities to facility datasets ------------------------

facility_exact_matches <- fac_287g_non_doc |>
  inner_join(
    facility_sources_exact,
    by = c("state_key", "county_key", "facility_guess_key" = "facility_key"),
    relationship = "many-to-many"
  ) |>
  mutate(
    match_type = "exact_state_county_facility_name",
    match_score = 1.0
  ) |>
  group_by(state, county, agency, support_clean) |>
  arrange(source_rank) |>
  slice_head(n = 1) |>
  ungroup()

facility_unmatched_after_exact <- fac_287g_non_doc |>
  anti_join(facility_exact_matches, by = c("state", "county", "agency"))

# use fuzzy string matching on facility names within same county ---------

facility_fuzzy_county <- facility_unmatched_after_exact |>
  inner_join(
    facility_sources_exact,
    by = c("state_key", "county_key"),
    relationship = "many-to-many"
  ) |>
  mutate(
    match_dist = stringdist(
      facility_guess_key,
      facility_key,
      method = "jw",
      p = 0.1
    )
  ) |>
  filter(match_dist <= 0.22) |>
  group_by(state, county, agency) |>
  arrange(match_dist, source_rank) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_type = "fuzzy_county_facility",
    match_score = 1 - match_dist,
    needs_review = TRUE
  )

facility_unmatched_after_fuzzy_county <- facility_unmatched_after_exact |>
  anti_join(facility_fuzzy_county, by = c("state", "county", "agency"))

# broader state-level fuzzy matching for remaining unmatched facilities ----

facility_fuzzy_state <- facility_unmatched_after_fuzzy_county |>
  inner_join(
    facility_sources_exact,
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  mutate(
    match_dist = stringdist(
      facility_guess_key,
      facility_key,
      method = "jw",
      p = 0.1
    )
  ) |>
  filter(match_dist <= 0.15) |>
  group_by(state, county, agency) |>
  arrange(match_dist, source_rank) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_type = "fuzzy_state_facility",
    match_score = 1 - match_dist,
    needs_review = TRUE
  )

# DOC: match to all facilities in state ----------------------------------

doc_facility_pattern <- paste(
  "correctional",
  "prison",
  "penitentiary",
  "detention",
  "work camp",
  "re-?entry",
  "work release",
  "work program",
  "transition(al)? center",
  "classification",
  "diagnostic",
  "treatment facility",
  sep = "|"
)

doc_exclude_pattern <- paste(
  "county jail",
  "parish jail",
  "city jail",
  "municipal jail",
  "sheriff",
  "police",
  "courthouse",
  "regional lock-?up",
  sep = "|"
)

doc_candidates <- fac_287g_doc |>
  inner_join(
    facility_sources_exact,
    by = "state_key",
    relationship = "many-to-many"
  )

doc_matches <- doc_candidates |>
  filter(
    str_detect(str_to_lower(facility_name), doc_facility_pattern),
    !str_detect(str_to_lower(facility_name), doc_exclude_pattern)
  ) |>
  arrange(state, county, agency, source_rank) |>
  group_by(state, county, agency, facility_key) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_type = "state_doc_all_facilities",
    match_score = 1,
    needs_review = TRUE
  )

# manual matches ---------------------------------------------------------

manual_points_specific <- manual_points |>
  filter(!is.na(county), county != "")

manual_points_general <- manual_points |>
  filter(is.na(county) | county == "")

manual_matches_specific <- fac_287g |>
  inner_join(
    manual_points_specific,
    by = c("agency", "state", "county")
  )

manual_matches_general <- fac_287g |>
  inner_join(
    manual_points_general |> select(-county),
    by = c("agency", "state")
  )

manual_matches <- bind_rows(
  manual_matches_specific,
  manual_matches_general
) |>
  distinct(state, county, agency, .keep_all = TRUE) |>
  transmute(
    state,
    county,
    agency,
    agency_level,
    needs_review = TRUE,
    support_clean,
    has_addendum,
    moa_pending,
    state_key,
    county_key,
    agency_key,
    facility_guess,
    facility_guess_key,

    source = "manual",
    source_rank = 0L,
    detention_facility_code = NA_character_,
    facility_name = manual_facility_name,
    facility_address = manual_address,
    facility_city = manual_city,
    facility_county = coalesce(manual_county, county),
    facility_county_fips = NA_character_,
    facility_state = coalesce(manual_state, state),
    facility_state_fips = NA_character_,
    facility_zip = manual_zip,
    facility_address_full = str_squish(
      paste(manual_address, manual_city, manual_state, manual_zip, sep = ", ")
    ),
    facility_latitude = latitude,
    facility_longitude = longitude,
    facility_field_office = NA_character_,
    facility_key = norm_key(manual_facility_name),

    match_type = "manual",
    match_score = 1,
    manual_reason,
    manual_note
  )

# combine all matches ----------------------------------------------------

non_doc_matches <- bind_rows(
  facility_exact_matches,
  facility_fuzzy_county,
  facility_fuzzy_state
) |>
  group_by(state, county, agency) |>
  arrange(source_rank, desc(match_score)) |>
  slice_head(n = 1) |>
  ungroup()

auto_matches <- bind_rows(
  non_doc_matches,
  doc_matches
)

facility_all_matches <- bind_rows(
  manual_matches,
  auto_matches |>
    anti_join(manual_matches, by = c("state", "county", "agency"))
)

facility_unmatched_final <- fac_287g |>
  anti_join(facility_all_matches, by = c("state", "county", "agency")) |>
  mutate(
    match_type = "unmatched",
    source = NA_character_,
    source_rank = NA_integer_,
    needs_review = TRUE
  )

readr::write_csv(
  facility_unmatched_final |>
    select(state, county, agency, agency_level, support_clean, facility_guess),
  "data/processed/facility_unmatched.csv"
)

# facility point sf layer ------------------------------------------------

facility_agreements_sf <- facility_all_matches |>
  filter(!is.na(facility_latitude), !is.na(facility_longitude)) |>
  st_as_sf(
    coords = c("facility_longitude", "facility_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  mutate(
    src = source,
    needs_review = needs_review |
      source != "facilities" |
      match_type != "exact_state_county_facility_name" |
      has_addendum |
      moa_pending
  )

# diagnostics ------------------------------------------------------------

facility_fuzzy_matches <- facility_all_matches |>
  as_tibble() |>
  select(-any_of("geometry")) |>
  filter(str_detect(match_type, "fuzzy")) |>
  filter(match_score < 0.85) |>
  select(
    state,
    county,
    agency,
    facility_guess,
    facility_name,
    source,
    match_type,
    match_score
  ) |>
  arrange(match_score)

facility_review <- facility_all_matches |>
  as_tibble() |>
  select(-any_of("geometry")) |>
  mutate(
    flag_missing_coordinates = is.na(facility_latitude) |
      is.na(facility_longitude),

    flag_state_level_fuzzy = match_type == "fuzzy_state_facility",

    flag_low_confidence_fuzzy = str_detect(match_type, "fuzzy") &
      match_score < 0.85,

    flag_jails_prisons_source = source == "jails_prisons",

    review_reasons = pmap_chr(
      list(
        flag_missing_coordinates,
        flag_state_level_fuzzy,
        flag_low_confidence_fuzzy,
        flag_jails_prisons_source
      ),
      \(missing_coords, state_fuzzy, low_fuzzy, jails_source) {
        reasons <- c(
          if (missing_coords) "missing_coordinates",
          if (state_fuzzy) "state_level_fuzzy",
          if (low_fuzzy) "low_confidence_fuzzy",
          if (jails_source) "jails_prisons_source"
        )

        paste(reasons, collapse = "; ")
      }
    )
  ) |>
  filter(review_reasons != "") |>
  arrange(desc(flag_missing_coordinates), match_score) |>
  select(
    review_reasons,
    state,
    county,
    agency,
    facility_guess,
    facility_name,
    source,
    match_type,
    match_score,
    facility_address,
    facility_city,
    facility_county,
    facility_latitude,
    facility_longitude
  )

readr::write_csv(
  facility_review,
  "data/processed/facility_matches_needing_review.csv"
)

non_facility_layers <- bind_rows(
  state_agreements_sf |> st_transform(4326) |> mutate(match_layer = "state"),
  county_agreements_sf |> st_transform(4326) |> mutate(match_layer = "county"),
  university_agreements_sf |>
    st_transform(4326) |>
    mutate(match_layer = "university"),
  municipal_agreements_sf |>
    st_transform(4326) |>
    mutate(match_layer = "municipal")
) |>
  st_as_sf()

non_facility_unmatched <- non_facility_layers[
  is.na(st_geometry(non_facility_layers)) |
    st_is_empty(st_geometry(non_facility_layers)),
] |>
  st_drop_geometry()

agency_name_cols <- intersect(
  c("agency", "LAW ENFORCEMENT AGENCY"),
  names(non_facility_unmatched)
)

if (length(agency_name_cols) > 0) {
  if ("agency" %in% names(non_facility_unmatched)) {
    non_facility_unmatched <- non_facility_unmatched |>
      mutate(agency = coalesce(!!!syms(agency_name_cols)))
  } else {
    non_facility_unmatched <- non_facility_unmatched |>
      rename(agency = all_of(agency_name_cols[[1]]))
  }
} else {
  non_facility_unmatched <- non_facility_unmatched |>
    mutate(agency = NA_character_)
}

non_facility_unmatched <- non_facility_unmatched |>
  select(
    match_layer,
    state,
    county,
    agency,
    geom_class,
    needs_review,
    any_of(c(
      "state_match",
      "county_match",
      "city_guess",
      "city_match",
      "manual_match_layer",
      "src",
      "manual_reason",
      "manual_note",
      "university_name",
      "university_guess",
      "university_guess_final"
    ))
  )

readr::write_csv(
  non_facility_unmatched,
  "data/processed/non_facility_matches_needing_review.csv"
)

# bind all layers --------------------------------------------------------

all_agreements_sf <- bind_rows(
  non_facility_layers,
  facility_agreements_sf |> st_transform(4326)
) |>
  filter(!is.na(geometry)) |>
  st_make_valid() |>
  st_transform(4326)

# unresolved geometry diagnostics
unmatched_geom <- agencies_all |>
  filter(geom_class != "unknown") |>
  anti_join(
    st_drop_geometry(all_agreements_sf),
    by = intersect(
      names(agencies_all),
      names(st_drop_geometry(all_agreements_sf))
    )
  )

# save final outputs -----------------------------------------------------

sfarrow::st_write_parquet(
  all_agreements_sf,
  "data/processed/all_agreements_sf.parquet"
)
sfarrow::st_write_parquet(
  facility_agreements_sf,
  "data/processed/facility_agreements_sf.parquet"
)
