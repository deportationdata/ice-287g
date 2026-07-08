library(tidyverse)
library(sf)
library(arrow)

source("code/functions.R")

flag_list <- function(...) {
  flags <- c(...)
  flags <- flags[!is.na(flags) & flags != ""]
  paste(flags, collapse = "; ")
}

facility_agreements_sf <- read_sf_parquet(
  "data/facility_agreements_sf.parquet"
) |>
  st_drop_geometry()

# DOC excluded-local audit -----------------------------------------------

doc_excluded_local <- readr::read_csv(
  "data/doc_facility_excluded_local_jails.csv",
  show_col_types = FALSE
)

doc_excluded_local_audit <- doc_excluded_local |>
  mutate(
    facility_name_clean = str_to_lower(facility_name),
    operator_clean = str_to_lower(coalesce(facility_operator_name, "")),
    source_type_clean = str_to_upper(str_squish(facility_source_type)),
    audit_flags = pmap_chr(
      list(facility_name_clean, operator_clean, source_type_clean),
      \(name, operator, source_type) {
        flag_list(
          if (str_detect(
            name,
            "department of corrections|\\bdoc\\b|state prison|state correction"
          )) {
            "doc_or_state_name"
          },
          if (str_detect(
            operator,
            "department of corrections|\\bdoc\\b|state prison|state correction"
          )) {
            "doc_or_state_operator"
          },
          if (str_detect(
            name,
            "prison complex|state prison complex|correctional complex"
          )) {
            "prison_complex_name"
          },
          if (source_type == "PRIMARY STATE AGENCY") {
            "hifld_primary_state_agency"
          },
          if (source_type == "FEDERAL") {
            "hifld_prisons_federal"
          },
          if (
            str_detect(name, "correctional center|correctional facility") &
              !str_detect(name, "\\b(county|parish|city|municipal|borough)\\b")
          ) {
            "correctional_name_without_local_modifier"
          }
        )
      }
    )
  ) |>
  filter(audit_flags != "") |>
  select(
    audit_flags,
    state,
    county,
    agency,
    facility_name,
    source,
    facility_source_type,
    facility_operator_name,
    facility_source_state_fips,
    facility_source_county_fips,
    facility_status,
    doc_match_tier,
    doc_research_reason,
    facility_address,
    facility_city,
    facility_county,
    facility_latitude,
    facility_longitude
  ) |>
  arrange(state, facility_source_type, facility_name)

readr::write_csv(
  doc_excluded_local_audit,
  "data/doc_facility_excluded_local_jails_audit.csv"
)

# County facility mapping QA ---------------------------------------------

county_facility_qa <- facility_agreements_sf |>
  filter(agency_level == "county") |>
  group_by(state, county, agency) |>
  mutate(agency_facility_count = n()) |>
  ungroup() |>
  mutate(
    agency_county_key = norm_place(county),
    facility_county_key = norm_place(facility_county),
    source_county_key = norm_place(facility_source_county),
    county_keys_match = agency_county_key %in% c(
      facility_county_key,
      source_county_key
    ),
    qa_flags = pmap_chr(
      list(
        match_type,
        county_keys_match,
        agency_facility_count,
        facility_source_type
      ),
      \(match_type, county_match, facility_count, source_type) {
        flag_list(
          if (str_detect(match_type, "fuzzy")) "fuzzy_match",
          if (!isTRUE(county_match)) "county_mismatch",
          if (facility_count > 10) "many_facilities_for_agency",
          if (source_type %in% c(
            "LOCAL POLICE DEPARTMENT",
            "PRIMARY STATE AGENCY",
            "FEDERAL",
            "STATE"
          )) {
            "unexpected_source_type_for_county"
          }
        )
      }
    )
  ) |>
  select(
    qa_flags,
    state,
    county,
    agency,
    agency_facility_count,
    facility_name,
    facility_county,
    facility_source_county,
    facility_source_county_fips,
    facility_source_type,
    source,
    match_type,
    match_score,
    agreement_needs_review = needs_review,
    facility_address,
    facility_city
  ) |>
  arrange(desc(qa_flags != ""), state, county, agency, facility_name)

readr::write_csv(
  county_facility_qa,
  "data/county_facility_mapping_qa.csv"
)

# Municipal facility mapping QA ------------------------------------------

municipal_facility_qa <- facility_agreements_sf |>
  filter(agency_level == "municipal") |>
  mutate(
    city_guess = extract_city_guess(agency),
    city_key = norm_match_phrase(city_guess),
    facility_name_key = norm_match_phrase(facility_name),
    expected_city_name = !is.na(city_key) & city_key != "" &
      str_detect(facility_name_key, paste0("^", city_key, "\\b")),
    agency_county_key = norm_place(county),
    facility_county_key = norm_place(facility_county),
    source_county_key = norm_place(facility_source_county),
    county_keys_match = is.na(agency_county_key) | agency_county_key == "" |
      agency_county_key %in% c(facility_county_key, source_county_key),
    qa_flags = pmap_chr(
      list(
        match_type,
        expected_city_name,
        county_keys_match,
        facility_source_type
      ),
      \(match_type, expected_city, county_match, source_type) {
        flag_list(
          if (str_detect(match_type, "fuzzy")) "fuzzy_match",
          if (!isTRUE(expected_city)) "facility_name_not_city_prefixed",
          if (!isTRUE(county_match)) "county_mismatch",
          if (source_type %in% c(
            "SHERIFF'S OFFICE",
            "COUNTY",
            "STATE",
            "FEDERAL",
            "PRIMARY STATE AGENCY"
          )) {
            "unexpected_source_type_for_municipal"
          }
        )
      }
    )
  ) |>
  select(
    qa_flags,
    state,
    county,
    agency,
    city_guess,
    facility_name,
    facility_city,
    facility_county,
    facility_source_county,
    facility_source_county_fips,
    facility_source_type,
    source,
    match_type,
    match_score,
    agreement_needs_review = needs_review,
    facility_address
  ) |>
  arrange(desc(qa_flags != ""), state, county, agency, facility_name)

readr::write_csv(
  municipal_facility_qa,
  "data/municipal_facility_mapping_qa.csv"
)
