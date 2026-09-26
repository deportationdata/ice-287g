# Match-quality and review helpers.

# match tiers, strongest first: own-county name, statewide full name, jurisdiction-word-stripped key
match_tier_rank <- function(match_type) {
  case_when(
    match_type == "exact_state_county_agency_name" ~ 1L,
    match_type == "unique_state_full_name" ~ 2L,
    match_type == "unique_state_agency_name" ~ 3L,
    match_type == "prefix_state_agency_name" ~ 4L,
    TRUE ~ 5L
  )
}
