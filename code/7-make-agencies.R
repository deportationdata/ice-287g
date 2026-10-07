# One record per agency, summarised from its agreements -> data/agencies.parquet
library(tidyverse)

source("code/functions.R")

identities <- arrow::read_parquet("data/intermediate/identity-agreements.parquet")
ice <- arrow::read_parquet("data/intermediate/identity-agencies.parquet")
agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")
geography <- arrow::read_parquet(
  "data/agreements-sf.parquet",
  col_select = c("agreement_id", "ORI9", "place", "place_type", "place_geoid", "county", "county_fips",
                 "state_fips", "geoid", "geoid_type")
)

# --- ICE's record: listing window, models, signing dates, removal; named as in the agreements file ------------
ice_record <- identities |>
  group_by(agency_id) |>
  arrange(first_seq, .by_group = TRUE) |>
  summarise(
    first_appeared = min(first_appeared), last_appeared = max(last_appeared),
    n_agreements = n(), n_active = sum(status == "Active"),
    # blank while any agreement is active; the agency left ICE's list between last_appeared and last_removed_by
    last_removed_by = if (any(status == "Active")) as.Date(NA) else max(removed_by, na.rm = TRUE),
    first_signed = min(signed), last_signed = max(signed),
    models = paste(unique(support_abbr(support_key)), collapse = "; "),
    # in signing order; agreements ICE listed out of that order, or on the same sheet, would otherwise jumble it
    model_history = paste(sprintf("%s %s", support_abbr(support_key), signed)[order(signed, first_seq)], collapse = " -> "),
    .groups = "drop"
  ) |>
  mutate(has_active = n_active > 0)

# --- identifier and geography ---------------------------------------------
# the agency takes its active or latest agreement's ORI and geography, named as in the agreements file;
# historical agreements carry no geography, and the fields never differ between an agency's active agreements
level_modern <- agreements |>
  arrange(desc(status == "Active"), desc(last_appeared)) |>
  distinct(agency_id, .keep_all = TRUE) |>
  select(agency_id, agreement_id) |>
  left_join(geography, by = "agreement_id") |>
  select(-agreement_id)

# --- jurisdiction level -----------------------------------------------------------
# what the agency is (State, County, Municipal, Campus, ...), set per agreement in 2-make-agreements.R from
# the manual list, the agency's name and ICE's TYPE; the same on every agreement an agency signs
level <- agreements |>
  distinct(agency_id, jurisdiction_level)
stopifnot("one jurisdiction level per agency" = !anyDuplicated(level$agency_id))

# --- assemble ---------------------------------------------------------------------
agencies <- ice |>
  select(agency_id, state, state_abbr, display_agency) |>
  left_join(ice_record, by = "agency_id") |>
  left_join(level, by = "agency_id") |>
  left_join(level_modern, by = "agency_id") |>
  select(agency = display_agency, agency_id, ORI9, jurisdiction_level, has_active,
         n_agreements, n_active, first_appeared_date = first_appeared, last_appeared_date = last_appeared,
         last_removed_by_date = last_removed_by, first_signed_date = first_signed, last_signed_date = last_signed,
         models, model_history,
         place, place_type, place_geoid, county, county_fips, state, state_fips, geoid, geoid_type) |>
  arrange(state, agency)

stopifnot(
  "every agency id is unique" = !anyDuplicated(agencies$agency_id),
  "every agency holds at least one agreement" = all(agencies$n_agreements >= 1L),
  "every agreement's agency is present" = all(agreements$agency_id %in% agencies$agency_id)
)

dir.create("data", showWarnings = FALSE)
arrow::write_parquet(agencies, "data/agencies.parquet")
message(sprintf("agencies: %d (%d with an active agreement) from %d agreements", nrow(agencies), sum(agencies$has_active), nrow(agreements)))
