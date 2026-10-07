# One record per agency, summarised from its agreements -> data/agencies.parquet
library(tidyverse)

source("code/functions.R")

identities <- arrow::read_parquet("data/intermediate/identity-agreements.parquet")
ice <- arrow::read_parquet("data/intermediate/identity-agencies.parquet")
agreements <- arrow::read_parquet("data/intermediate/agreements.parquet")
geography <- arrow::read_parquet(
  "data/agreements-sf.parquet",
  col_select = c("agreement_id", "county_fips")
)

# --- ICE's record: listing window, models, signing dates, removal; named as in the agreements file ------------
ice_record <- identities |>
  group_by(agency_id) |>
  arrange(first_seq, .by_group = TRUE) |>
  summarise(
    first_appeared = min(first_appeared), last_appeared = max(last_appeared),
    n_agreements = n(), n_active = sum(status == "Active"),
    # blank while any agreement is active; the agency left ICE's list between last_appeared and removed_by
    removed_by = if (any(status == "Active")) as.Date(NA) else max(removed_by, na.rm = TRUE),
    first_signed = min(signed), last_signed = max(signed),
    models = paste(unique(support_abbr(support_key)), collapse = "; "),
    # in signing order; agreements ICE listed out of that order, or on the same sheet, would otherwise jumble it
    model_history = paste(sprintf("%s %s", support_abbr(support_key), signed)[order(signed, first_seq)], collapse = " -> "),
    .groups = "drop"
  ) |>
  mutate(has_active = n_active > 0)

# --- jurisdiction and county --------------------------------------------------------
# the agency takes its active or latest agreement's level and counties ("; "-joined, none for a state agency)
level_modern <- agreements |>
  arrange(desc(status == "Active"), desc(last_appeared)) |>
  distinct(agency_id, .keep_all = TRUE) |>
  left_join(geography, by = "agreement_id") |>
  transmute(agency_id, jurisdiction_level, county_fips)

# --- assemble ---------------------------------------------------------------------
agencies <- ice |>
  select(agency_id, state, state_abbr, display_agency) |>
  left_join(ice_record, by = "agency_id") |>
  left_join(level_modern, by = "agency_id") |>
  mutate(jurisdiction_level = coalesce(jurisdiction_level,
                                       str_to_title(na_if(agency_level_from_name(display_agency, state), "unknown")))) |>
  select(agency_id, state, state_abbr, display_agency, jurisdiction_level, county_fips, has_active,
         n_agreements, n_active, first_appeared_date = first_appeared, last_appeared_date = last_appeared,
         removed_by_date = removed_by, first_signed_date = first_signed, last_signed_date = last_signed,
         models, model_history) |>
  arrange(state, display_agency)

stopifnot(
  "every agency id is unique" = !anyDuplicated(agencies$agency_id),
  "every agency holds at least one agreement" = all(agencies$n_agreements >= 1L),
  "every agreement's agency is present" = all(agreements$agency_id %in% agencies$agency_id),
  "every jurisdiction level is one of the eight" =
    all(is.na(agencies$jurisdiction_level) | agencies$jurisdiction_level %in% JURISDICTION_LEVELS)
)
if (anyNA(agencies$jurisdiction_level)) {
  warning(sum(is.na(agencies$jurisdiction_level)), " agency(s) have no jurisdiction level: ",
          paste(agencies$agency_id[is.na(agencies$jurisdiction_level)], collapse = ", "))
}

dir.create("data", showWarnings = FALSE)
arrow::write_parquet(agencies, "data/agencies.parquet")
message(sprintf("agencies: %d (%d with an active agreement) from %d agreements", nrow(agencies), sum(agencies$has_active), nrow(agreements)))
