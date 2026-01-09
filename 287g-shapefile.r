library(readxl)
library(dplyr)
library(stringr)

participating_agencies <-
  read_excel("participatingAgencies01052025pm.xlsx") |>
  mutate(status = "participating")

pending_agencies <-
  read_excel("pendingAgencies01052025pm.xlsx") |>
  mutate(status = "pending")

agencies_all <-
  bind_rows(participating_agencies, pending_agencies) |>
  rename(
    state = STATE,
    agency = `LAW ENFORCEMENT AGENCY`,
    agency_type = TYPE,
    county_name = COUNTY,
    support_type = `SUPPORT TYPE`,
    signed_date = SIGNED
  ) |>
  mutate(
    agency_type = str_to_lower(agency_type),
    support_type = str_to_lower(support_type),
    agency = str_squish(agency),
    county_name = str_squish(county_name)
  )

# baseline
agencies_all <-
  agencies_all |>
  mutate(
    geometry_sheet = case_when(
      support_type == "jail enforcement model" ~ "facility",
      support_type %in% c("task force model", "warrant service officer") &
        agency_type == "state" ~ "state",
      support_type %in% c("task force model", "warrant service officer") &
        agency_type == "county" ~ "county",
      support_type %in% c("task force model", "warrant service officer") &
        agency_type == "municipality" ~ "municipality",
      TRUE ~ "unknown"
    )
  )

agencies_all <-
  agencies_all |>
  mutate(
    pdf_required = case_when(
      status == "pending" ~ FALSE, # no MOA exists yet
      support_type == "jail enforcement model" ~ TRUE,
      support_type == "warrant service officer" ~ TRUE,
      geometry_sheet == "unknown" ~ TRUE,
      TRUE ~ FALSE
    )
  )

agencies_all <-
  agencies_all |>
  group_by(state, agency) |>
  mutate(
    multiple_agreements = n() > 1
  ) |>
  ungroup()

agencies_all <-
  agencies_all |>
  mutate(
    pdf_required = if_else(
      multiple_agreements & status == "participating",
      TRUE,
      pdf_required
    )
  )

agencies_all <-
  agencies_all |>
  mutate(
    geometry_final = if_else(
      pdf_required,
      NA_character_,
      geometry_sheet
    )
  )

agencies_all <-
  agencies_all |>
  mutate(
    agreement_id = row_number()
  )
