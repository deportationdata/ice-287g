# Manual override CSVs -> data/manual-{points,polygons,regional}.parquet
library(tidyverse)
library(tidygeocoder)

# all-character: readr would guess logical for empty columns and drop zip zeros
manual_points <- read_csv(
  "inputs/manual-facility-points.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(
    agency,
    state,
    county,
    facility_name = manual_facility_name,
    # CNMI's positive longitude is correct
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    reason = manual_reason,
    note = manual_note
  )

# match_layer/match_name are consumed verbatim by the layer matchers
manual_polygons <- read_csv(
  "inputs/manual-non-facility-polygons.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(
    agency,
    state,
    county,
    match_layer = manual_match_layer,
    match_name = manual_match_name,
    reason = manual_reason,
    note = manual_note
  )

# a regional department's member municipalities, one per row
manual_regional <- read_csv(
  "inputs/manual-regional-municipalities.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(
    agency,
    state,
    county,
    municipality,
    # township names recur across counties
    municipality_county,
    source,
    note
  )

# a judicial district's or circuit's counties, one per row, with the source for each
manual_judicial <- read_csv(
  "inputs/manual-judicial-district-counties.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(agency, state, county, source, note)

# a regional jail authority's member counties, one per row, with the source for each
manual_regional_jails <- read_csv(
  "inputs/manual-regional-jail-counties.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(agency, state, county, source, note)

# a port authority's airports, one per row, keyed on the FAA location identifier
manual_ports <- read_csv(
  "inputs/manual-port-airports.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(agency, state, faa_code, airport, source, note)

# a state DOC's own list of the facilities it runs, as of the day it was read, geocoded by address
manual_doc_facilities <- read_csv(
  "inputs/manual-doc-facilities.csv",
  col_types = cols(.default = col_character())
) |>
  transmute(
    state,
    agency,
    facility_name,
    facility_address,
    facility_city,
    facility_zip,
    listed_on = as.Date(listed_on),
    source_url,
    note,
    # a facility HIFLD names differently is pinned to HIFLD's record; one no source places, to published coordinates
    hifld_id,
    manual_latitude = as.numeric(latitude),
    manual_longitude = as.numeric(longitude),
    # a facility that publishes only a post office box is placed by name in the matcher instead
    geocode_address = if_else(
      !is.na(facility_address) & !str_detect(facility_address, regex("^(P\\.? ?O\\.?|POST OFFICE) ?BOX|^BOX \\d", ignore_case = TRUE)),
      str_remove(str_squish(paste(facility_address, facility_city, state, coalesce(facility_zip, ""), sep = ", ")), ",\\s*$"),
      NA_character_
    )
  )

# the committed cache is append-only and keyed by address; do not regenerate it
geocode_cache_path <- "data/intermediate/cache-arcgis-geocodes.rds"
geocode_cache <- read_rds(geocode_cache_path)
new_addresses <- setdiff(na.omit(unique(manual_doc_facilities$geocode_address)), geocode_cache$address_full)
if (length(new_addresses)) {
  geocode_cache <- bind_rows(
    geocode_cache,
    tibble(address_full = new_addresses) |>
      geocode(address_full, method = "arcgis", lat = latitude, long = longitude, limit = 1, full_results = TRUE)
  ) |>
    distinct(address_full, .keep_all = TRUE)
  write_rds(geocode_cache, geocode_cache_path)
}

# street-level matches only: a city or zip centroid would place the facility wrong
manual_doc_facilities <- manual_doc_facilities |>
  left_join(
    geocode_cache |>
      filter(`attributes.Addr_type` %in% c("PointAddress", "StreetAddress", "Subaddress", "StreetInt")) |>
      select(address_full, latitude, longitude),
    by = c("geocode_address" = "address_full")
  ) |>
  mutate(latitude = coalesce(manual_latitude, latitude), longitude = coalesce(manual_longitude, longitude)) |>
  select(-geocode_address, -manual_latitude, -manual_longitude)

# where a department says which of its facilities its 287(g) program runs in, only those count;
# a department with no row here runs it in every facility on its list
manual_doc_program_sites <- read_csv(
  "inputs/manual-doc-program-sites.csv",
  col_types = cols(.default = col_character())
)
stopifnot("every program site in inputs/manual-doc-program-sites.csv is on its department's list" =
  nrow(anti_join(manual_doc_program_sites, manual_doc_facilities, by = c("state", "agency", "facility_name"))) == 0)
manual_doc_facilities <- manual_doc_facilities |>
  left_join(
    manual_doc_program_sites |> transmute(state, agency, facility_name, program_site_source = source),
    by = c("state", "agency", "facility_name")
  ) |>
  mutate(program_site = case_when(
    !is.na(program_site_source) ~ TRUE,
    paste(state, agency) %in% paste(manual_doc_program_sites$state, manual_doc_program_sites$agency) ~ FALSE,
    TRUE ~ NA
  ))

arrow::write_parquet(manual_doc_facilities, "data/intermediate/manual-doc-facilities.parquet")
arrow::write_parquet(manual_points, "data/intermediate/manual-facility-points.parquet")
arrow::write_parquet(manual_ports, "data/intermediate/manual-port-airports.parquet")
arrow::write_parquet(manual_polygons, "data/intermediate/manual-non-facility-polygons.parquet")
arrow::write_parquet(manual_regional, "data/intermediate/manual-regional-municipalities.parquet")
arrow::write_parquet(manual_judicial, "data/intermediate/manual-judicial-district-counties.parquet")
arrow::write_parquet(manual_regional_jails, "data/intermediate/manual-regional-jail-counties.parquet")
