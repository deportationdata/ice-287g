# BJS jail and prison censuses, one row per facility per round, geocoded -> data/intermediate/facility-list-censuses.parquet
# Census of Jails 1999 (ICPSR 3318), 2006 (26602), 2013 (36128) and 2019 (38323, read from
# ice-detention-facilities); Census of State and Federal Adult Correctional Facilities 2000 (4021),
# 2005 (24642), 2012 (37294) and 2019 (38325). A facility listed in a round was open that year.
library(tidyverse)
library(tidygeocoder)

source("code/functions.R")

state_xwalk <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")

read_icpsr <- function(study) {
  haven::read_dta(sprintf("inputs/ICPSR_%1$s/DS0001/%1$s-0001-Data.dta", study))
}

# ICPSR fills withheld or empty text with 9s or -9
blank_fill <- function(x) {
  x <- str_squish(as.character(x))
  if_else(is.na(x) | x == "" | str_detect(x, "^-?9+$"), NA_character_, x)
}

po_box_pattern <- regex("^(P\\.? ?O\\.?|POST OFFICE) ?BOX|^BOX \\d|\\bDRAWER\\b", ignore_case = TRUE)

# the label of each column, to find the 2006 facility slots by what they are
column_labels <- function(x) {
  map_chr(x, \(v) attr(v, "label") %||% "")
}

# --- Census of Jails -----------------------------------------------------------

# the 1999 file splits the Census of Governments id that later rounds carry whole as JURISID
jails_1999 <- read_icpsr("03318") |>
  transmute(
    census_year = 1999L,
    jurisdiction_id = sprintf("%02d%d%03d%03d", as.integer(V1A), as.integer(V1B), as.integer(V1C), as.integer(V1D)),
    facility_id = NA_character_,
    facility_name = blank_fill(V2),
    facility_address = blank_fill(V6),
    facility_city = blank_fill(V7),
    state_abbr = blank_fill(V8),
    facility_zip = str_sub(blank_fill(V9), 1, 5),
    facility_county = str_to_title(blank_fill(V3)),
    year_built = NA_integer_,
    closure_planned = NA,
    closure_note = NA_character_
  )

# a 2006 row is a jail jurisdiction with up to 13 facilities; only the first has an address
jails_2006_raw <- read_icpsr("26602")
labels_2006 <- column_labels(jails_2006_raw)
slot_names <- names(labels_2006)[str_detect(labels_2006, "^FACILITY NAME - \\d+$")]
slot_built <- names(labels_2006)[str_detect(labels_2006, "^ITEM 7 - YEAR OF ORIGINAL CONSTRUCTION - \\d+$")]
stopifnot("2006 facility slots and construction years no longer line up" = length(slot_names) == length(slot_built))

jails_2006 <- map2(slot_names, slot_built, \(name_col, built_col) {
  jails_2006_raw |>
    transmute(
      slot = as.integer(str_extract(labels_2006[[name_col]], "\\d+$")),
      jurisdiction_id = str_sub(V1, 1, 9),
      facility_name = blank_fill(.data[[name_col]]),
      year_built = suppressWarnings(as.integer(.data[[built_col]])),
      V5, V6, V7, V8, V22, V22S
    )
}) |>
  list_rbind() |>
  filter(!is.na(facility_name)) |>
  transmute(
    census_year = 2006L,
    jurisdiction_id,
    facility_id = NA_character_,
    facility_name,
    facility_address = if_else(slot == 1L, blank_fill(V5), NA_character_),
    facility_city = if_else(slot == 1L, blank_fill(V6), NA_character_),
    state_abbr = blank_fill(V7),
    facility_zip = if_else(slot == 1L, str_sub(blank_fill(V8), 1, 5), NA_character_),
    facility_county = NA_character_,
    year_built = if_else(between(year_built, 1700L, 2006L), year_built, NA_integer_),
    # the jurisdiction's plan names the facilities it will close, not always this one
    closure_planned = as.integer(V22) == 1L,
    closure_note = blank_fill(V22S)
  )

jails_2013 <- read_icpsr("36128") |>
  transmute(
    census_year = 2013L,
    jurisdiction_id = blank_fill(JURISID),
    facility_id = blank_fill(FACID),
    facility_name = blank_fill(NAME),
    facility_address = NA_character_,
    facility_city = NA_character_,
    # the Census of Governments id opens with the state's alphabetical code
    state_abbr = NA_character_,
    facility_zip = NA_character_,
    facility_county = NA_character_,
    year_built = NA_integer_,
    closure_planned = NA,
    closure_note = NA_character_
  )

jails_2019 <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/jails_prisons.parquet"
) |>
  transmute(
    census_year = 2019L,
    jurisdiction_id = as.character(bjs_facility_ID),
    facility_id = NA_character_,
    facility_name = blank_fill(name),
    facility_address = blank_fill(address),
    facility_city = blank_fill(city),
    state_abbr = blank_fill(state),
    facility_zip = str_sub(blank_fill(zip), 1, 5),
    facility_county = NA_character_,
    year_built = NA_integer_,
    closure_planned = NA,
    closure_note = NA_character_
  )

# 2013 carries no state; the 1999 ids pair each alphabetical state code with its abbreviation
gov_state_codes <- jails_1999 |>
  distinct(gov_state = str_sub(jurisdiction_id, 1, 2), state_abbr) |>
  filter(!is.na(state_abbr)) |>
  distinct(gov_state, .keep_all = TRUE)

jails <- bind_rows(jails_1999, jails_2006, jails_2013, jails_2019) |>
  mutate(census = "Census of Jails", gov_state = str_sub(jurisdiction_id, 1, 2)) |>
  left_join(gov_state_codes |> rename(state_from_id = state_abbr), by = "gov_state") |>
  mutate(state_abbr = coalesce(state_abbr, state_from_id)) |>
  select(-gov_state, -state_from_id)

# --- Census of State and Federal Adult Correctional Facilities -----------------

prisons_2000 <- read_icpsr("04021") |>
  transmute(
    census_year = 2000L,
    facility_id = blank_fill(VID),
    facility_name = blank_fill(VNAMEFAC),
    facility_address = blank_fill(VADDRESS),
    facility_city = blank_fill(VCITY),
    state_abbr = na_if(blank_fill(VSTATE), "FD"),
    facility_zip = str_sub(blank_fill(VZIPCODE), 1, 5),
    facility_county = str_to_title(blank_fill(VCNTYNAM)),
    year_built = suppressWarnings(as.integer(V8)),
    closure_planned = as.integer(V903) == 1L,
    closure_note = NA_character_
  )

# 2005 gives a contact address and a physical one; the physical one places the facility
prisons_2005 <- read_icpsr("24642") |>
  transmute(
    census_year = 2005L,
    facility_id = blank_fill(V1),
    facility_name = blank_fill(V2),
    facility_address = blank_fill(V9),
    facility_city = blank_fill(V10),
    state_abbr = coalesce(blank_fill(V11), blank_fill(V7)),
    facility_zip = str_sub(blank_fill(V12), 1, 5),
    facility_county = str_to_title(blank_fill(V235)),
    year_built = suppressWarnings(as.integer(V41)),
    closure_planned = as.integer(V43) == 1L,
    closure_note = NA_character_
  )

prisons_2012 <- read_icpsr("37294") |>
  mutate(across(c(V05, V06), blank_fill)) |>
  transmute(
    census_year = 2012L,
    facility_id = blank_fill(V02),
    facility_name = blank_fill(V04),
    # a street line wins over a post office box whichever line it is on
    facility_address = if_else(str_detect(coalesce(V05, ""), po_box_pattern) & !is.na(V06), V06, coalesce(V05, V06)),
    facility_city = blank_fill(V07),
    state_abbr = blank_fill(V08),
    facility_zip = str_sub(blank_fill(V09), 1, 5),
    facility_county = NA_character_,
    year_built = NA_integer_,
    closure_planned = haven::as_factor(V27) == "Yes",
    closure_note = if_else(
      haven::as_factor(V27) == "Yes",
      str_squish(paste(haven::as_factor(V28), haven::as_factor(V29))),
      NA_character_
    )
  )

# 2019 ids start afresh, so the round links to the others by place alone; 17 rows are withheld whole
prisons_2019 <- read_icpsr("38325") |>
  transmute(
    census_year = 2019L,
    facility_id = blank_fill(V003),
    facility_name = blank_fill(V005),
    facility_address = blank_fill(V006),
    facility_city = blank_fill(V007),
    state_abbr = blank_fill(V008),
    facility_zip = str_sub(blank_fill(V010), 1, 5),
    facility_county = NA_character_,
    year_built = NA_integer_,
    closure_planned = NA,
    closure_note = NA_character_,
    # who runs it: a state, the federal government, a private contractor or a state and locality jointly
    facility_operator = if_else(as.integer(V032) > 0, as.character(haven::as_factor(V032)), NA_character_)
  ) |>
  filter(!is.na(facility_name))

prisons <- bind_rows(prisons_2000, prisons_2005, prisons_2012, prisons_2019) |>
  mutate(census = "Census of State and Federal Adult Correctional Facilities", jurisdiction_id = NA_character_)

# --- geocode street addresses into the shared cache -----------------------------

# the committed cache is append-only and keyed by address; do not regenerate it
geocode_cache_path <- "data/intermediate/cache-arcgis-geocodes.rds"

censuses <- bind_rows(jails, prisons) |>
  mutate(
    year_built = if_else(between(year_built, 1700L, census_year), year_built, NA_integer_),
    # a post office box places the mail, not the building
    geocode_address = if_else(
      !is.na(facility_address) & !is.na(facility_city) & !is.na(state_abbr) &
        !str_detect(facility_address, po_box_pattern),
      str_remove(str_squish(paste(facility_address, facility_city, state_abbr, coalesce(facility_zip, ""), sep = ", ")), ",\\s*$"),
      NA_character_
    )
  )

geocode_cache <- if (file.exists(geocode_cache_path)) {
  read_rds(geocode_cache_path)
} else {
  tibble(address_full = character())
}

new_addresses <- setdiff(na.omit(unique(censuses$geocode_address)), geocode_cache$address_full)

for (chunk in split(new_addresses, ceiling(seq_along(new_addresses) / 250))) {
  geocode_cache <- bind_rows(
    geocode_cache,
    tibble(address_full = chunk) |>
      geocode(address_full, method = "arcgis", lat = latitude, long = longitude, limit = 1, full_results = TRUE)
  ) |>
    distinct(address_full, .keep_all = TRUE)
  write_rds(geocode_cache, geocode_cache_path)
  message(nrow(geocode_cache), " addresses geocoded")
}

# street-level matches only: a city or zip centroid would place the facility wrong
geocoded <- geocode_cache |>
  filter(`attributes.Addr_type` %in% c("PointAddress", "StreetAddress", "Subaddress", "StreetInt")) |>
  select(address_full, latitude, longitude)

censuses |>
  left_join(geocoded, by = c("geocode_address" = "address_full")) |>
  left_join(state_xwalk |> select(state_abbr, state_full), by = "state_abbr") |>
  transmute(
    census,
    census_year,
    jurisdiction_id,
    facility_id,
    facility_name,
    facility_address,
    facility_city,
    facility_state = state_full,
    facility_zip,
    facility_county,
    year_built,
    closure_planned,
    closure_note,
    facility_operator,
    latitude,
    longitude,
    state_key = norm_state(state_full),
    facility_key = norm_key(facility_name)
  ) |>
  arrange(census, census_year, facility_state, facility_name) |>
  arrow::write_parquet("data/intermediate/facility-list-censuses.parquet")
