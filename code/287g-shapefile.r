library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(tigris)
library(purrr)
library(stringdist)
library(arrow)
library(tidylog)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# data loading -----------------------------------------------------------
# api_key <- ""
# states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
#             "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
#             "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
#             "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
#             "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
#             "DC")

# all_states <- list()

# for (state in states) {
#   response <- request(paste0("https://api.usa.gov/crime/fbi/cde/agency/byStateAbbr/", state)) |>
#     req_url_query(API_KEY = api_key) |>
#     req_perform()

#   raw <- resp_body_json(response, simplifyVector = TRUE)

#   all_states[[state]] <- bind_rows(raw, .id = "county")

#   Sys.sleep(0.5)
# }

# crime_data <- bind_rows(all_states, .id = "state")
# write_parquet(crime_data, "data/crime-data-all-states.parquet")

participating_agencies <-
  read_excel(
    "sheets/sheets_20260421_173735/participatingAgencies04212026am 12.15.07 AM.xlsx"
  )

load("data/35158-0001-Data.rda")
LEAIC <- da35158.0001

hifld <- arrow::read_parquet(
  "data/ice-detention-facilities/data/hifld-local-law-enforcement-facilities.parquet"
)

hifld_prisons <- arrow::read_parquet(
  "data/ice-detention-facilities/data/hifld-prisons.parquet"
)

jails_prisons <- arrow::read_parquet(
  "data/ice-detention-facilities/data/jails_prisons.parquet"
)

crime_data <- arrow::read_parquet("data/crime-data-all-states.parquet")

facilities <- arrow::read_parquet(
  "data/ice-detention-facilities/data/facilities-latest-sf.parquet"
)


# helper functions -------------------------------------------------------
norm_key <- function(x) {
  x |>
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

norm_place <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all(
      "\\b(county|city|town|village|borough|township|municipality)\\b",
      " "
    ) |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish() |>
    str_replace_all("\\s+", " ")
}

extract_city_guess <- function(x) {
  s <- str_squish(x)
  s <- str_remove(s, regex("(?i)^\\s*city\\s+of\\s+"))
  s <- str_remove(
    s,
    regex(
      "(?i)\\b(police|pd|police dept\\.?|police department|department|dept|public safety|office)\\b.*$"
    )
  )
  s <- str_squish(s)
  s <- na_if(s, "")
  str_to_title(s)
}

# prepare agencies -------------------------------------------------------
agencies_all <-
  # bind_rows(participating_agencies, pending_agencies) |>
  participating_agencies |>
  mutate(
    state = str_to_title(str_trim(STATE)),
    county = str_to_title(str_trim(COUNTY)),
    type_clean = str_to_lower(str_trim(TYPE)),
    support_clean = str_to_lower(str_trim(`SUPPORT TYPE`)),
    has_addendum = !(is.na(ADDENDUM) | ADDENDUM %in% c("", "NA")),
    moa_pending = str_detect(str_to_lower(str_trim(MOA)), "pending")
  ) |>
  # fix data error — Pittsburgh is in Pennsylvania, not New Hampshire
  mutate(
    state = if_else(
      `LAW ENFORCEMENT AGENCY` == "Pittsburgh Police Department" &
        state == "New Hampshire",
      "Pennsylvania",
      state
    )
  ) |>
  group_by(state, `LAW ENFORCEMENT AGENCY`) |>
  mutate(
    agency_level = case_when(
      type_clean %in% c("state agency", "state") ~ "state",
      type_clean == "county" ~ "county",
      type_clean == "municipality" ~ "municipal",
      TRUE ~ "unknown"
    ),

    geom_class = case_when(
      support_clean == "task force model" &
        agency_level == "state" ~ "state_polygon",
      support_clean == "task force model" &
        agency_level == "county" ~ "county_polygon",
      support_clean == "task force model" &
        agency_level == "municipal" ~ "municipal_polygon",

      support_clean %in%
        c(
          "jail enforcement model",
          "warrant service officer"
        ) ~ "facility_point",

      TRUE ~ "unknown"
    ),

    needs_review = case_when(
      geom_class == "unknown" ~ TRUE,
      has_addendum ~ TRUE,
      moa_pending ~ TRUE,
      n() > 1 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  ungroup()

# source tables ----------------------------------------------------------
leaic_tbl <- LEAIC |>
  transmute(
    leaic_state = str_to_title(str_squish(STATENAME)),
    leaic_county = str_to_title(str_squish(COUNTYNAME)),
    leaic_name = str_squish(NAME),
    FSTATE,
    FCOUNTY,
    FPLACE,
    ORI9,
    AGCYTYPE,
    SUBTYPE1,
    SUBTYPE2,
    COMMENT
  ) |>
  mutate(
    state_key = norm_state(leaic_state),
    county_key = norm_place(leaic_county),
    agency_key_src = norm_key(leaic_name)
  )

# state abbreviation to full name crosswalk
state_xwalk <- tibble(
  state_abbr = state.abb,
  state_full = state.name
) |>
  bind_rows(tibble(state_abbr = "DC", state_full = "District Of Columbia"))

hifld_tbl <- bind_rows(
  hifld |>
    transmute(
      src_dataset = "hifld",
      src_id = as.character(hifld_id),
      src_name = str_squish(name),
      src_address = address,
      src_city = str_squish(city),
      src_state = str_squish(state),
      src_zip = zip,
      src_type = type,
      src_status = status,
      src_population = NA_real_,
      src_hold_72 = NA,
      src_latitude = latitude,
      src_longitude = longitude,
      src_date = date
    ),
  hifld_prisons |>
    transmute(
      src_dataset = "hifld_prisons",
      src_id = as.character(hifld_id),
      src_name = str_squish(name),
      src_address = address,
      src_city = str_squish(city),
      src_state = str_squish(state),
      src_zip = zip,
      src_type = type,
      src_status = status,
      src_population = population,
      src_hold_72 = NA,
      src_latitude = latitude,
      src_longitude = longitude,
      src_date = date
    ),
  jails_prisons |>
    transmute(
      src_dataset = "jails_prisons",
      src_id = as.character(bjs_facility_ID),
      src_name = str_squish(name),
      src_address = address,
      src_city = str_squish(city),
      src_state = str_squish(state),
      src_zip = zip,
      src_type = NA_character_,
      src_status = NA_character_,
      src_population = NA_real_,
      src_hold_72 = hold_72,
      src_latitude = NA_real_,
      src_longitude = NA_real_,
      src_date = date
    )
) |>
  left_join(state_xwalk, by = c("src_state" = "state_abbr")) |>
  mutate(
    state_key = norm_state(src_state),
    county_key = norm_place(src_city),
    agency_key_src = norm_key(src_name)
  ) |>
  select(-state_full)

crime_lookup <- crime_data |>
  transmute(
    ori = str_squish(ori),
    crime_lat = latitude,
    crime_lon = longitude,
    agency_name = str_squish(agency_name),
    agency_type = agency_type_name,
    nibrs_start = nibrs_start_date,
    state_abbr = str_squish(state_abbr)
  ) |>
  distinct(ori, .keep_all = TRUE)

# pull boundary geometries -----------------------------------------------
YEAR <- 2024

states_sf <- tigris::states(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(state = str_to_title(NAME), statefp = STATEFP, geometry)

counties_sf <- tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    state = str_to_title(STATE_NAME),
    county = str_to_title(NAME),
    statefp = STATEFP,
    countyfp = COUNTYFP,
    geometry
  )

places_sf <- tigris::places(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(
    state = str_to_title(STATE_NAME),
    place_guess = str_to_title(NAME),
    statefp = STATEFP,
    placefp = PLACEFP,
    geometry
  )

cousubs_sf <- map_dfr(unique(states_sf$statefp), function(fp) {
  tigris::county_subdivisions(state = fp, cb = TRUE, year = YEAR, class = "sf")
}) |>
  transmute(
    state = str_to_title(STATE_NAME),
    place_guess = str_to_title(NAME),
    statefp = STATEFP,
    placefp = COUSUBFP,
    geometry
  )

# normalized places lookup (places preferred over cousubs) ---------------
places_lookup <- bind_rows(
  places_sf |> mutate(src = "place"),
  cousubs_sf |> mutate(src = "cousub")
) |>
  mutate(
    state_key = norm_state(state),
    place_key = norm_place(place_guess)
  ) |>
  mutate(src_rank = if_else(src == "place", 1L, 2L)) |>
  group_by(state_key, place_key) |>
  slice_min(src_rank, n = 1, with_ties = FALSE) |>
  ungroup()

# county centroids (fallback for unmatched municipalities) ---------------
county_centroids <- counties_sf |>
  st_transform(5070) |>
  mutate(centroid = st_centroid(geometry)) |>
  st_drop_geometry() |>
  st_as_sf(sf_column_name = "centroid") |>
  st_transform(4326) |>
  mutate(
    state_key = norm_state(state),
    county_key = norm_place(county)
  ) |>
  rename(geometry = centroid)

# assign polygon geometries ----------------------------------------------
state_agreements_sf <- agencies_all |>
  filter(geom_class == "state_polygon") |>
  left_join(states_sf, by = "state") |>
  st_as_sf()

county_agreements_sf <- agencies_all |>
  filter(geom_class == "county_polygon") |>
  left_join(counties_sf, by = c("state", "county")) |>
  st_as_sf()

municipal_agreements_sf <- agencies_all |>
  filter(geom_class == "municipal_polygon") |>
  mutate(
    city_guess = extract_city_guess(`LAW ENFORCEMENT AGENCY`),
    state_key = norm_state(state),
    place_key = norm_place(city_guess),
    county_key = norm_place(county)
  ) |>
  left_join(
    places_lookup |> select(state_key, place_key, geometry, src),
    by = c("state_key", "place_key")
  ) |>
  left_join(
    county_centroids |>
      select(state_key, county_key, geometry) |>
      rename(county_geometry = geometry),
    by = c("state_key", "county_key")
  ) |>
  mutate(
    missing_place = is.na(src) |
      st_is_empty(geometry) |
      is.na(st_dimension(geometry)),
    src = if_else(missing_place, "county_centroid_fallback", src)
  ) |>
  (\(df) {
    df$geometry[df$missing_place] <- df$county_geometry[df$missing_place]
    df
  })() |>
  select(-county_geometry, -missing_place) |>
  mutate(
    needs_review = needs_review | is.na(geometry) | st_is_empty(geometry)
  ) |>
  st_as_sf()

# facility point matching: exact matches only ----------------------------
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
    agency_key = norm_key(agency)
  )

facilities_tbl <- facilities |>
  st_drop_geometry() |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
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

leaic_facility_tbl <- leaic_tbl |>
  transmute(
    source = "leaic",
    source_rank = 5L,
    detention_facility_code = ORI9,
    facility_name = leaic_name,
    facility_address = NA_character_,
    facility_city = NA_character_,
    facility_county = leaic_county,
    facility_county_fips = as.character(FCOUNTY),
    facility_state = leaic_state,
    facility_state_fips = as.character(FSTATE),
    facility_zip = NA_character_,
    facility_address_full = NA_character_,
    facility_latitude = NA_real_,
    facility_longitude = NA_real_,
    facility_field_office = NA_character_,
    state_key,
    county_key,
    facility_key = agency_key_src,
    ORI9
  ) |>
  left_join(
    crime_lookup |> select(ori, crime_lat, crime_lon),
    by = c("ORI9" = "ori")
  ) |>
  mutate(
    facility_latitude = crime_lat,
    facility_longitude = crime_lon
  ) |>
  select(-crime_lat, -crime_lon)

facility_sources_exact <- bind_rows(
  facilities_tbl,
  hifld_facility_tbl,
  leaic_facility_tbl
)

facility_exact_matches <- fac_287g |>
  inner_join(
    facility_sources_exact,
    by = c(
      "state_key",
      "county_key",
      "agency_key" = "facility_key"
    ),
    relationship = "many-to-many"
  ) |>
  mutate(
    match_type = "exact_state_county_name",
    match_score = 1
  ) |>
  group_by(state, county, agency) |>
  arrange(source_rank) |>
  slice_head(n = 1) |>
  ungroup()

facility_unmatched <- fac_287g |>
  anti_join(
    facility_exact_matches,
    by = c("state", "county", "agency")
  ) |>
  mutate(
    match_type = "unmatched",
    source = NA_character_,
    source_rank = NA_integer_,
    needs_review = TRUE
  )

facility_agreements_sf <- facility_exact_matches |>
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
      has_addendum |
      moa_pending
  )

agencies_all |> count(geom_class)

facility_exact_matches |>
  count(source, match_type, sort = TRUE)

facility_unmatched |>
  select(state, county, agency, support_clean) |>
  arrange(state, county, agency)

facility_exact_matches |>
  filter(is.na(facility_latitude) | is.na(facility_longitude)) |>
  select(state, county, agency, source, facility_name)

# combine and write ------------------------------------------------------
all_agreements_sf <- bind_rows(
  state_agreements_sf |> st_transform(4326),
  county_agreements_sf |> st_transform(4326),
  municipal_agreements_sf |> st_transform(4326),
  facility_agreements_sf |> st_transform(4326)
) |>
  left_join(
    crime_lookup,
    by = c("ORI9" = "ori")
  ) |>
  filter(!is.na(geometry)) |>
  st_make_valid() |>
  st_transform(4326)

# unresolved geometry
unmatched_geom <- agencies_all |>
  filter(geom_class != "unknown") |>
  anti_join(
    st_drop_geometry(all_agreements_sf),
    by = intersect(
      names(agencies_all),
      names(st_drop_geometry(all_agreements_sf))
    )
  )

st_write(all_agreements_sf, "287g_agreements.shp", delete_dsn = TRUE)
