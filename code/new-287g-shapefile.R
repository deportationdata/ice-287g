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
# write_feather(crime_data, "crime_data_all_states.feather")

participating_agencies <-
  read_excel("participatingAgencies03182026am.xlsx") |>
  mutate(status = "participating")

pending_agencies <-
  read_excel("pendingAgencies02132026am.xlsx") |>
  mutate(status = "pending")

load("35158-0001-Data.rda")
LEAIC <- da35158.0001

hifld <- arrow::read_feather(
  "data/ice-detention-facilities/data/hifld-local-law-enforcement-facilities.feather"
)

hifld_prisons <- arrow::read_feather(
  "data/ice-detention-facilities/data/hifld-prisons.feather"
)

jails_prisons <- arrow::read_feather(
  "data/ice-detention-facilities/data/jails_prisons.feather"
)

crime_data <- arrow::read_feather("crime_data_all_states.feather")

# helper functions -------------------------------------------------------
norm_key <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("\\bst\\.?\\b", "saint") |>
    str_replace_all("\\bpd\\b", " ") |>
    str_replace_all("\\b(county|city|town|village|borough|township|municipality)\\b", " ") |>
    str_replace_all("\\b(police|dept|department|public|safety|office)\\b", " ") |>
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
    str_replace_all("\\b(county|city|town|village|borough|township|municipality)\\b", " ") |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish() |>
    str_replace_all("\\s+", " ")
}

extract_city_guess <- function(x) {
  s <- str_squish(x)
  s <- str_remove(s, regex("(?i)^\\s*city\\s+of\\s+"))
  s <- str_remove(s, regex("(?i)\\b(police|pd|police dept\\.?|police department|department|dept|public safety|office)\\b.*$"))
  s <- str_squish(s)
  s <- na_if(s, "")
  str_to_title(s)
}

# prepare agencies -------------------------------------------------------
agencies_all <-
  bind_rows(participating_agencies, pending_agencies) |>
  mutate(
    state         = str_to_title(str_trim(STATE)),
    county        = str_to_title(str_trim(COUNTY)),
    type_clean    = str_to_lower(str_trim(TYPE)),
    support_clean = str_to_lower(str_trim(`SUPPORT TYPE`)),
    has_addendum  = !(is.na(ADDENDUM) | ADDENDUM %in% c("", "NA")),
    moa_pending   = str_detect(str_to_lower(str_trim(MOA)), "pending")
  ) |>
  # fix data error — Pittsburgh is in Pennsylvania, not New Hampshire
  mutate(
    state = if_else(
      `LAW ENFORCEMENT AGENCY` == "Pittsburgh Police Department" & state == "New Hampshire",
      "Pennsylvania",
      state
    )
  ) |>
  group_by(state, `LAW ENFORCEMENT AGENCY`) |>
  mutate(
    geom_class = case_when(
      support_clean %in% c("jail enforcement model", "warrant service officer") ~ "facility_point",
      type_clean %in% c("state agency", "state")                                ~ "state_polygon",
      type_clean == "county"                                                     ~ "county_polygon",
      type_clean == "municipality"                                               ~ "municipal_polygon",
      TRUE                                                                       ~ "unknown"
    ),
    agency_level = case_when(
      type_clean %in% c("state agency", "state") ~ "state",
      type_clean == "county"                     ~ "county",
      type_clean == "municipality"               ~ "municipal",
      TRUE                                       ~ "unknown"
    ),
    needs_review = case_when(
      geom_class == "unknown"                                                    ~ TRUE,
      has_addendum                                                               ~ TRUE,
      moa_pending                                                                ~ TRUE,
      n() > 1                                                                    ~ TRUE,
      type_clean == "county" & str_detect(
        str_to_lower(`LAW ENFORCEMENT AGENCY`),
        "(corrections|department of corrections|board of county commissioners)"
      )                                                                          ~ TRUE,
      !is.na(county) & str_detect(county, ",")                                  ~ TRUE,
      TRUE                                                                       ~ FALSE
    )
  ) |>
  ungroup()

# source tables ----------------------------------------------------------
leaic_tbl <- LEAIC |>
  transmute(
    leaic_state  = str_to_title(str_squish(STATENAME)),
    leaic_county = str_to_title(str_squish(COUNTYNAME)),
    leaic_name   = str_squish(NAME),
    FSTATE, FCOUNTY, FPLACE, ORI9, AGCYTYPE, SUBTYPE1, SUBTYPE2, COMMENT
  ) |>
  mutate(
    state_key      = norm_state(leaic_state),
    county_key     = norm_place(leaic_county),
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
      src_dataset    = "hifld",
      src_id         = as.character(hifld_id),
      src_name       = str_squish(name),
      src_address    = address,
      src_city       = str_squish(city),
      src_state      = str_squish(state),
      src_zip        = zip,
      src_type       = type,
      src_status     = status,
      src_population = NA_real_,
      src_hold_72    = NA,
      src_latitude   = latitude,
      src_longitude  = longitude,
      src_date       = date
    ),
  hifld_prisons |>
    transmute(
      src_dataset    = "hifld_prisons",
      src_id         = as.character(hifld_id),
      src_name       = str_squish(name),
      src_address    = address,
      src_city       = str_squish(city),
      src_state      = str_squish(state),
      src_zip        = zip,
      src_type       = type,
      src_status     = status,
      src_population = population,
      src_hold_72    = NA,
      src_latitude   = latitude,
      src_longitude  = longitude,
      src_date       = date
    ),
  jails_prisons |>
    transmute(
      src_dataset    = "jails_prisons",
      src_id         = as.character(bjs_facility_ID),
      src_name       = str_squish(name),
      src_address    = address,
      src_city       = str_squish(city),
      src_state      = str_squish(state),
      src_zip        = zip,
      src_type       = NA_character_,
      src_status     = NA_character_,
      src_population = NA_real_,
      src_hold_72    = hold_72,
      src_latitude   = NA_real_,
      src_longitude  = NA_real_,
      src_date       = date
    )
) |>
  left_join(state_xwalk, by = c("src_state" = "state_abbr")) |>
  mutate(
    state_key      = norm_state(src_state),
    county_key     = norm_place(src_city),
    agency_key_src = norm_key(src_name)
  ) |>
  select(-state_full)

crime_lookup <- crime_data |>
  transmute(
    ori          = str_squish(ori),
    crime_lat    = latitude,
    crime_lon    = longitude,
    agency_name  = str_squish(agency_name),
    agency_type  = agency_type_name,
    nibrs_start  = nibrs_start_date,
    state_abbr   = str_squish(state_abbr)
  ) |>
  distinct(ori, .keep_all = TRUE)

# pull boundary geometries -----------------------------------------------
YEAR <- 2024

states_sf <- tigris::states(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(state = str_to_title(NAME), statefp = STATEFP, geometry)

counties_sf <- tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(state = str_to_title(STATE_NAME), county = str_to_title(NAME),
            statefp = STATEFP, countyfp = COUNTYFP, geometry)

places_sf <- tigris::places(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(state = str_to_title(STATE_NAME), place_guess = str_to_title(NAME),
            statefp = STATEFP, placefp = PLACEFP, geometry)

cousubs_sf <- map_dfr(unique(states_sf$statefp), function(fp) {
  tigris::county_subdivisions(state = fp, cb = TRUE, year = YEAR, class = "sf")
}) |>
  transmute(state = str_to_title(STATE_NAME), place_guess = str_to_title(NAME),
            statefp = STATEFP, placefp = COUSUBFP, geometry)

# normalized places lookup (places preferred over cousubs) ---------------
places_lookup <- bind_rows(
  places_sf  |> mutate(src = "place"),
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

# county centroids (fallback for unmatched municipalities/facilities) ----
county_centroids <- counties_sf |>
  st_transform(5070) |>
  mutate(centroid = st_centroid(geometry)) |>
  st_drop_geometry() |>
  st_as_sf(sf_column_name = "centroid") |>
  st_transform(4326) |>
  mutate(
    state_key  = norm_state(state),
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
    state_key  = norm_state(state),
    place_key  = norm_place(city_guess),
    county_key = norm_place(county)
  ) |>
  left_join(
    places_lookup |> select(state_key, place_key, geometry, src),
    by = c("state_key", "place_key")
  ) |>
  left_join(
    county_centroids |> select(state_key, county_key, geometry) |>
      rename(county_geometry = geometry),
    by = c("state_key", "county_key")
  ) |>
  mutate(
    missing_place = is.na(src) | st_is_empty(geometry) | is.na(st_dimension(geometry)),
    src = if_else(missing_place, "county_centroid_fallback", src)
  ) |>
  (\(df) {
    df$geometry[df$missing_place] <- df$county_geometry[df$missing_place]
    df
  })() |>
  select(-county_geometry, -missing_place) |>
  mutate(needs_review = needs_review | is.na(geometry) | st_is_empty(geometry)) |>
  st_as_sf()

# facility point matching ------------------------------------------------
fac_287g <- agencies_all |>
  filter(geom_class == "facility_point") |>
  transmute(
    state         = state,
    county        = county,
    agency        = `LAW ENFORCEMENT AGENCY`,
    agency_level  = agency_level,
    needs_review  = needs_review,
    status        = status,
    support_clean = support_clean
  ) |>
  mutate(
    state_key  = norm_state(state),
    county_key = norm_place(county),
    agency_key = norm_key(agency)
  )

# layer 1: exact match via LEAIC (state + county + agency name)
fac_leaic_exact <- fac_287g |>
  inner_join(
    leaic_tbl,
    by = c("state_key", "county_key", "agency_key" = "agency_key_src"),
    relationship = "many-to-many"
  ) |>
  group_by(state, county, agency) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(fac_source = "leaic", match_type = "exact", match_score = 1)

after_fac_leaic_exact <- fac_287g |>
  anti_join(fac_leaic_exact, by = c("state", "county", "agency"))

# layer 2: fuzzy match via LEAIC (state + county, fuzzy agency name)
fac_leaic_fuzzy <- after_fac_leaic_exact |>
  inner_join(
    leaic_tbl,
    by = c("state_key", "county_key"),
    relationship = "many-to-many"
  ) |>
  mutate(dist = stringdist(agency_key, agency_key_src, method = "jw", p = 0.1)) |>
  group_by(state, county, agency) |>
  slice_min(dist, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(dist <= 0.12) |>
  mutate(fac_source = "leaic", match_type = "fuzzy", match_score = 1 - dist)

after_fac_leaic <- after_fac_leaic_exact |>
  anti_join(fac_leaic_fuzzy, by = c("state", "county", "agency"))

# layer 3: exact match via HIFLD/jails (has coordinates)
fac_hifld_exact <- after_fac_leaic |>
  inner_join(
    hifld_tbl,
    by = c("state_key", "agency_key" = "agency_key_src"),
    relationship = "many-to-many"
  ) |>
  group_by(state, county, agency) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(fac_source = src_dataset, match_type = "exact", match_score = 1)

after_fac_hifld_exact <- after_fac_leaic |>
  anti_join(fac_hifld_exact, by = c("state", "county", "agency"))

# layer 4: fuzzy match via HIFLD/jails
fac_hifld_fuzzy <- after_fac_hifld_exact |>
  inner_join(
    hifld_tbl,
    by = c("state_key"),
    relationship = "many-to-many"
  ) |>
  mutate(dist = stringdist(agency_key, agency_key_src, method = "jw", p = 0.1)) |>
  group_by(state, county, agency) |>
  slice_min(dist, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(dist <= 0.12) |>
  mutate(fac_source = src_dataset, match_type = "fuzzy", match_score = 1 - dist)

# combine and pick best match across all layers
fac_all_matches <- bind_rows(
  fac_leaic_exact,
  fac_leaic_fuzzy,
  fac_hifld_exact,
  fac_hifld_fuzzy  
) |>
  mutate(
    src_rank = case_when(
      fac_source == "leaic"         ~ 1L,
      fac_source == "hifld"         ~ 2L,
      fac_source == "hifld_prisons" ~ 3L,
      fac_source == "jails_prisons" ~ 4L,
      TRUE                          ~ 99L
    )
  ) |>
  group_by(state, county, agency) |>
  arrange(src_rank, desc(match_score)) |>
  slice_head(n = 1) |>
  ungroup() |>
  # bridge LEAIC matches to crime data coordinates via ORI9
  left_join(
    crime_lookup |> select(ori, crime_lat, crime_lon),
    by = c("ORI9" = "ori")
  ) |>
  mutate(
    # prefer HIFLD/jails coords, fall back to crime data coords
    src_latitude  = coalesce(src_latitude, crime_lat),
    src_longitude = coalesce(src_longitude, crime_lon)
  ) |>
  select(-crime_lat, -crime_lon)

fac_unmatched <- fac_287g |>
  anti_join(fac_all_matches, by = c("state", "county", "agency"))

# assign point geometries ------------------------------------------------
# matched facilities with direct coordinates from HIFLD/jails
fac_with_coords <- fac_all_matches |>
  filter(!is.na(src_latitude) & !is.na(src_longitude)) |>
  st_as_sf(coords = c("src_longitude", "src_latitude"), crs = 4326) |>
  mutate(src = fac_source)

# LEAIC-only matches — try crime data coords first, then county centroid
fac_leaic_only <- fac_all_matches |>
  filter(is.na(src_latitude) | is.na(src_longitude)) |>
  mutate(
    state_key  = norm_state(state),
    county_key = norm_place(county)
  ) |>
  left_join(
    county_centroids |> select(state_key, county_key, geometry),
    by = c("state_key", "county_key")
  ) |>
  mutate(
    needs_review = TRUE,
    src          = "county_centroid_fallback"
  ) |>
  st_as_sf()

# unmatched facilities — county centroid fallback, flagged for review
fac_no_match_sf <- fac_unmatched |>
  mutate(
    state_key  = norm_state(state),
    county_key = norm_place(county)
  ) |>
  left_join(
    county_centroids |> select(state_key, county_key, geometry),
    by = c("state_key", "county_key")
  ) |>
  mutate(
    needs_review = TRUE,
    src          = "county_centroid_fallback_unmatched"
  ) |>
  st_as_sf()

facility_agreements_sf <- bind_rows(
  fac_with_coords,
  fac_leaic_only,
  fac_no_match_sf
)

# diagnostics ------------------------------------------------------------
agencies_all |> count(geom_class)
fac_all_matches |> count(fac_source, match_type)
fac_unmatched |> select(state, county, agency) |> arrange(state, county)

municipal_agreements_sf |>
  st_drop_geometry() |>
  filter(needs_review) |>
  select(state, county, `LAW ENFORCEMENT AGENCY`, city_guess, src) |>
  arrange(state, county)

# combine and write ------------------------------------------------------
all_agreements_sf <- bind_rows(
  state_agreements_sf    |> st_transform(4326),
  county_agreements_sf   |> st_transform(4326),
  municipal_agreements_sf |> st_transform(4326),
  facility_agreements_sf  |> st_transform(4326)
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
    by = intersect(names(agencies_all), names(st_drop_geometry(all_agreements_sf)))
  )

st_write(all_agreements_sf, "287g_agreements.shp", delete_dsn = TRUE)