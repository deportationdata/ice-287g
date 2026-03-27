library(readxl)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(stringdist)
library(sf)
library(tigris)
library(purrr)

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

# law enforcement agency identifiers crosswalk
load("35158-0001-Data.rda")
LEAIC <- da35158.0001

# homeland infrastructure foundation-level data
hifld <- arrow::read_feather(
  "data/ice-detention-facilities/data/hifld-local-law-enforcement-facilities.feather"
)

hifld_prisons <- arrow::read_feather(
  "data/ice-detention-facilities/data/hifld-prisons.feather"
)

# jails and prisons data
jails_prisons <- arrow::read_feather(
  "data/ice-detention-facilities/data/jails_prisons.feather"
)

agencies_all <-
  bind_rows(participating_agencies, pending_agencies) |>
  mutate(
    state = str_to_title(str_trim(STATE)),
    county = str_to_title(str_trim(COUNTY)),
    type_clean = str_to_lower(str_trim(TYPE)),
    support_clean = str_to_lower(str_trim(`SUPPORT TYPE`)),
    has_addendum = !(is.na(ADDENDUM) | ADDENDUM %in% c("", "NA")),
    moa_pending = str_detect(str_to_lower(str_trim(MOA)), "pending"),

    # facility detector for later point handling
    facility_detector = str_detect(
      str_to_lower(`LAW ENFORCEMENT AGENCY`),
      "(detention|detention center|correctional|corrections center|jail|workhouse|facility|processing center)"
    )
  )

# fix error - Pittsburgh is in Pennsylvania, not New Hampshire
agencies_all <- agencies_all |>
  mutate(
    state = if_else(
      `LAW ENFORCEMENT AGENCY` == "Pittsburgh Police Department" & state == "New Hampshire",
      "Pennsylvania",
      state
    )
  )

# Direct String Matching Municipalities ----------------------------------
# helper functions
norm_key <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("\\bst\\b", "saint") |>
    str_replace_all("\\b(county|city|town|village|borough|township|municipality)\\b", " ") |>
    str_replace_all("\\b(police|pd|dept|department|public|safety|office)\\b", " ") |>
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

match_exact <- function(x, y, by_cols, source_name, keep_cols = NULL) {
  out <- x |>
    inner_join(
      y,
      by = by_cols,
      relationship = "many-to-many",
      suffix = c("", "_src")
    )

  if (!is.null(keep_cols)) {
    out <- out |> select(any_of(c(names(x), keep_cols)))
  }

  out |>
    group_by(state, county, agency) |>
    slice_head(n = 1) |>
    ungroup() |>
    mutate(
      source = source_name,
      match_type = "exact",
      match_score = 1
    )
}

match_fuzzy <- function(x, y, join_cols, source_name, threshold = 0.12, keep_cols = NULL) {
  candidates <- x |>
    inner_join(
      y,
      by = join_cols,
      relationship = "many-to-many",
      suffix = c("", "_src")
    )

  if (!is.null(keep_cols)) {
    candidates <- candidates |> select(any_of(c(names(x), keep_cols)))
  }

  candidates |>
    mutate(
      dist = stringdist(agency_key, agency_key_src, method = "jw", p = 0.1)
    ) |>
    group_by(state, county, agency) |>
    slice_min(dist, n = 1, with_ties = FALSE) |>
    ungroup() |>
    filter(dist <= threshold) |>
    mutate(
      source = source_name,
      match_type = "fuzzy",
      match_score = 1 - dist
    )
}

# 287(g) municipalities
muni_287g <- agencies_all |>
  filter(type_clean == "municipality") |>
  transmute(
    state = str_to_title(str_squish(STATE)),
    county = str_to_title(str_squish(COUNTY)),
    agency = str_squish(`LAW ENFORCEMENT AGENCY`)
  ) |>
  mutate(
    state_key  = norm_state(state),
    county_key = norm_place(county),
    agency_key = norm_key(agency),
    city_guess = extract_city_guess(agency),
    city_key   = norm_place(city_guess)
  )

# clean county column
muni_287g <- muni_287g |>
  mutate(county = na_if(str_to_lower(str_trim(county)), "na"),
         county = na_if(county, "#na"),
         county_key = norm_place(county))

# source tables
leaic_tbl <- LEAIC |>
  transmute(
    leaic_state = str_to_title(str_squish(STATENAME)),
    leaic_county = str_to_title(str_squish(COUNTYNAME)),
    leaic_name = str_squish(NAME),
    FSTATE, FCOUNTY, FPLACE, ORI9, AGCYTYPE, SUBTYPE1, SUBTYPE2, COMMENT
  ) |>
  mutate(
    state_key = norm_state(leaic_state),
    county_key = norm_place(leaic_county),
    agency_key_src = norm_key(leaic_name)
  )

hifld_tbl <- hifld |>
  transmute(
    src_id = as.character(hifld_id),
    src_name = str_squish(name),
    src_address = address,
    src_city = str_squish(city),
    src_state = str_squish(state),
    src_zip = zip,
    src_type = type,
    src_status = status,
    src_latitude = latitude,
    src_longitude = longitude,
    src_date = date
  ) |>
  mutate(
    state_key = norm_state(src_state),
    city_key = norm_place(src_city),
    agency_key_src = norm_key(src_name)
  )

# combined prisons and jails
facilities_tbl <- bind_rows(
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
      src_latitude = NA_real_,
      src_longitude = NA_real_,
      src_date = date
    )
) |>
  mutate(
    state_key = norm_state(src_state),
    city_key = norm_place(src_city),
    agency_key_src = norm_key(src_name)
  )

# layer 1: leaic
leaic_keep <- c(
  "leaic_state", "leaic_county", "leaic_name",
  "FSTATE", "FCOUNTY", "FPLACE", "ORI9", "AGCYTYPE", "SUBTYPE1", "SUBTYPE2", "COMMENT",
  "agency_key_src"
)

leaic_exact <- match_exact(
  x = muni_287g,
  y = leaic_tbl,
  by_cols = c("state_key", "county_key", "agency_key" = "agency_key_src"),
  source_name = "LEAIC",
  keep_cols = leaic_keep
)

after_leaic_exact <- muni_287g |>
  anti_join(leaic_exact, by = c("state", "county", "agency"))

leaic_fuzzy <- match_fuzzy(
  x = after_leaic_exact,
  y = leaic_tbl,
  join_cols = c("state_key", "county_key"),
  source_name = "LEAIC",
  threshold = 0.12,
  keep_cols = leaic_keep
)

after_leaic <- after_leaic_exact |>
  anti_join(leaic_fuzzy, by = c("state", "county", "agency"))

# layer 2: hifld law enforcement
hifld_keep <- c(
  "src_id", "src_name", "src_address", "src_city", "src_state", "src_zip",
  "src_type", "src_status", "src_latitude", "src_longitude", "src_date",
  "agency_key_src"
)

hifld_exact <- match_exact(
  x = after_leaic,
  y = hifld_tbl,
  by_cols = c("state_key", "city_key", "agency_key" = "agency_key_src"),
  source_name = "HIFLD",
  keep_cols = hifld_keep
)

after_hifld_exact <- after_leaic |>
  anti_join(hifld_exact, by = c("state", "county", "agency"))

hifld_candidates_base <- after_hifld_exact |>
  inner_join(
    hifld_tbl,
    by = c("state_key"),
    relationship = "many-to-many",
    suffix = c("", "_src")
  ) |>
  filter(is.na(city_key) | city_key == "" | city_key == city_key_src)

hifld_fuzzy <- hifld_candidates_base |>
  mutate(
    dist = stringdist(agency_key, agency_key_src, method = "jw", p = 0.1)
  ) |>
  group_by(state, county, agency) |>
  slice_min(dist, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(dist <= 0.12) |>
  mutate(
    source = "HIFLD",
    match_type = "fuzzy",
    match_score = 1 - dist
  ) |>
  select(any_of(c(names(after_hifld_exact), hifld_keep, "source", "match_type", "match_score", "dist")))

after_hifld <- after_hifld_exact |>
  anti_join(hifld_fuzzy, by = c("state", "county", "agency"))

# layer 3: jails and prisons
fac_keep <- c(
  "src_dataset", "src_id", "src_name", "src_address", "src_city", "src_state", "src_zip",
  "src_type", "src_status", "src_latitude", "src_longitude", "src_date",
  "agency_key_src"
)

fac_exact <- match_exact(
  x = after_hifld,
  y = facilities_tbl,
  by_cols = c("state_key", "city_key", "agency_key" = "agency_key_src"),
  source_name = "FACILITIES",
  keep_cols = fac_keep
) |>
  mutate(source = src_dataset)

after_fac_exact <- after_hifld |>
  anti_join(fac_exact, by = c("state", "county", "agency"))

fac_candidates_base <- after_fac_exact |>
  inner_join(
    facilities_tbl,
    by = c("state_key"),
    relationship = "many-to-many",
    suffix = c("", "_src")
  ) |>
  filter(is.na(city_key) | city_key == "" | city_key == city_key_src)

fac_fuzzy <- fac_candidates_base |>
  mutate(
    dist = stringdist(agency_key, agency_key_src, method = "jw", p = 0.1)
  ) |>
  group_by(state, county, agency) |>
  slice_min(dist, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(dist <= 0.12) |>
  mutate(
    source = src_dataset,
    match_type = "fuzzy",
    match_score = 1 - dist
  ) |>
  select(any_of(c(names(after_fac_exact), fac_keep, "source", "match_type", "match_score", "dist")))

# combine and pick best match
all_matches <- bind_rows(
  leaic_exact,
  leaic_fuzzy,
  hifld_exact,
  hifld_fuzzy,
  fac_exact,
  fac_fuzzy
)

final_match <- all_matches |>
  mutate(
    source_rank = case_when(
      source == "LEAIC" ~ 1L,
      source == "HIFLD" ~ 2L,
      source %in% c("hifld_prisons", "jails_prisons") ~ 3L,
      TRUE ~ 99L
    )
  ) |>
  group_by(state, county, agency) |>
  arrange(source_rank, desc(match_score)) |>
  slice_head(n = 1) |>
  ungroup()

unmatched_final <- muni_287g |>
  anti_join(final_match, by = c("state", "county", "agency"))

# prepare for geometry assignment (for LEAIC matches only, pad FIPS for joins later)
final_match <- final_match |>
  mutate(
    FSTATE_chr = if_else(!is.na(FSTATE), str_pad(as.character(FSTATE), 2, pad = "0"), NA_character_),
    FCOUNTY_chr = if_else(!is.na(FCOUNTY), str_pad(as.character(FCOUNTY), 3, pad = "0"), NA_character_),
    FPLACE_chr = if_else(!is.na(FPLACE), str_pad(as.character(FPLACE), 5, pad = "0"), NA_character_),
    county_geoid = if_else(!is.na(FSTATE_chr) & !is.na(FCOUNTY_chr), paste0(FSTATE_chr, FCOUNTY_chr), NA_character_),
    place_geoid = if_else(!is.na(FSTATE_chr) & !is.na(FPLACE_chr), paste0(FSTATE_chr, FPLACE_chr), NA_character_)
  )

# diagnostics
final_match |> count(source, match_type)

unmatched_final |>
  select(state, county, agency) |>
  arrange(state, county, agency)

# Assigning Geometry -----------------------------------------------------
# diagnostics
agencies_all |>
  count(support_clean, type_clean) |>
  arrange(support_clean, type_clean)

agencies_all <- agencies_all |>
  group_by(state, `LAW ENFORCEMENT AGENCY`) |>
  mutate(
    geom_class = case_when(
      # facility points — jurisdiction limited to a single facility
      support_clean %in% c("jail enforcement model", "warrant service officer") ~ "facility_point",
      # task force model and everything else - polygon based on agency type
      type_clean %in% c("state agency", "state")  ~ "state_polygon",
      type_clean == "county"                       ~ "county_polygon",
      type_clean == "municipality"                 ~ "municipal_polygon",
      TRUE                                         ~ "unknown"
    ),
    # preserve agency-level type for facility points
    agency_level = case_when(
      type_clean %in% c("state agency", "state") ~ "state",
      type_clean == "county"                     ~ "county",
      type_clean == "municipality"               ~ "municipal",
      TRUE                                       ~ "unknown"
    )
    needs_review = case_when(
      geom_class == "unknown"                                                         ~ TRUE,
      has_addendum                                                                    ~ TRUE,
      moa_pending                                                                     ~ TRUE,
      # agency appears multiple times (may have overlapping agreements)
      n() > 1                                                                         ~ TRUE,
      # signer ambiguity
      type_clean == "county" & str_detect(
        str_to_lower(`LAW ENFORCEMENT AGENCY`),
        "(corrections|department of corrections|board of county commissioners)"
      )                                                                               ~ TRUE,
      # multi-county facilities (county field contains a comma)
      !is.na(county) & str_detect(county, ",")                                       ~ TRUE,
      TRUE                                                                            ~ FALSE
    )
  ) |>
  ungroup()

# Creating Shapefile -----------------------------------------------------
YEAR <- 2024 # use one consistent year

# pull boundary geometries
states_sf <- tigris::states(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(state = str_to_title(NAME),
            statefp = STATEFP,
            geometry)

counties_sf <- tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(state = str_to_title(STATE_NAME),
            county = str_to_title(NAME),
            statefp = STATEFP,
            countyfp = COUNTYFP,
            geometry)

places_sf <- tigris::places(cb = TRUE, year = YEAR, class = "sf") |>
  transmute(state = str_to_title(STATE_NAME),
            place_guess = str_to_title(NAME),
            statefp = STATEFP,
            placefp = PLACEFP,
            geometry)

cousubs_sf <- map_dfr(unique(states_sf$statefp), function(fp) {
  tigris::county_subdivisions(state = fp, cb = TRUE, year = YEAR, class = "sf")
}) |>
  transmute(state = str_to_title(STATE_NAME),
            place_guess = str_to_title(NAME),
            statefp = STATEFP,
            placefp = COUSUBFP,
            geometry)

# join agencies to correct geometry
# state polygons
state_agreements_sf <- agencies_all |>
  filter(geom_class == "state_polygon") |>
  left_join(states_sf, by = "state") |>
  st_as_sf()

# county polygons (requires COUNTY present + matches tigris county name format)
county_agreements_sf <- agencies_all |>
  filter(geom_class == "county_polygon") |>
  left_join(counties_sf, by = c("state", "county")) |>
  st_as_sf()

# build a normalized places lookup from both places and county subdivisions
places_lookup <- bind_rows(
  places_sf |> mutate(src = "place"),
  cousubs_sf |> mutate(src = "cousub")
) |>
  mutate(
    state_key = norm_state(state),
    place_key = norm_place(place_guess)
  )

# deduplicate before joining - prefer places over cousubs when conflict
places_lookup_deduped <- places_lookup |>
  mutate(src_rank = if_else(src == "place", 1L, 2L)) |>
  group_by(state_key, place_key) |>
  slice_min(src_rank, n = 1, with_ties = FALSE) |>
  ungroup()

# county centroid fallback
county_centroids <- counties_sf |>
  st_transform(5070) |>  # Albers Equal Area, good for CONUS
  mutate(centroid = st_centroid(geometry)) |>
  st_drop_geometry() |>
  st_as_sf(sf_column_name = "centroid") |>
  st_transform(4326) |>  # transform back to WGS84 to match everything else
  mutate(
    state_key  = norm_state(state),
    county_key = norm_place(county)
  ) |>
  rename(geometry = centroid)

municipal_agreements_sf <- agencies_all |>
  filter(geom_class == "municipal_polygon") |>
  mutate(
    city_guess = extract_city_guess(`LAW ENFORCEMENT AGENCY`),
    state_key  = norm_state(state),
    place_key  = norm_place(city_guess),
    county_key = norm_place(county)
  ) |>
  left_join(
    places_lookup_deduped |> select(state_key, place_key, geometry, src),
    by = c("state_key", "place_key")
  ) |>
  left_join(
    county_centroids |> select(state_key, county_key, geometry) |> rename(county_geometry = geometry),
    by = c("state_key", "county_key")
  ) |>
  mutate(
    missing_place = is.na(src) | st_is_empty(geometry) | is.na(st_dimension(geometry)),
    src = if_else(missing_place, "county_centroid_fallback", src)
  ) |>
  # use base R to swap in fallback geometry where needed
  (\(df) {
    df$geometry[df$missing_place] <- df$county_geometry[df$missing_place]
    df
  })() |>
  select(-county_geometry, -missing_place) |>
  st_as_sf()

# needs manual review
municipal_agreements_sf |>
  st_drop_geometry() |>
  filter(needs_review) |>
  select(state, county, `LAW ENFORCEMENT AGENCY`, city_guess, src) |>
  arrange(state, county)

# combine, verify, and write shapefile
all_agreements_sf <- bind_rows(
  state_agreements_sf,
  county_agreements_sf,
  municipal_agreements_sf
) |>
  # keep only rows where geometry actually resolved
  filter(!is.na(geometry)) |>
  st_make_valid() |>
  st_transform(4326)

# unresolved geometry
unmatched <- agencies_all |>
  filter(geom_class %in% c("state_polygon", "county_polygon", "municipal_polygon")) |>
  anti_join(st_drop_geometry(all_agreements_sf),
            by = intersect(names(agencies_all), names(st_drop_geometry(all_agreements_sf))))

st_write(all_agreements_sf, "287g_agreements.shp", delete_dsn = TRUE)