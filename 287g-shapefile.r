library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(stringdist)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

participating_agencies <-
  read_excel("participatingAgencies02132026am.xlsx") |>
  mutate(status = "participating")

pending_agencies <-
  read_excel("pendingAgencies02132026am.xlsx") |>
  mutate(status = "pending")

# law enforcement agency identifiers crosswalk
load("35158-0001-Data.rda")
LEAIC <- da35158.0001

agencies_all <-
  bind_rows(participating_agencies, pending_agencies) |>
  mutate(
    state  = str_to_title(str_trim(STATE)),
    county = str_to_title(str_trim(COUNTY)),
    type_clean  = str_to_lower(str_trim(TYPE)),
    support_clean = str_to_lower(str_trim(`SUPPORT TYPE`)),
    has_addendum = !(is.na(ADDENDUM) | ADDENDUM %in% c("", "NA")),
    moa_pending  = str_detect(str_to_lower(str_trim(MOA)), "pending"),

    # facility detector for later point handling
    facility_detector = str_detect(
      str_to_lower(`LAW ENFORCEMENT AGENCY`),
      "(detention|detention center|correctional|corrections center|jail|workhouse|facility|processing center)"
    )
  )

# Fuzzy Matching Municipalities ------------------------------------------
muni_287g <- agencies_all |>
  filter(type_clean == "municipality") |>
  transmute(
    state  = str_to_title(str_squish(STATE)),
    county = str_to_title(str_squish(COUNTY)),
    agency = str_squish(`LAW ENFORCEMENT AGENCY`)
  ) |>
  mutate(
    agency_clean = agency |>
      str_to_lower() |>
      str_replace_all("&", " and ") |>
      str_replace_all("[^a-z0-9\\s]", " ") |>
      str_squish(),
    county_clean = county |>
      str_to_lower() |>
      str_replace_all("[^a-z0-9\\s]", " ") |>
      str_squish() |>
      str_remove("\\bcounty\\b$") |>
      str_squish()
  )

leaic_keys <- LEAIC |>
  transmute(
    leaic_state  = str_to_title(str_squish(STATENAME)),
    leaic_county = str_to_title(str_squish(COUNTYNAME)),
    leaic_name   = str_squish(NAME),
    # keep IDs for later joins to TIGER
    FSTATE, FCOUNTY, FPLACE, ORI9
  ) |>
  mutate(
    leaic_name_clean = leaic_name |>
      str_to_lower() |>
      str_replace_all("&", " and ") |>
      str_replace_all("[^a-z0-9\\s]", " ") |>
      str_squish(),
    leaic_county_clean = leaic_county |>
      str_to_lower() |>
      str_replace_all("[^a-z0-9\\s]", " ") |>
      str_squish() |>
      str_remove("\\bcounty\\b$") |>
      str_squish()
  )

candidates <- muni_287g |>
  inner_join(
    leaic_keys,
    by = c("state" = "leaic_state", "county_clean" = "leaic_county_clean"),
    relationship = "many-to-many"
  )

# fuzzy scoring on FULL NAMES (Jaro-Winkler)
matches_all <- candidates |>
  mutate(
    jw_dist = stringdist(agency_clean, leaic_name_clean, method = "jw", p = 0.1),
    jw_sim  = 1 - jw_dist
  ) |>
  filter(jw_dist <= 0.22) |>
  arrange(state, county, agency, jw_dist)

# best match per 287(g) agency (lowest distance)
best_match <- matches_all |>
  group_by(state, county, agency) |>
  slice_min(order_by = jw_dist, n = 1, with_ties = FALSE) |>
  ungroup()

# ambiguous matches: multiple close candidates
ambiguous <- matches_all |>
  group_by(state, county, agency) |>
  summarise(
    n_candidates = n(),
    best_dist = min(jw_dist),
    second_best_dist = sort(jw_dist)[pmin(2, n())],
    .groups = "drop"
  ) |>
  filter(n_candidates > 1 & (second_best_dist - best_dist) < 0.02)

# unmatched municipalities after thresholding
unmatched <- muni_287g |>
  anti_join(best_match, by = c("state", "county", "agency"))

# quick summaries
best_match |> summarise(matched = n_distinct(paste(state, county, agency)))
unmatched  |> summarise(unmatched = n())

# Assigning Geometry -----------------------------------------------------
agencies_all <- agencies_all |>
  group_by(state, `LAW ENFORCEMENT AGENCY`) |>
  mutate(
    geom_class = case_when(
      # state-level signers
      type_clean %in% c("state agency", "state") ~ "state_polygon",
      # county-level signers
      type_clean %in% c("county") ~ "county_polygon",
      # municipal-level signers
      type_clean %in% c("municipality") ~ "municipal_polygon",
      TRUE ~ "unknown"
    ),

    # flag for manual review (criteria from memo)
    needs_review = case_when(
      geom_class == "unknown" ~ TRUE,
      has_addendum ~ TRUE,
      moa_pending ~ TRUE,
      # state agencies with “jail enforcement model”
      geom_class == "state_polygon" & str_detect(support_clean, "jail enforcement") ~ TRUE,
      # county agencies with “jail enforcement model”
      geom_class == "county_polygon" & str_detect(support_clean, "jail enforcement") ~ TRUE,
      # municipal agencies with “jail enforcement model”
      geom_class == "municipal_polygon" & str_detect(support_clean, "jail enforcement") ~ TRUE,
      # agencies that appear multiple times
      n() > 1 ~ TRUE,
      # signer ambiguity
      type_clean == "county" &
        str_detect(str_to_lower(`LAW ENFORCEMENT AGENCY`),
          "(corrections|department of corrections|board of county commissioners)") ~ TRUE,
      TRUE ~ FALSE
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

# municipal polygons
municipal_agreements_sf <- agencies_all |>
  filter(geom_class == "municipal_polygon") |>
  left_join(places_sf, by = c("state", "place_guess")) |>
  st_as_sf()

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