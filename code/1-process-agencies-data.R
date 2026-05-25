library(tidyverse)
library(readxl)
library(sf)
library(tigris)
library(arrow)
library(sfarrow)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

source("code/functions.R")

YEAR <- 2024

# data loading -----------------------------------------------------------

agency_files <- list.files(
  "sheets",
  pattern = "^participatingAgencies.*\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(agency_files) == 0) {
  stop("No participating agencies file found.")
}

folder_stamp <- sub(
  ".*sheets_(\\d{8}_\\d{6}).*",
  "\\1",
  agency_files
)

folder_time <- as.POSIXct(
  folder_stamp,
  format = "%Y%m%d_%H%M%S",
  tz = "UTC"
)

latest_agency_file <- agency_files[which.max(folder_time)]

participating_agencies <- read_excel(latest_agency_file)

load("data/35158-0001-Data.rda")
LEAIC <- da35158.0001 |> as_tibble()

hifld <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-local-law-enforcement-facilities.parquet"
)

hifld_prisons <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/hifld-prisons.parquet"
)

jails_prisons <- arrow::read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/jails_prisons.parquet"
)

crime_data <- arrow::read_parquet(
  "data/crime-data-all-states.parquet"
)

facilities <- sfarrow::st_read_parquet(
  "https://github.com/deportationdata/ice-detention-facilities/raw/refs/heads/main/data/facilities-latest-sf.parquet"
)

university_boundaries <- st_read(
  "data/colleges-and-universities-campuses/CollegeUniversityCampuses.shp"
)

state_xwalk <- tibble(
  state_abbr = state.abb,
  state_full = state.name
) |>
  bind_rows(tibble(state_abbr = "DC", state_full = "District Of Columbia"))

counties_lookup <- tigris::counties(cb = TRUE, year = YEAR, class = "sf") |>
  st_transform(4326) |>
  transmute(
    src_county = str_to_title(NAME),
    src_county_fips = paste0(STATEFP, COUNTYFP),
    county_key_spatial = norm_place(NAME),
    geometry
  )

agencies_all <- participating_agencies |>
  mutate(
    state = str_to_title(str_trim(STATE)),
    county = str_to_title(str_trim(COUNTY)),
    type_clean = str_to_lower(str_trim(TYPE)),
    support_clean = str_to_lower(str_trim(`SUPPORT TYPE`)),
    has_addendum = !(is.na(ADDENDUM) | ADDENDUM %in% c("", "NA")),
    moa_pending = str_detect(str_to_lower(str_trim(MOA)), "pending")
  ) |>
  # fix data error - Pittsburgh is in Pennsylvania, not New Hampshire
  mutate(
    state = if_else(
      `LAW ENFORCEMENT AGENCY` == "Pittsburgh Police Department" &
        state == "New Hampshire",
      "Pennsylvania",
      state
    ),
    # expand territory name to match collapsed county polygon
    state = if_else(
      state == "Northern Mariana Islands",
      "Commonwealth of the Northern Mariana Islands",
      state
    )
  ) |>
  mutate(
    agency_level = case_when(
      type_clean %in% c("state agency", "state") ~ "state",
      type_clean == "county" ~ "county",
      type_clean == "municipality" ~ "municipal",
      TRUE ~ "unknown"
    ),

    is_university_agency = str_detect(
      str_to_lower(`LAW ENFORCEMENT AGENCY`),
      "university|college|campus|board of trustees"
    ),

    geom_class = case_when(
      support_clean == "task force model" &
        is_university_agency ~ "university_polygon",
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
    )
  ) |>
  add_count(state, `LAW ENFORCEMENT AGENCY`, name = "agency_count") |>
  mutate(
    needs_review = case_when(
      geom_class == "unknown" ~ TRUE,
      has_addendum ~ TRUE,
      moa_pending ~ TRUE,
      agency_count > 1 ~ TRUE,
      TRUE ~ FALSE
    )
  )

# source lookup tables ---------------------------------------------------

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

hifld_raw <- bind_rows(
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
  mutate(hifld_row_id = row_number()) |>
  left_join(state_xwalk, by = c("src_state" = "state_abbr")) |>
  mutate(
    src_state_full = coalesce(state_full, src_state),
    state_key = norm_state(src_state_full),
    agency_key_src = norm_key(src_name)
  )

hifld_counties_from_xy <- hifld_raw |>
  filter(!is.na(src_latitude), !is.na(src_longitude)) |>
  st_as_sf(
    coords = c("src_longitude", "src_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  st_join(counties_lookup, join = st_within, left = TRUE) |>
  st_drop_geometry() |>
  distinct(hifld_row_id, .keep_all = TRUE) |>
  select(
    hifld_row_id,
    src_county,
    src_county_fips,
    county_key_spatial
  )

hifld_tbl <- hifld_raw |>
  left_join(hifld_counties_from_xy, by = "hifld_row_id") |>
  mutate(
    county_key = county_key_spatial
  ) |>
  select(
    -hifld_row_id,
    -state_full,
    -county_key_spatial
  )

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

manual_points <- read_csv("data/manual-facility-points.csv")
manual_non_facility_polygons <- read_csv(
  "data/manual-non-facility-polygons.csv"
)

# save processed data ----------------------------------------------------

arrow::write_parquet(agencies_all, "data/processed/agencies_all.parquet")
arrow::write_parquet(leaic_tbl, "data/processed/leaic_tbl.parquet")
arrow::write_parquet(hifld_tbl, "data/processed/hifld_tbl.parquet")
arrow::write_parquet(crime_lookup, "data/processed/crime_lookup.parquet")
arrow::write_parquet(manual_points, "data/processed/manual_points.parquet")
arrow::write_parquet(
  manual_non_facility_polygons,
  "data/processed/manual_non_facility_polygons.parquet"
)
saveRDS(state_xwalk, "data/processed/state_xwalk.rds")
sfarrow::st_write_parquet(
  facilities,
  "data/processed/facilities.parquet"
)
sfarrow::st_write_parquet(
  university_boundaries,
  "data/processed/university_boundaries.parquet"
)
