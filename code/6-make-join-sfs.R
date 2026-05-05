source("code/2-make-state-sf.R")
source("code/3-make-county-sf.R")
source("code/4-make-municipal-sf.R")
source("code/5-make-university-sf.R")

# facility source tables -------------------------------------------------

facilities_tbl <- facilities |>
  st_drop_geometry() |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
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

# 287(g) facility-model agencies -----------------------------------------

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
    agency_key = norm_key(agency),
    facility_guess = extract_facility_guess(agency),
    facility_guess_key = norm_key(facility_guess)
  )

# split DOC agencies out before matching ---------------------------------
# state DOC agencies need 1-to-many matching logic and are handled separately

doc_pattern <- "department of corrections|correctional services|public safety & corrections|division of corrections|department of public safety"

is_doc_agency <- function(x) str_detect(str_to_lower(x), doc_pattern)

fac_287g_doc <- fac_287g |>
  filter(is_doc_agency(agency) & agency_level == "state")

fac_287g_non_doc <- fac_287g |>
  filter(!(is_doc_agency(agency) & agency_level == "state"))

# exact matching (non-DOC only) ------------------------------------------

facility_exact_matches <- fac_287g_non_doc |>
  inner_join(
    facility_sources_exact,
    by = c("state_key", "county_key", "agency_key" = "facility_key"),
    relationship = "many-to-many"
  ) |>
  mutate(match_type = "exact_state_county_agency_name", match_score = 1) |>
  group_by(state, county, agency, support_clean) |>
  arrange(source_rank) |>
  slice_head(n = 1) |>
  ungroup()

facility_unmatched_after_exact <- fac_287g_non_doc |>
  anti_join(facility_exact_matches, by = c("state", "county", "agency"))

# fuzzy match within state and county (non-DOC only) ---------------------

facility_fuzzy_county <- facility_unmatched_after_exact |>
  inner_join(
    facility_sources_exact,
    by = c("state_key", "county_key"),
    relationship = "many-to-many"
  ) |>
  mutate(
    dist_agency = stringdist(agency_key, facility_key, method = "jw", p = 0.1),
    dist_guess = stringdist(
      facility_guess_key,
      facility_key,
      method = "jw",
      p = 0.1
    ),
    match_dist = pmin(dist_agency, dist_guess, na.rm = TRUE)
  ) |>
  filter(match_dist <= 0.22) |>
  group_by(state, county, agency) |>
  arrange(match_dist, source_rank) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_type = "fuzzy_state_county",
    match_score = 1 - match_dist,
    needs_review = TRUE
  )

facility_unmatched_after_fuzzy_county <- facility_unmatched_after_exact |>
  anti_join(facility_fuzzy_county, by = c("state", "county", "agency"))

# fuzzy match within state only (non-DOC only, looser fallback) ----------

facility_fuzzy_state <- facility_unmatched_after_fuzzy_county |>
  inner_join(
    facility_sources_exact,
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  mutate(
    dist_agency = stringdist(agency_key, facility_key, method = "jw", p = 0.1),
    dist_guess = stringdist(
      facility_guess_key,
      facility_key,
      method = "jw",
      p = 0.1
    ),
    match_dist = pmin(dist_agency, dist_guess, na.rm = TRUE)
  ) |>
  filter(match_dist <= 0.12) |>
  group_by(state, county, agency) |>
  arrange(match_dist, source_rank) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    match_type = "fuzzy_state",
    match_score = 1 - match_dist,
    needs_review = TRUE
  )

# DOC: match to all facilities in state ----------------------------------

doc_facility_pattern <- "correctional|prison|penitentiary|detention|work camp|correctional facility|correctional center"

doc_matches <- fac_287g_doc |>
  inner_join(
    facility_sources_exact,
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  filter(str_detect(str_to_lower(facility_name), doc_facility_pattern)) |>
  arrange(state, county, agency, source_rank) |>
  mutate(
    match_type = "state_doc_all_facilities",
    match_score = 1,
    needs_review = TRUE
  )

# DOC: fuzzy fallback for state DOC agencies with no exact facility match ----

doc_unmatched <- fac_287g_doc |>
  anti_join(doc_matches, by = c("state", "county", "agency"))

doc_fuzzy_matches <- doc_unmatched |>
  inner_join(
    facility_sources_exact,
    by = "state_key",
    relationship = "many-to-many"
  ) |>
  mutate(
    dist_agency = stringdist(agency_key, facility_key, method = "jw", p = 0.1),
    dist_guess = stringdist(
      facility_guess_key,
      facility_key,
      method = "jw",
      p = 0.1
    ),
    match_dist = pmin(dist_agency, dist_guess, na.rm = TRUE)
  ) |>
  filter(match_dist <= 0.22) |>
  arrange(state, county, agency, match_dist, source_rank) |>
  mutate(
    match_type = "state_doc_fuzzy",
    match_score = 1 - match_dist,
    needs_review = TRUE
  )

# manual matches for regional jail authorities ---------------------------

manual_matches <- fac_287g |>
  inner_join(manual_points, by = c("agency", "state")) |>
  mutate(
    match_type = "manual",
    match_score = 1,
    needs_review = TRUE,
    facility_latitude = latitude,
    facility_longitude = longitude
  )

# combine all matches ----------------------------------------------------

facility_all_matches <- bind_rows(
  bind_rows(
    facility_exact_matches,
    facility_fuzzy_county,
    facility_fuzzy_state
  ) |>
    group_by(state, county, agency) |>
    arrange(source_rank, desc(match_score)) |>
    slice_head(n = 1) |>
    ungroup(),
  doc_matches,
  doc_fuzzy_matches,
  manual_matches
)

# unmatched in fac_287g but not in any match table
facility_unmatched_final <- fac_287g |>
  anti_join(facility_all_matches, by = c("state", "county", "agency")) |>
  mutate(
    match_type = "unmatched",
    source = NA_character_,
    source_rank = NA_integer_,
    needs_review = TRUE
  )

# facility point sf layer ------------------------------------------------

facility_agreements_sf <- facility_all_matches |>
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
      match_type != "exact_state_county_agency_name" |
      has_addendum |
      moa_pending
  )

# diagnostics ------------------------------------------------------------

agencies_all |> count(geom_class)

facility_all_matches |> count(match_type, source, sort = TRUE)

facility_all_matches |>
  filter(str_detect(match_type, "fuzzy")) |>
  select(
    state,
    county,
    agency,
    facility_guess,
    facility_name,
    source,
    match_type,
    match_score
  ) |>
  arrange(match_score)

facility_unmatched_final |>
  select(state, county, agency, facility_guess, support_clean) |>
  arrange(state, county, agency)

# bind all layers --------------------------------------------------------

all_agreements_sf <- bind_rows(
  state_agreements_sf |> st_transform(4326),
  county_agreements_sf |> st_transform(4326),
  university_agreements_sf |> st_transform(4326),
  municipal_agreements_sf |> st_transform(4326),
  facility_agreements_sf |> st_transform(4326)
) |>
  left_join(crime_lookup, by = c("ORI9" = "ori")) |>
  filter(!is.na(geometry)) |>
  st_make_valid() |>
  st_transform(4326)

# unresolved geometry diagnostics
unmatched_geom <- agencies_all |>
  filter(geom_class != "unknown") |>
  anti_join(
    st_drop_geometry(all_agreements_sf),
    by = intersect(
      names(agencies_all),
      names(st_drop_geometry(all_agreements_sf))
    )
  )

message(glue::glue("{nrow(unmatched_geom)} agencies with unresolved geometry"))

st_write(all_agreements_sf, "287g_agreements.shp", delete_dsn = TRUE)
