# Published files in the site's other formats -> data/agreements.*, data/agencies.*, data/agreements-shp.zip

library(tidyverse)
library(sf)

sf_use_s2(FALSE)

agreements_sf <- st_read("data/agreements-sf.parquet", quiet = TRUE)
agreements <- agreements_sf |> st_drop_geometry() |> as_tibble()
agencies <- arrow::read_parquet("data/agencies.parquet")

arrow::write_parquet(agreements, "data/agreements.parquet")
writexl::write_xlsx(agreements, "data/agreements.xlsx")
haven::write_dta(agreements, "data/agreements.dta")
haven::write_sav(agreements, "data/agreements.sav")
writexl::write_xlsx(agencies, "data/agencies.xlsx")
haven::write_dta(agencies, "data/agencies.dta")
haven::write_sav(agencies, "data/agencies.sav")

# a shapefile layer holds one geometry type, so the zip has a point and a polygon layer
shp_dir <- file.path(tempdir(), "agreements-shp")
unlink(shp_dir, recursive = TRUE)
dir.create(shp_dir)

# shapefile field names stop at 10 characters and GDAL's truncation collides; fields.csv maps them back
shp_names <- c(
  agrmnt_id = "agreement_id",
  sheet_row = "latest_sheet_row",
  sheet = "latest_sheet",
  sheet_url = "latest_sheet_url",
  juris_lvl = "jurisdiction_level",
  juris_src = "jurisdiction_level_source",
  support = "support_type",
  ice_suppt = "ice_support_type",
  moa_pendng = "moa_pending",
  has_addend = "has_addendum",
  first_seen = "first_appeared",
  first_src = "first_appeared_source",
  last_seen = "last_appeared",
  removed_sr = "removed_by_source",
  remov_flag = "removal_flag",
  plc_geoid = "place_geoid",
  cnty_fips = "county_fips",
  geom_type = "geometry_type",
  geom_vintg = "geometry_vintage"
)
agreements_shp <- agreements_sf |> rename(any_of(shp_names))

stopifnot(
  "a column name is too long for a shapefile; add it to shp_names" =
    all(nchar(setdiff(names(agreements_shp), "geometry")) <= 10)
)

tibble(shapefile_field = names(st_drop_geometry(agreements_shp)), field = names(agreements)) |>
  write_csv(file.path(shp_dir, "fields.csv"))

layers <- c(point = "MULTIPOINT", polygon = "MULTIPOLYGON")
stopifnot(
  "every agreement is in exactly one shapefile layer" =
    sum(agreements_sf$geometry_type %in% names(layers)) == nrow(agreements_sf)
)
for (type in names(layers)) {
  agreements_shp |>
    filter(geom_type == type) |>
    st_cast(layers[[type]]) |>
    st_write(
      file.path(shp_dir, paste0("agreements-", type, "s.shp")),
      layer_options = "ENCODING=UTF-8",
      quiet = TRUE
    )
}

if (file.exists("data/agreements-shp.zip")) {
  file.remove("data/agreements-shp.zip")
}
zip(
  zipfile = "data/agreements-shp.zip",
  files = list.files(shp_dir, full.names = TRUE),
  flags = "-j -q"
)
