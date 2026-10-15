# Published files in the site's other formats -> data/agreements.*, data/agencies.*, data/agreements-shp.zip

library(tidyverse)
library(sf)

sf_use_s2(FALSE)

agreements_sf <- st_read("data/agreements-sf.parquet", quiet = TRUE)
agreements <- agreements_sf |> st_drop_geometry() |> as_tibble()
agencies <- arrow::read_parquet("data/agencies.parquet")

writexl::write_xlsx(agreements, "data/agreements.xlsx")
# Stata strings have no missing value, and haven fails on NA in a long (strL) string such as jurisdiction_facilities
haven::write_dta(
  agreements |> mutate(across(where(is.character), \(x) coalesce(x, ""))),
  "data/agreements.dta"
)
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
  juris = "jurisdiction",
  juris_fclt = "jurisdiction_facilities",
  juris_lvl = "jurisdiction_level",
  juris_src = "jurisdiction_level_source",
  support = "support_type",
  signed_dt = "signed_date",
  signed_src = "signed_source",
  moa_pendng = "moa_pending",
  has_addend = "has_addendum",
  addend_2 = "addendum_second",
  addend_sgn = "addendum_signed_date",
  addend_sg2 = "addendum_signed_date_second",
  first_seen = "first_appeared_date",
  first_src = "first_appeared_source",
  last_seen = "last_appeared_date",
  removed_dt = "removed_by_date",
  removed_sr = "removed_by_source",
  remov_flag = "removal_flag",
  plc_geoid = "place_geoid",
  cnty_fips = "county_fips",
  geom_type = "geometry_type",
  geom_vintg = "geometry_vintage"
)
agreements_shp <- agreements_sf |> rename(any_of(shp_names))

stopifnot(
  "a column name is too long for a shapefile; add it to shp_names" = all(
    nchar(setdiff(names(agreements_shp), "geometry")) <= 10
  )
)

tibble(
  shapefile_field = names(st_drop_geometry(agreements_shp)),
  field = names(agreements)
) |>
  write_csv(file.path(shp_dir, "fields.csv"))

layers <- c(Point = "MULTIPOINT", Polygon = "MULTIPOLYGON")
stopifnot(
  "every agreement with a shape is in exactly one shapefile layer" = sum(
    agreements_sf$geometry_type %in% names(layers)
  ) ==
    sum(!st_is_empty(agreements_sf))
)
for (type in names(layers)) {
  agreements_shp |>
    filter(geom_type == type) |>
    st_cast(layers[[type]]) |>
    st_write(
      file.path(shp_dir, paste0("agreements-", tolower(type), "s.shp")),
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
  flags = "-j -q",
  zip = Sys.which("zip")
)
