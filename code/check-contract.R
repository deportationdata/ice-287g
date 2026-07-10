#!/usr/bin/env Rscript
# Assert the downstream contract on data/agreement-level-sf.parquet, the
# artifact slicer-shiny-app consumes from main (see datasets/287g.yml there:
# map_geometry = geometry, map_label = agency, map_geos joins state by full
# name, filter/group cols below). Prints a markdown checklist and exits
# nonzero on any violation so CI can gate on it.
#
# Usage: check-contract.R [path]  (default data/agreement-level-sf.parquet)

suppressMessages({
  library(dplyr)
  library(sf)
})

args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args) >= 1) args[1] else "data/agreement-level-sf.parquet"

# canonical full state names, embedded so the script runs from a sparse
# checkout without data/state_xwalk.parquet; territory spellings must match
# data/state_xwalk.parquet (built in 1-process-state-xwalk.R)
territory_names <- c(
  "District Of Columbia",
  "Puerto Rico",
  "Guam",
  "American Samoa",
  "United States Virgin Islands",
  "Commonwealth of the Northern Mariana Islands"
)
valid_states <- c(datasets::state.name, territory_names)

required_cols <- c(
  "agency",
  "state",
  "county",
  "support_type",
  "agency_level",
  "geom_class",
  "has_addendum",
  "moa_pending",
  "state_fips",
  "match_layer",
  "geometry"
)

grain_cols <- c(
  "agency",
  "state",
  "county",
  "support_type",
  "agency_level",
  "geom_class",
  "has_addendum",
  "moa_pending",
  "state_fips"
)

failures <- character(0)
check <- function(ok, pass_msg, fail_msg) {
  if (isTRUE(ok)) {
    cat("- [x] ", pass_msg, "\n", sep = "")
  } else {
    cat("- [ ] **FAIL** ", fail_msg, "\n", sep = "")
    failures <<- c(failures, fail_msg)
  }
}

cat(sprintf("### Contract check: `%s`\n\n", basename(path)))

if (!file.exists(path) || file.size(path) == 0) {
  cat("- [ ] **FAIL** file missing or empty\n")
  quit(status = 1)
}

x <- sfarrow::st_read_parquet(path)
cat(sprintf("- %d rows, %d columns\n", nrow(x), ncol(x)))

missing_cols <- setdiff(required_cols, names(x))
check(
  length(missing_cols) == 0,
  "all required columns present",
  paste("missing columns:", paste(missing_cols, collapse = ", "))
)

bad_states <- setdiff(unique(x$state), valid_states)
check(
  length(bad_states) == 0,
  "state values are canonical full names",
  paste("non-canonical state values:", paste(bad_states, collapse = ", "))
)

present_grain <- intersect(grain_cols, names(x))
n_dup <- x |>
  st_drop_geometry() |>
  count(across(all_of(present_grain))) |>
  filter(n > 1) |>
  nrow()
check(
  n_dup == 0,
  "one row per agreement (grain keys unique)",
  sprintf("%d duplicated agreement key(s)", n_dup)
)

check(
  identical(st_crs(x)$epsg, 4326L),
  "CRS is EPSG:4326",
  sprintf("CRS is %s, expected EPSG:4326", st_crs(x)$input)
)

n_gc <- sum(st_geometry_type(x) == "GEOMETRYCOLLECTION" & !st_is_empty(x))
check(
  n_gc == 0,
  "no non-empty GEOMETRYCOLLECTION geometries",
  sprintf("%d non-empty GEOMETRYCOLLECTION row(s)", n_gc)
)

cat("\n")
if (length(failures) > 0) {
  cat(sprintf("**%d contract violation(s).**\n", length(failures)))
  quit(status = 1)
}
cat("**All contract checks passed.**\n")
