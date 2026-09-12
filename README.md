# ice-287g

Code for creating a dataset of ICE 287(g) agreements and the jurisdictions and
facilities they cover. Each agreement from ICE's [participating agencies
spreadsheet](https://www.ice.gov/identify-and-arrest/287g) is matched to the
geometry it governs — a state, county, municipality, university campus,
Pennsylvania constable ward, or detention facility — and annotated with agency
identifiers (ORI codes, FIPS codes) from four independent law-enforcement
rosters. A companion to the other
[deportationdata](https://github.com/deportationdata) repositories, built with
the same approach.

## Data files

| file | contents |
|---|---|
| **`data/all_agreements_sf.parquet`** | One row per agreement × matched feature (a DOC agreement spans many facilities). Match provenance (`match_layer`, `match_name`, `match_type`), FIPS codes, ORI, per-roster annotations, review flags, geometry. |
| **`data/agreement-level-sf.parquet`** | One row per agreement in the source spreadsheet, geometries unioned. The columns of the ICE sheet plus identifiers and geometry. |
| `data/missing-identifiers.parquet` | Exception report: agreements still missing an ORI and/or the FIPS code their layer requires. |
| `data/agreements.parquet` | The parsed ICE spreadsheet, one row per agreement, with `agreement_id` — the key every other artifact joins on. |
| `data/agreement-identifiers.parquet` | Per-agreement roster matches: chosen `ORI9` + each roster's candidate ORI/county codes and match types. |
| `data/leaic.parquet`, `data/lear.parquet`, `data/crime.parquet`, `data/hifld-law-enforcement.parquet` | The four agency rosters, normalized to a shared matching schema (LEAIC 2012, LEAR 2016, FBI Crime Data Explorer 2025, HIFLD police stations). |
| `data/facilities.parquet`, `data/jails-prisons.parquet` | Detention-facility candidate tables (ICE facilities from ice-detention-facilities; HIFLD prisons + Census of Jails). |
| `data/*-sf.parquet` (state, county, municipal, pa-constable, university, facility, non-facility) | Per-layer match results, EPSG:4326. |
| `data/crime-data-all-states.parquet` | Committed raw cache of the CDE API download. Delete it to refresh from the API (needs `CDE_API_KEY`). |
| `data/jails-prisons-geocoded-arcgis.rds` | Committed append-only ArcGIS geocode cache keyed by address. Do not regenerate from scratch — only new addresses hit the API. |

Geometry parquet is GeoParquet written with `sfarrow`; read with
`sfarrow::st_read_parquet()` in R or `geopandas.read_parquet()` in Python.
Unmatched agreements are never dropped: they ride along with empty geometries
and `needs_review = TRUE`.

## How it works

`bash code/run_all.sh` runs the numbered scripts in order; the numbers are the
dependency tiers.

- **`0-287g-scraper.R`** (run by CI, not `run_all.sh`) downloads the ICE
  spreadsheet and every agreement PDF into timestamped `sheets/` and
  `agreements/` snapshots; `0-287g-deduplicate.R` collapses unchanged files and
  `0-287g-diff.R` summarizes what changed.
- **`1-read-*.R`** ingest one source each into a normalized parquet under
  `data/`: the ICE sheet (`1-read-agreements.R`, which mints `agreement_id`),
  the four rosters, the facility tables, university campus boundaries, and the
  manual-input CSVs. Downloads are cache-gated where a committed cache exists.
- **`2-make-*-sf.R`** match agreements to geometry, one script per layer. They
  read only `1-read` outputs and never each other, so they can run in any
  order. All emit the same core schema: `agreement_id`, `match_name` (the
  matched geometry's own name), `match_type`, FIPS codes, `needs_review`,
  geometry (EPSG:4326).
- **`3-make-non-facility-sf.R`** stacks the five non-facility layers.
- **`4-match-rosters.R`** matches each agreement against the four rosters
  (exact state+county+name, with a guarded statewide fallback) and writes the
  identifier annotations. ORIs are annotations, not match inputs — geometry
  matching never depends on them.
- **`5-format-agreements-dataset.R`** joins everything by `agreement_id`,
  computes cross-roster county-disagreement flags and the LEAIC place-code
  confirmation that clears ambiguous municipal matches, and writes the two
  shipped datasets.
- **`6-make-missing-identifiers.R`** writes the exception report.

## Hand-curated inputs

- `inputs/manual-agency-ori.csv` — ORIs for agencies the rosters miss. A row
  without a county applies to every agreement of that agency statewide.
- `inputs/manual-facility-points.csv` / `inputs/manual-facility-review.csv` /
  `inputs/manual-non-facility-polygons.csv` — facility coordinates, match
  corrections (include/exclude), and layer overrides for agreements the
  automated matchers get wrong. Each row carries a `reason`/`note` recording
  the evidence.
- `inputs/manual-regional-municipalities.csv` — the member municipalities of
  regional police departments, one row per member with the source documenting
  the membership. A regional department gets one feature row per member and its
  agreement-level geometry is their union.
- `inputs/county-name-fixes.csv` — spelling fixes for the ICE sheet's county
  column.

Prefer extending these files over hand-editing outputs: the pipeline is fully
regenerable.

## Caveats

- `needs_review = TRUE` marks rows whose match rests on weaker evidence
  (fuzzy name match, ambiguous candidates, roster disagreement about the
  county, a pending MOA link). The flags that fed the decision ship alongside.
- Some agreements have no geometry yet. Texas constables are elected by justice
  precinct and those boundaries are drawn by each county's commissioners court
  with no published statewide layer, so those agreements stay unmatched rather
  than being widened to a whole county. Airport authority police, a Louisiana
  ward marshal, and departments whose county the sheet leaves blank are also
  unresolved.
- LEAIC is a 2012 crosswalk and LEAR a 2016 census; agencies created or
  renamed since may carry stale or missing identifiers there. The shipped
  `ORI9` prefers the newest source (CDE 2025, then LEAR, then LEAIC, then
  manual).
- County geometry comes from Census cartographic boundaries (2024 vintage);
  Pennsylvania constable wards/precincts come from PASDA and LRC shapefiles.

Corrections are welcome — please open an issue or pull request.
