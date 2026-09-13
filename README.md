# ice-287g

Code for creating a dataset of ICE 287(g) agreements and the jurisdictions and
facilities they cover. Every version of ICE's [participating agencies
spreadsheet](https://www.ice.gov/identify-and-arrest/287g) ever archived is
read into one record per agreement, each agreement is matched to the geometry
it governs — a state, county, municipality, judicial district, regional
department's member municipalities, university campus, Pennsylvania constable
ward, airport, or detention facility — and annotated with agency identifiers
(ORI codes, FIPS codes) from four independent law-enforcement rosters. The
program's history before ICE published spreadsheets is reconstructed from
government, FOIA and academic sources into one partnership-level table. A
companion to the other [deportationdata](https://github.com/deportationdata)
repositories, built with the same approach.

## Two grains

- A **partnership** is the relationship between ICE and one agency in one
  state (`partnership_id`, e.g. `NC-alamance-county-sheriff-office`, derived
  from the agency's canonical name so a spelling change never forks it).
- An **agreement** is one signed instrument under a partnership: a support
  model signed on a date (`agreement_id`, e.g.
  `NC-alamance-county-sheriff-office#JEM#2007-01-10`). A partnership can hold
  several — a re-signing supersedes its predecessor, and a sheriff may run jail,
  task-force and warrant-service agreements at once. `status` is `active`
  (on the current sheet), `superseded` (a later agreement of the same
  partnership took over) or `removed`.

Both ids are content-derived and stable across runs; an id changes only when
the canonical name it is built from changes, which is exactly when the diff
deserves a manual check.

## Data files

`data/` holds only the published products: the agreement-level geometry file, the
partnership history and the committed QA reports in `data/qa/`. Everything the
pipeline builds on the way — rosters, per-layer and per-feature matches, the identity layer,
source claims and the two API caches — is in `data/intermediate/`, committed so
a pull request shows what moved.

| file | contents |
|---|---|
| **`data/agreement-level-sf.parquet`** | One row per agreement, geometries unioned: the ICE sheet's columns (its TYPE as printed is `ice_type`), the agreement's `jurisdiction_level` and `jurisdiction_level_source`, identifiers, `geom_class`, `geometry_vintage`, `match_quality`, `review_reason`, geometry. The file the slicer consumes. |
| **`data/partnerships.parquet`** | One row per partnership across every era (2002 → today): its jurisdiction level (State, County, Municipal, Regional, Campus, Port, Constable District or Judicial District), ICE's listing and removal windows, first and latest signing dates with the source of each, models, the window the evidence speaks to, MOA archive status and which sources attest it. |
| `data/intermediate/agreements.parquet` | The current sheet cleaned, one row per agreement, with lineage (`partnership_id`, `succeeded_by`), first/last appearance and removal window. |
| `data/intermediate/identity-agreements.parquet`, `data/intermediate/sheet-publications.parquet`, `data/intermediate/sheet-publication-files.parquet`, `data/intermediate/sheet-row-agreements.parquet`, `data/intermediate/identity-agency-spellings.parquet` | The identity layer: every distinct sheet ever published, every row of every sheet resolved to an agreement, and every spelling ICE printed for each partnership. |
| `data/intermediate/historical-source-claims.csv`, `data/intermediate/historical-source-claims-unresolved.csv` | Every claim a non-sheet source (ICE's undated lists, ICE's MOA archive index, DHS OIG's Oct 2009 appendix, ICE press releases) makes about a partnership — listed, pending, signed, model, MOA file, rescinded — with the rule that resolved it; what no rule resolves is listed, never dropped. |
| `data/intermediate/partnership-disagreements.csv` | Where a source and ICE differ: a signing date, a model, a state, or presence on the ICE publication nearest to the source's date. |
| `data/intermediate/match-agency-identifiers.parquet` | Per-agreement roster matches: chosen `ORI9` + each roster's candidate ORI/county codes, match types and ambiguity. |
| `data/intermediate/match-missing-identifiers.parquet` | Exception report: agreements still missing an ORI and/or the FIPS code their layer requires. |
| `data/qa/` | Committed QA: `qa-summary.csv` (every invariant and count, `pass`/`fail`/`info`), `qa-match-types.csv`, `qa-review-reasons.csv`, `qa-acquisition.csv`, `identity-candidates.csv` (spellings the rules would not merge), `historical-summary.md` (per-source counts of partnerships attested, listed, dated and unresolved). A `fail` row fails CI. |
| `data/intermediate/agency-roster-leaic-2012.parquet`, `data/intermediate/agency-roster-lear-2016.parquet`, `data/intermediate/agency-roster-cde-2025.parquet`, `data/intermediate/agency-roster-hifld.parquet` | The four agency rosters, normalized to a shared matching schema (LEAIC 2012, LEAR 2016, FBI Crime Data Explorer 2025, HIFLD police stations). |
| `data/intermediate/facility-list-ice-detention.parquet`, `data/intermediate/facility-list-jails-prisons.parquet` | Detention-facility candidate tables (ICE facilities from ice-detention-facilities; HIFLD prisons + Census of Jails). |
| `data/intermediate/match-*.parquet` (state, county, municipal, pa-constable, university, facility, non-facility) | Per-layer match results, EPSG:4326. |
| `data/intermediate/match-all-features.parquet` | One row per agreement × matched feature (a DOC agreement spans its state's prisons; a regional department its member municipalities). Match provenance (`match_layer`, `match_name`, `match_type`, `match_quality`), FIPS codes, ORI, per-roster annotations, every review flag and the composed `review_reason`, geometry. The layer behind `agreement-level-sf`; the PR diff workflow compares it against `main`. |
| `data/intermediate/cache-cde-api.parquet` | Committed raw cache of the CDE API download. Delete it to refresh from the API (needs `CDE_API_KEY`). |
| `data/intermediate/cache-arcgis-geocodes.rds` | Committed append-only ArcGIS geocode cache keyed by address. Do not regenerate from scratch — only new addresses hit the API. |

Geometry files are Parquet with the native GEOMETRY type, written by GDAL through
`sf::st_write(driver = "Parquet", layer_options = "USE_PARQUET_GEO_TYPES=ONLY")`; the
CRS (EPSG:4326) travels in the type, not in GeoParquet `geo` metadata. The pipeline
reads them with `sf::st_read()` (GDAL ≥ 3.12); `arrow::read_parquet()` plus
`sf::st_as_sfc(<WKB column>, crs = 4326)` or DuckDB ≥ 1.4 also work.
`.Rprofile` (and `code/run_ci.sh`) set `ARROW_DEFAULT_MEMORY_POOL=system`: R arrow
and sf's GDAL each carry their own libarrow, and one system allocator keeps the
two copies from freeing each other's memory, which corrupts a GDAL Parquet write.
Unmatched agreements are never dropped: they ride along with empty geometries
and `review_reason = "no geometry matched"`.

## How it works

`bash code/run_all.sh` runs the numbered scripts in order; the numbers are
dependency tiers. `bash code/run_all.sh 3-match-state.R` starts partway.

- **`0-acquire-*.R`** (run by CI, not `run_all.sh`). The scraper snapshots
  the ICE spreadsheet into a timestamped `sheets/` folder when its bytes change.
  The MOA pass revalidates every agreement PDF the sheet links against the
  origin's `Last-Modified` (a shard each hour, everything when the sheet
  changes) and fetches only what is new or changed into `agreements/`; a link
  the origin answers 404 for is recovered from the spelling the origin actually
  serves or from the Wayback Machine, and recorded either way. Each snapshot
  folder's `manifest.csv` records url, hash, validators and, when dedupe
  removed a byte-identical copy, where the bytes were kept. The Wayback
  backfill, dedupe, diff and manifest scripts round out the stage. A weekly
  workflow (`moa-discovery-287g.yaml`) runs `0-acquire-archive-index.R`, which
  snapshots ICE's `/287g-archive` index into `inputs/historical/` when it
  changes, and `0-acquire-moa-probe.R`, which asks ice.gov for the filenames
  ICE's naming pattern predicts for agreements its sheet has shown as link
  pending for three weeks or more; PDFs whose text confirms the agency or signing
  date arrive as a pull request, and `2-make-agreements.R` links a pending
  agreement to the held PDF its name predicts until ICE's sheet links it.
  `0-acquire-moa-wayback.R`, run by hand, recovers MOA PDFs the Wayback Machine
  archived that no snapshot folder holds. CI commits
  the raw acquisition before it builds `data/`, so a failed build never loses
  one.
- **`1-read-sheets.R`** reads every archived sheet — live scrapes, ICE's own
  workbooks recovered from the Wayback Machine, and the roster table of every
  archived ICE page from April 2008 on, parsed straight from the raw capture
  in each `sheets/sheets_wayback_*/` folder — into publications (distinct table contents,
  dated by ICE's own filename date, or by archive capture before ICE's workbooks carried one) and rows. Nothing derived is stored beside the
  captures: a page's table is read when the pipeline runs. The fix tables in
  `inputs/` are applied to the rows here, and two rules catch slips ICE makes
  without a row: a state ICE printed for a short stretch of lists, never beside
  the longer run's state, moves to that state; a signing year printed a year
  early (a listing first seen on an ICE-dated list more than 300 days after its
  printed date, with the month and day falling in the 60 days before that list)
  takes the year of its first listing, unless the same date was already printed
  for a near-identical spelling.
  **`2-make-identities.R`** resolves rows into agreements and partnerships:
  exact keys, then a typo tier and a modifier tier that only merge when the
  windows and spellings make one agreement the only reading, a rename tier for
  two spellings ICE printed on adjacent lists linking the same MOA file, then the
  committed alias table. A signing date ICE corrects between consecutive sheets
  (a listing with no MOA link, "link pending" or a blank cell, re-dated, whether or not its MOA has posted, or one
  MOA re-dated within 30 days or by exactly a year) stays one agreement, as does a
  pending listing's date ICE printed for a stretch and then reverted; each is logged in
  `data/qa/signing-date-corrections.csv`. What it declines to merge is proposed in
  `data/qa/identity-candidates.csv`. **`2-make-agreements.R`** cleans the
  current sheet into `agreements.parquet`: county and TYPE fixes, the county an
  agency's own name states, an MOA ICE has posted but not yet linked (found
  among the held PDFs by name), each agreement's jurisdiction level and the
  geometry class that level calls for.
- **`1-read-*.R`** ingest the rosters, facility tables, campus boundaries and
  manual-input CSVs. **`1-read-historical-*.R`** parse ICE's other records —
  the undated agency lists of Sep 2007 and Mar 2008, the MOA archive index —
  and the DHS OIG appendix of Oct 2009, each registered in
  `inputs/historical/source-registry.csv` with its grade, rank and as-of date.
  **`2-make-partnerships.R`** reduces them, and the press-release claims in
  `inputs/historical/press-claims.csv`, to typed claims resolved to
  partnerships by rule, then arbitrates one record per partnership with the
  winning source beside each value and records every disagreement with ICE.
  Only a press-release claim may add a partnership; every other source
  attests to existing ones.
- **`3-match-*.R`** match agreements to geometry, one script per layer, in
  any order. Each emits `agreement_id`, `match_name` (the matched geometry's
  own name), `match_type`, FIPS codes, `geometry_vintage`, its named review
  flags and geometry (EPSG:4326). County geometry is the 2024 Census
  cartographic vintage except Connecticut, whose legacy counties come from
  2021 because the sources name them. A judicial-district office is the union
  of its counties (`inputs/manual-judicial-district-counties.csv`), a regional
  department the union of its member municipalities, a port authority a point
  at its airport, and a constable district stays unplaced: no statewide layer
  of justice precincts or justice court districts exists.
- **`4-match-non-facility.R`** stacks the non-facility layers and asserts
  every placed feature carries a geoid.
- **`5-match-agency-identifiers.R`** matches each agreement against the four rosters
  (exact state+county+name, then a unique statewide full name, then a guarded
  key, then a roster name that begins with the agency's whole name where
  nothing else matched) and writes the identifier annotations, with `ori_ambiguous` where a
  roster offered more than one ORI. ORIs are annotations, not match inputs.
- **`6-make-agreement-level-sf.R`** joins everything by `agreement_id`,
  judges roster-county disagreements per agreement, composes `review_reason`
  from one vocabulary of flags, derives `needs_review` and `match_quality`,
  and writes the two shipped datasets.
- **`7-match-missing-identifiers.R`** writes the exception report and
  **`7-make-qa-report.R`** the QA tables; `QA_STRICT=1` (set by CI) makes any
  failing check fatal.

CI runs two jobs: `acquire` commits the raw snapshot, then `build` runs the
pipeline and commits `data/`; a pull request shows the agreements diff and the
QA diff.

## Hand-curated inputs

- `inputs/agency-aliases.csv` — identity verdicts the rules cannot know: a
  spelling that is the `same` agency as another, or two near-identical names
  that are `distinct`. Rows only add merges or silence a candidate; the
  pipeline never waits on them.
- `inputs/manual-agency-ori.csv` — ORIs for agencies the rosters miss. A row
  without a county applies to every agreement of that agency statewide.
- `inputs/manual-facility-points.csv` / `inputs/manual-facility-review.csv` /
  `inputs/manual-non-facility-polygons.csv` — facility coordinates, match
  corrections (include/exclude), and layer overrides for agreements the
  automated matchers get wrong. Each row carries a `reason`/`note` recording
  the evidence.
- `inputs/manual-jurisdiction-levels.csv` — an agreement's jurisdiction level
  where neither ICE's TYPE nor the agency's name settles it, with the evidence
  in each row. Each agreement's `jurisdiction_level` names what the agency
  covers, one of eight: `State`, `County` and `Municipal` (the Census Bureau's
  general-purpose governments); `Campus` for an educational institution's own
  police, a university, college or school district; `Constable District` for a
  constable whose office is a division of the county (a Texas justice
  precinct, a Mississippi justice court district), while other constables are
  `County` officers whatever TYPE ICE printed (Tennessee's are elected by
  district but hold county-wide jurisdiction) except Pennsylvania's, who serve
  a borough, township or ward and stay `Municipal`; `Regional` for a body
  several jurisdictions formed (a regional jail authority, a regional police
  department, a multi-agency task force); `Judicial District` for a state
  office serving a multi-county judicial district or circuit, whose counties
  are listed in `inputs/manual-judicial-district-counties.csv`; and `Port` for
  an airport or seaport authority's police. The level comes from this file
  first, then from name rules (constable, campus, judicial, port and regional
  words, each tested over every partnership with no false positive, and a
  sheriff or jail naming its own county), then from ICE's TYPE, and for the
  fact-sheet era, which printed no TYPE, from the name alone;
  `jurisdiction_level_source` says which, and `ice_type` carries ICE's TYPE as
  printed. A partnership takes the level of its latest agreement.
- `inputs/manual-regional-municipalities.csv` — the member municipalities of
  regional police departments, one row per member with the source documenting
  the membership. A regional department gets one feature row per member and its
  agreement-level geometry is their union.
- `inputs/county-name-fixes.csv`, `inputs/signed-date-fixes.csv`,
  `inputs/agency-name-fixes.csv`, `inputs/state-fixes.csv`,
  `inputs/agency-type-fixes.csv` — county, date, agency-name, state and TYPE
  fixes for the ICE sheet, keyed on the erroneous value so a row does nothing once ICE
  corrects it. A county fix with an `agency` applies to that agency only (a real
  county ICE assigned to the wrong place); without one it fixes a misspelled
  county wherever it is printed. A blank `county_fixed` removes a county that
  does not exist. `inputs/renewal-date-fixes.csv` re-dates an agreement ICE
  renewed without changing its signing date, from the first list that links the
  new MOA, so the old agreement ends where the renewal begins.
  `inputs/moa-link-fixes.csv` supplies an agency's own MOA where the sheet links
  another agency's (`moa_linked`), used only while it links that file; an MOA
  ICE has posted but not linked needs no row, since the build finds it among the
  held PDFs by name.
  An agency-name fix is an error ICE printed on specific agreements (a county's
  jail agreement listed under its sheriff) and carries the MOA evidence; an
  alias in `inputs/agency-aliases.csv` is two spellings of one agency.
- `inputs/historical/` — ICE's records of the program before the live
  scraper and the one OIG appendix the pipeline reads: `SOURCES.csv` is the
  citation and provenance record for every file, `source-registry.csv` the
  machine-readable register, `press-claims.csv` the agreements ICE
  announced that reached no roster. `inputs/other_sources/` holds the
  secondary reports, panels, mirrors and the retired hand-built jurisdiction
  crosswalk the pipeline does not read; none adds a partnership or date.

Prefer extending these files over hand-editing outputs: the pipeline is fully
regenerable.

## Caveats

- `review_reason` names every reason a row deserves a look — no geometry, a
  fuzzy or fallback match, several candidates, a roster placing the agency in
  another county, disagreeing or ambiguous ORIs, a row ICE printed twice —
  and `needs_review` is simply whether it is set. A pending MOA link or an
  addendum is reported (`moa_pending`, `has_addendum`) but is not a reason for
  review. `match_quality` summarizes the same evidence as
  exact / exact_flagged / fuzzy / weak / manual / unmatched.
- Some agreements have no geometry yet. Constable districts (Texas justice
  precincts, Mississippi justice court districts) are drawn by each county with
  no published statewide layer, so those agreements stay unmatched rather than
  being widened to a whole county. A Louisiana ward marshal and departments
  whose county the sheet leaves blank are also unresolved.
- The fact-sheet era printed no agency type; for those rows the jurisdiction
  level is derived from the name (`jurisdiction_level_source = "agency_name"`).
- LEAIC is a 2012 crosswalk and LEAR a 2016 census; agencies created or
  renamed since may carry stale or missing identifiers there. The shipped
  `ORI9` prefers the strongest match tier, then the newest source (CDE 2025,
  then LEAR, then LEAIC, then manual); an exact match that could not tell two
  ORIs apart yields to another roster's unique full-name match.
- Every list since March 2025 is dated by ICE's own filename date, lists sharing
  a date in ICE's am/mid/pm order; earlier lists (archived ICE pages, which carry
  no date) by the Eastern date of their earliest archive capture.
  `first_appeared_source`, `removed_by_source` and `ice_listed_from_source` say
  which. Capture times only order lists that share a date and catch a filename
  date the file was online before (`date_flag` in
  `data/intermediate/sheet-publications.parquet`); a filename dated a year early
  (01062025 on a January 2026 list) takes the year its newest signing date needs.
- The DHS OIG appendix is a PDF text layer; its model marks are placed by
  column position and its every row is checked against the 67 it states.
  ICE's own dates disagree with themselves in places (the archive index and
  the sheet differ on 14 signing dates); `data/intermediate/partnership-disagreements.csv`
  records each and `first_signed` takes the earliest.

Corrections are welcome — please open an issue or pull request.
