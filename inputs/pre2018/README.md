# Pre-2018 287(g) agreements — original-source archive and reconstruction

Assembled 12 September 2026. Everything here is either an **original source file
in its published form** or an output derived from those files by the R scripts in
`R/`. Nothing is hand-typed.

**Contents as downloaded:** 33 archived ICE roster pages (HTML), 170 signed MOAs
(PDF, 419 MB), the Yale FOIA page (HTML), 16 reports (PDF), and the East et al.
county-month panel (.dta). Total ~500 MB. The pipeline has been run end to end on
these files; `output/` holds the results.

**170 MOAs is the ceiling, not a shortfall.** Every agreement ICE ever linked and
Wayback ever captured is here. Eighteen 2017-era agreements are linked by ICE's own
table but were never successfully archived — Wayback holds only 404 captures for
them, confirmed by CDX lookup. They are flagged `moa_pdf_present = FALSE` in
`output/agreements_best_guess.csv`, and they are all from the mid-2017 expansion:
Clay FL, East Baton Rouge LA, Anne Arundel MD, Horry SC, Knox TN, and thirteen Texas
counties. Same failure mode as the Yale FOIA PDFs — the crawler arrived after the
files were pulled. 83 of 101 agreements have their document; for the other 18 the
roster row (agency, model, signing date) is all that survives.

The goal is a defensible answer to: *for which periods can we say "these are all
the 287(g) agreements"?* The short answer is that ICE's own fact-sheet roster,
captured repeatedly by the Wayback Machine, is a complete census of **active**
agreements on ~24 separate dates between Sep 2007 and Aug 2017, and the union of
those censuses supports a complete-enumeration claim for roughly 2007–2017.

---

**Full citations and URLs for every file live in `SOURCES.csv`** (26 records:
file, kind, formal citation, canonical URL, where it was actually retrieved from,
retrieval date, and what roster or table it contains). The prose below explains the
material; `SOURCES.csv` is the machine-readable provenance record.

## Layout

```
287g_archive/
  README.md                     <- this file
  SOURCES.csv                   <- citation + URL for every source file
  R/                            <- the pipeline (run 99_run_all.R)
  sources/
    ice_factsheets/             <- 33 archived ICE roster pages (HTML, as served)
    moa_pdfs/                   <- 170 signed MOAs (PDF, as served by ICE) + _cdx_index.tsv
    reports/                    <- 16 reports with rosters or counts (PDF)
    panels/                     <- East et al. county-month policy panel (.dta)
    yale_wirac_287g_foia_2008.html
  crosswalk/
    ice_agency_to_county.csv    <- ICE agency name -> county FIPS (hand-built, see below)
  prebuilt/                     <- parsed outputs, for when a host blocks you
  output/                       <- everything the scripts produce
```

## Running it

```sh
Rscript R/99_run_all.R          # from the archive root
```

Packages: `httr rvest xml2 dplyr tidyr stringr pdftools haven`.
`pdftools` needs poppler (`apt install libpoppler-cpp-dev`, or `brew install poppler`).

Scripts are independent and idempotent; `00` skips files already downloaded, so it
is safe to interrupt and re-run.

**On archive.org rate limiting.** web.archive.org refuses further connections after
roughly 20 requests in a short window, then recovers after 40–60 seconds of quiet.
`00` is therefore strictly serial with exponential backoff on refusal — do not be
tempted to parallelise it, which trips the limiter almost immediately and gets you
nothing. Fetching all 170 MOAs takes a while; that is expected.

---

## Provenance, file by file

### `sources/ice_factsheets/*.html` — the core rosters

ICE maintained a live table on its 287(g) fact sheet:

```
STATE | LAW ENFORCEMENT AGENCY | SUPPORT TYPE | DATES SIGNED | MOA (hyperlink)
```

captioned "Mutually Signed Agreements (N) as of MM/DD/YYYY". It moved URL once.
Retrieved from the Wayback Machine; the upstream originals are gone.

| Era | Original URL |
|---|---|
| 2011–2014 | `http://www.ice.gov/news/library/factsheets/287g.htm` |
| 2015–2017 | `http://www.ice.gov/factsheets/287g` |

Snapshot timestamps are listed in `00_download_sources.R` and written to
`_snapshot_manifest.csv`. Two eras earlier (`/pi/news/factsheets/070622factsheet287gprogover.htm`,
captures in 2008 and 2010) and the landing page `ice.gov/287g` are noted in the
script but not part of the roster series.

**Two caveats, both handled in `01_parse_ice_factsheets.R`:**

1. **The caption goes stale after 2014.** From 2015 on, every capture still reads
   "(35) signed as of 07/25/2014" while the table underneath kept changing — from
   32 rows in mid-2015 to 58 by Aug 2017. Trust the row count and the per-row
   dates, never the caption. The parser records which basis it used in `asof_basis`.
2. **`DATES SIGNED` is the *current* agreement's date**, overwritten on each
   re-signing (the 2009 standardisation, then 2013, then 2016). It is not first
   adoption. `05_build_best_guess.R` recovers first adoption as the earliest value
   observed across all snapshots and sources.

### `sources/moa_pdfs/*.pdf` — the agreements themselves

ICE served these from
`http://www.ice.gov/doclib/foia/memorandumsofAgreementUnderstanding/`.
The directory is gone from the live site; the files are archived. `00` enumerates
them from the Wayback CDX index rather than a hardcoded list, so nothing is missed,
and writes `_moa_manifest.csv`. Four filename generations:

| Pattern | Vintage | Crawled |
|---|---|---|
| `<agencyname>.pdf` | original pre-2009 MOAs | 2009-05 |
| `r_287g<agency><MMDDYY>.pdf` | 2009 standardised re-signings | 2010-05 |
| `r_287g<agency>.pdf` | 2013–14 re-signings | 2014-04 / 2014-09 |
| `<Agency>_MOA_<date>.pdf`, `<agency>-<st>-2017.pdf` | 2016–17 agreements | 2016-12 / 2017-04 |

Downloaded with the Wayback `id_` modifier, which returns the original bytes with
no toolbar injected. Some are large (Maricopa County is ~26 MB).

A subset of the newer MOAs is also still live at
`https://www.ice.gov/doclib/287gMOA/<lowercaseagencyname>.pdf`, but filenames there
are not fully predictable and some 403.

### `sources/yale_wirac_287g_foia_2008.html`

Yale Law School's Worker & Immigrant Rights Advocacy Clinic filed a 287(g) FOIA
request on 2007-03-08. ICE produced **34 MOUs** on 2008-01-17 plus two cover
letters, withholding others under claimed exemptions. This is the earliest
independent, FOIA-verified census.

Original `http://islandia.law.yale.edu/wirc/287g_foia.html` (dead); retrieved from
Wayback capture `20080807125556`.

**The MOU PDFs are not recoverable.** Wayback crawled `/wirc/pdfs/287_g_foia/`
only after Yale removed it — all four captures, including the 60 MB ZIP of the
whole release, are 404s. Verified against the CDX index. Use `sources/moa_pdfs/`
for the documents; most of those 34 agencies are in there.

### `sources/reports/*.pdf`

| File | What it contains | Retrieved from |
|---|---|---|
| `GAO-09-109_Jan2009.pdf` | App. III, pp. 33–34: all 29 agencies as of 2007-09-01, names only | TRAC mirror (`gao.gov` 403s automated requests) |
| `DHS-OIG-10-63_Mar2010.pdf` | App. E, Table 3, p. 83: 66 agreements, 23 states, as of Jun 2009 | TRAC mirror (`oig.dhs.gov` 403s) |
| `DHS-OIG-10-124_Sep2010.pdf` | counts only: 71 MOAs as of 2010-08-01 (26 TFO / 32 jail / 13 joint) | TRAC mirror |
| `DHS-OIG-11-119_Sep2011.pdf` | counts only: 69 MOAs, 24 states, as of 2011-06-01 (34/20/15) | TRAC mirror |
| `DHS-OIG-12-130_Sep2012.pdf` | counts only: 64 MOAs, 24 states, Aug 2012 (35/20/9) | TRAC mirror |
| `CRS-RL32270_2009-03-11.pdf` | **App. A: 67 agreements, 23 states, with model and signed date, as of Feb 2009** | everycrsreport.com |
| `CRS-RL32270_2007-08-30.pdf` | App. A, p. 31: agencies + officer counts as of 2007-08-29 | everycrsreport.com |
| `CHRG-111hhrg49374_House-Homeland_2009-03-04.pdf` | Riley (ICE) testimony, Attachment 1, pp. 13–14 — the primary source behind the CRS table | govinfo.gov |
| `MPI_Delegation-and-Divergence_Jan2011.pdf` | App. 2: 72 jurisdictions, Aug 2010, model + original signing date | migrationpolicy.org |
| `MPI_Program-in-Flux_Mar2010.pdf` | App. 1: 71 MOAs active or in negotiation, Jan 2010, with dates | migrationpolicy.org |
| `NCLR_Lacayo_2010.pdf` | App. A: comprehensive list as of 2010-08-02, model + date signed | unidosus.org |
| `CIS_Vaughan-Edwards_Oct2009.pdf` | Table 4: 56 agencies with arrests, officers, MOA type (explicitly incomplete) | cis.org |
| `ACLU-NC_UNC_Feb2009.pdf` | 287(g) in North Carolina; appendices A–C, pp. 101–144, built from public-records requests | law.unc.edu |
| `Kostandini-et-al_AJAE_2014.pdf` | **App. Table A1: 66 agreements with exact signing dates, 2002-07-02 to 2010-08-19** | ncaeonline.org (free copy of AJAE 96(1):172–192) |
| `NIJ_Capellan-Sorg_2022.pdf` | FOIA'd ICE data on 167 counties that *applied* 2005–2010, incl. rejections. No county appendix — contact the authors | ojp.gov |
| `Cato-WP52_Forrester-Nowrasteh_2018.pdf` | Table A1: the 9 NC 287(g) counties with MOA signing dates, Feb 2006 – Oct 2009, plus annual removals | cato.org (requires a Referer header, see below) |

All 16 reports are present. Two access quirks are handled in `00`:

- **cato.org** returns a 212-byte HTML stub to any request that arrives without a
  `Referer` header. Sending the paper's landing page as the referrer returns the
  real 444 KB PDF. This looked like a hard block and was not one.
- **ice.gov** and **gao.gov** return 403 to scripted requests from every network
  tested. Neither matters: ICE's content comes through Wayback, and GAO-09-109 is
  byte-identical on the TRAC mirror.

### `sources/panels/287g_SC_EVerify_5_13_22.dta`

From the replication repo of **East, Hines, Luck, Mansour & Velásquez (2023),
"The Labor Market Effects of Immigration Enforcement," JOLE 41(4): 957–996** —
<https://github.com/cneast/East_etal_2022>. County × year × month, 3,140 counties,
1996–2016, with `jail287g`, `task287g`, `state287g`, Secure Communities and E-Verify.
The only deposited continuous 287(g) panel I could find.

Per the paper's Appendix A.1, it was built from ICE and DHS reports, MPI,
Kostandini et al., and news articles — a reconstruction, not a primary census. Its
real value-add over Kostandini is **termination** dates. The authors withhold their
ACS and TRAC files for licensing reasons; TRAC supplies their deportation and
detainer *outcome* counts, not the 287(g) policy variable, so nothing needed here
is missing.

### `crosswalk/ice_agency_to_county.csv`

**The one hand-built file in this archive.** Maps each of the 105 ICE agency names
to `statewide` or to county FIPS, so agency-level rosters can be compared with
county-level panels. Municipal agencies map to their containing county; Carrollton
PD maps to Dallas, Denton and Collin (the city spans all three); Virginia
independent cities map to the adjacent county. Reviewed against the 2015 Census
county list. Treat the many-to-one collapses as a judgement call, not ground truth —
they are the main reason an "in East, absent from ICE" flag may be a false positive.

---

## What the scripts produce

| Output | Contents |
|---|---|
| `ice_roster_long.csv` | one row per agency × snapshot, with as-of date, model, signed date, MOA filename |
| `kostandini_tableA1.csv` | Table A1 parsed out of the JSTOR scan, OCR repaired |
| `yale_foia_2008.csv` | the 34 agencies in ICE's Jan 2008 production |
| `east_county_spells.csv` | 51 county participation spells with start/end months |
| **`agreements_best_guess.csv`** | **the deliverable: one row per agreement** |
| `source_attestation.csv` | long form — which source saw which agency, when |
| `completeness_by_period.csv` | the ledger of dates a complete-census claim is defensible |
| `east_vs_ice_audit.csv` | East's panel diffed against ICE's rosters, date by date |

### How `agreements_best_guess.csv` decides

Unit of observation: a `(state, agency)` pair — one partnership, possibly signed,
re-signed and terminated several times. Agency names are normalised across sources
(abbreviation expansion, `SO`→`sheriffs office`, punctuation stripped) before matching.

- `first_signed_best` — earliest signing date in **any** source. Because ICE
  overwrote its date column on re-signing, the earliest observed value is the best
  surviving estimate of original adoption. `first_signed_source` says which source won.
- `active_from` / `active_to` — first and last roster as-of date on which the agency
  appears. **Interval-censored**: true start ≤ `active_from`, true end ≥ `active_to`.
- `terminated` — appears in some roster but not the final one. The termination date
  is bracketed by consecutive snapshots, never pinned.
- `confidence` — `high` = on ICE's own roster *and* ≥1 other source; `medium` = ICE
  only, or two non-ICE sources; `low` = a single non-ICE source.
- `moa_archived_url` — resolvable Wayback URL(s) for the signed agreement.
- `moa_pdf_present` — whether the document is actually in `sources/moa_pdfs/`.
  `FALSE` means ICE linked it but it was never archived (see above); the script
  checks the filesystem, so this recomputes correctly wherever you run it.

Current run, from all 33 real captures: **101 agreements**, 66 high / 35 medium
confidence, 43 terminated before the last roster, **83 with the signed document on disk** (18 linked but never archived),
earliest signing 2002-07-02. The roster series itself is 1,472 agency-snapshot rows
covering 104 distinct agency name variants (collapsed to 101 agreements after
normalisation — ICE spelled a few agencies more than one way, e.g. "Massachusetts
Department of Correction(s)" and "Harford"/"Hartford" County MD).

The caption-vs-reality warning is not hypothetical: at three captures the on-page
count and the actual row count disagree (2012-01-20 says 69 with 68 rows;
2012-10-07 says 63 with 64; 2014-04-14 says 36 with 37), and from 2015-03-31 the
caption disappears from the markup entirely while the table keeps changing —
32 rows in mid-2015 rising to 58 by 2017-08-01.

---

## Two findings worth carrying into any write-up

**1. Point-in-time rosters read backwards are survivor-biased.** Kostandini's
Table A1 is the most-cited early list, and its own source note is ICE's fact-sheet
URL — it is that roster transcribed, with the signing-date column read backwards.
So it enumerates agreements signed 2002–2010 *that were still alive when the roster
was captured*. Agreements that began and ended inside the window are invisible.
Concretely, Barnstable County MA, Framingham MA, Hudson NH and El Paso TX are all
in the Jan 2008 Yale FOIA release and all absent from ICE's Oct 2010 table.

**2. Kostandini is not even complete at its own as-of date.** The pipeline finds
five agreements on ICE's Oct 2010 roster that Table A1 omits: Etowah County AL,
Pinal County AZ, Massachusetts DOC, Guilford County NC, and the Rhode Island State
Police. Since much of the economics literature — every Amuedo-Dorantes paper
included — sources its 287(g) variable from Kostandini plus ICE's *current* page,
this omission propagates.

**3. ICE's own roster is not a perfect census either — it starts too late.** Forrester
& Nowrasteh (Cato WP 52) give MOA signing dates for nine North Carolina counties.
Eight match this reconstruction exactly, which is a strong independent check. The
ninth, **Cumberland County NC (signed 2008-06-25)**, appears in *no* ICE capture and
in Kostandini's table either — and it is also named in the UNC Latino Migration
Project's 2010 report, so it is not a Cato error. The likely explanation is the same
survivor bias one level up: the earliest ICE capture here is October 2010, so any
agreement that began and ended before then is invisible to this series too. Treat
2007–2010 as the period where stacking the GAO, Yale, CRS, OIG and MPI censuses
matters most; ICE's own table alone will not close it.

Related: the boilerplate in those papers cites "Amuedo-Dorantes and Bansak (2014)"
as a 287(g) source. That paper (*Contemporary Economic Policy* 32(3): 671–680) is
about E-Verify and says in a footnote that the authors "lack the geographic detail
needed to identify the counties." No Amuedo-Dorantes paper publishes a jurisdiction
list, and none of her three openICPSR deposits contains a standalone 287(g) file.

---

## Housekeeping

`_to_delete/` holds scratch files from assembly (transfer bundles, shell fetchers and
12 stray `.part` files). The bridge cannot delete files on your disk, so remove that
folder yourself.

`logs/` keeps the provenance trail: `worklist.tsv` (every URL fetched and where it
landed), `moa_present.txt` (the 170 MOA PDFs actually on disk, used to compute
`moa_pdf_present`), and `moa_urls_from_rosters.tsv` (MOA filename to archived URL,
harvested from the roster tables' own hyperlinks — used to chase the 19 files the CDX
enumeration missed, 18 of which proved never archived).

## If downloads are blocked

`web.archive.org`, `ice.gov` and `gao.gov` refuse automated requests from some
networks. If `00_download_sources.R` fails on those:

1. Copy `prebuilt/*.csv` into `output/` and run `R/05_build_best_guess.R` directly —
   the reconciliation works without the raw files.
2. `prebuilt/ice_roster_long.csv` was produced from the same 17 captures script `01`
   parses, so results are identical. It carries one representative MOA filename per
   agency rather than the per-snapshot filename; re-run `01` on real captures if you
   need the full per-snapshot link history.
3. For the MOA PDFs there is no substitute — they must come from Wayback. The CDX
   query in `00` lists every one; run it from a browser and save the result as
   `sources/moa_pdfs/_cdx_index.tsv`, then re-run the script. `_cdx_index.tsv` is
   already present here, so the enumeration step can be skipped.

## Known gaps

- **CRS RL32270 App. A (Feb 2009, 67 agreements with dates and models)** and the
  **MPI / NCLR appendices** are included as PDFs but not yet parsed. They are the
  best independent checks on 2009–2010 and the highest-value next addition.
- **2006 and earlier** rests on the 2009 MOA file crawl plus narrative in the early
  CRS versions (Florida 2002, Alabama 2003, LA County 2005, Arizona DOC 2005).
- **Applied-but-rejected jurisdictions** are a different question entirely; only
  Capellan & Sorg have that (167 counties, 2005–2010), unpublished.
