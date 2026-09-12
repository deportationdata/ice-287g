# Gap list — status after the systematic retrieval pass (12 Sep 2026)

## Obtained

| Item | Result |
|---|---|
| **ICE's live `/287g-archive` index** | **141 agreements with signing dates**, ICE's own curated list, updated 2026-04-20. Saved as `logs/ice_live_287gMOA_index_2026-04-20.tsv`. This was the "cheapest unexplored lead" and it was the biggest one. |
| **The 18 "permanently lost" 2017 MOAs** | **All 18 recovered.** They were archived under `/doclib/287gMOA/`, not the `/doclib/foia/memorandumsofAgreementUnderstanding/` path my original CDX query covered. My earlier claim that they were gone was wrong. |
| **79 further MOAs** absent from the ice-287g repo | Downloaded, 131 MB, in `sources/moa_pdfs_287gMOA/`. Includes Illinois, Nebraska, New York and Wisconsin agreements, states absent from the pre-2018 reconstruction entirely. |
| ILRC 287(g) FOIA dump | **90 MB**, `sources/gapfill/ILRC_287g_FOIA_excerpts.pdf` |
| UNC Latino Migration Project (Nguyen & Gill 2010) | obtained |
| ACLU of Georgia, "Terror and Isolation in Cobb" | obtained |
| Justice Strategies, "Local Democracy on Ice" (2009) | obtained, 12 MB |
| ALIPAC mirror of ICE's 29 Oct 2010 table | obtained |
| Cato WP 52 | obtained (needed a Referer header) |
| ACLU-NC / UNC (Feb 2009) | obtained, 6.4 MB |
| CRS RL32270 **unredacted** | obtained via PolicyArchive — the two co-author names EveryCRSReport blanks are **Karma Ester** and **Michael John Garcia** |
| **2 new congressional hearings** | House Homeland Security, 27 Jul 2005 (`CHRG-109hhrg28332`) — 287(g) is the exclusive subject, earliest in the archive; and the Gastonia NC field hearing, 25 Aug 2006 (`CHRG-109hhrg36029`). Neither prints a roster table, but both enumerate the whole program in testimony because it was still small. |
| 2 further hearings (2011, 2012) | obtained; no rosters |
| HSAC Task Force on Secure Communities (2011) | obtained — **checked and contains no 287(g) roster**, so that lead is closed |

## Confirmed not to exist / not obtainable

- **No HSAC or DHS advisory-body report specific to 287(g).** The Secure Communities one is the only such report and has no roster.
- **No congressional hearing 2005–2017 printed a formal roster table.**
- **Pham & Van's Immigration Climate Index jurisdiction data** is explicitly unpublished ("on file with authors and New York University Law Review"). Their GitHub repo contains no data files.
- **Tom K. Wong (2012)** has no open-access copy anywhere (confirmed via OpenAlex: `oa_status: closed`), and no replication data.
- **McCann et al. (2024)** is closed access, and its MOA appendix is embedded in the article body — there is no supplementary file to fetch even with access.
- **DHS FY2009 and FY2010 full Congressional Budget Justifications** do not appear to be online; only Budget-in-Brief.
- **MyAttorneyUSA** and **dhs.gov** now 403 scripted requests regardless of headers.
- **ice.gov itself** blocks non-browser clients at the WAF layer — headers do not help. Everything must come through Wayback or a real browser.

## Readers built

`R/06_parse_secondary_rosters.R` parses the appendices that were previously sitting unread:

| Source | Parsed | Stated | Status |
|---|---|---|---|
| CRS RL32270 App. A (Feb 2009) | 67 | 67 | exact |
| GAO-09-109 App. III (Sep 2007) | 29 | 29 | exact |
| Cato WP 52 Table A1 | 9 | 9 | exact |
| MPI Delegation and Divergence App. 2 (Aug 2010) | 68 | 72 | short by 4 |
| MPI A Program in Flux App. 1 (Jan 2010) | 70 | 71 | short by 1 |
| NCLR/Lacayo App. A (Aug 2010) | 69 | 71 | short by 2 |

Output: `output/secondary_rosters_long.csv`, 312 rows, 283 with signing dates.

**32 rows are flagged `needs_review`.** MPI's "A Program in Flux" and the NCLR brief centre their state label vertically across a block and wrap long agency names, so some rows emerge with the state bled into the agency field or the agency truncated to its tail. Those rows carry real dates and are kept, but flagged so nothing downstream treats them as clean. The four exact parsers are unaffected.

Not parsed, deliberately: **CIS Table 4** (no signing dates, and the authors state it omits non-reporting agencies) and the **ACLU-NC/UNC appendices** (scanned exhibits, not tabular).

## What the new readers found

Three jurisdictions that appear in **no ICE roster capture and not in Kostandini**, all from CRS's February 2009 appendix:

- **Brevard County Sheriff's Office, FL — signed 2008-08-13**
- **Manatee County Sheriff's Office, FL — signed 2008-07-08**
- **Cumberland County Sheriff's Office, NC — signed 2008-06-25** (independently corroborates Cato and the UNC report)

And first-ever signing dates for three of the four "orphans" that predate ICE's surviving tables:

- Barnstable County Sheriff's Office, MA — 2007-08-25
- Framingham Police Department, MA — 2007-08-14
- Hudson Police Department, NH — 2007-05-05

This strengthens the earlier finding: ICE's own roster series, which begins October 2010, misses agreements that started and ended before it. The 2007–2010 window genuinely requires the stacked censuses.

## Leads closed by OCR

**ILRC FOIA production — checked, no roster.** The 90 MB, 713-page compilation at `gapfill/ILRC_287g_FOIA_excerpts.pdf` was OCR'd at 300 dpi and read through: it contains **no roster table and no agreement census**. What it holds is procedural correspondence — application letters, internal routing, negotiation traffic — which documents individual agencies' dealings with ICE but adds nothing to the agreement reconstruction. The lead is closed. `ocr_ilrc_macos.sh` (and the Linux variant `ocr_ilrc.sh`) regenerate the page-level OCR from the PDF if the compilation ever needs searching again; the text itself is not committed, since nothing in the pipeline reads it.

**ACLU-NC / UNC appendices — closed, nothing there.** Pages 106–152 (report pp. 101–144) were rendered and OCR'd: no roster, no signing dates, zero pages naming five or more counties. They are North Carolina public-records responses, jail and arrest logs and local statistics — case documentation for the report's NC fieldwork, not a national list. See `derived/aclunc-appendices-finding.md`.

## ICE's own removal statistics: the earliest agency-level evidence we have

`gapfill/ICE287g-removals/287g_removals-2006_2013_ICE.csv` carries ICE's per-agency identification and removal counts for FY2006–FY2013, released in three tranches (two via MuckRock FOIA requests) and geocoded by the ICE287g-removals project. Because a nonzero count is ICE attesting that the agreement was *operating* in that fiscal year, this is roster-grade activity evidence under the project's source-authority rule — and FY2006 opens on **2005-10-01**, two and a half years before the roster series begins.

Parsed by `code/1-read-pre2018-ice-removals.R` into `data/pre2018-ice-removals-long.csv` (73 agencies × 8 fiscal years, 383 agency-years attesting activity, every state resolved) and folded into the reconciliation, where it:

- pushes **23 partnerships' active windows before GAO's September 2007 census**, six of them to FY2006 — Arizona DOC, Los Angeles County, Riverside County, San Bernardino County, Collier County FL and Mecklenburg County NC;
- promotes **12 partnerships from `medium` to `high` confidence**;
- supplies the only evidence for **Brevard and Manatee County, FL**, which appear on no roster capture we hold, and a third independent attestation for **Hudson, NH** and **Cumberland County, NC**.

It is explicitly *not* a census — an agency reporting no activity is simply absent — so it does not appear in the completeness ledger.

**Independent check on the final pre-modern roster.** `gapfill/MyAttorneyUSA_roster_2017-08-01.tsv` mirrors ICE's participating-agencies table for 2017-08-01, the same date as our own last fact-sheet capture, and is now parsed as a secondary roster. It lists 60 agencies where ICE's page shows 58 — the only independent read on that date.

## Attempted and not obtained

Three retrievals were started and left incomplete. None is a live gap:

- **CRS RL32270, unredacted copy from FAS** — abandoned partway; superseded by the PolicyArchive copy, which is committed as `gapfill/CRS-RL32270_2009-03-11_UNREDACTED.pdf`.
- **ICE 287(g) End-of-Year Report, FY2019** — partial download, not retried. Out of the pre-2018 scope, but it would be the natural source for FY2019 activity if that era is ever built out.
- **MyAttorneyUSA roster page (HTML)** — the table was extracted to `gapfill/MyAttorneyUSA_roster_2017-08-01.tsv` instead, which is what the parser reads.

Separately, `logs/moa_retrieval_failures.tsv` records 22 MOA urls that failed with a connection error on the first pass. All 22 were obtained on retry and are present in `agreements/agreements_wayback_pre2018/`; the log is kept so the archive shows the episode rather than appearing gap-free by luck.

**Damaged PDF rows are now repaired, not just flagged.** The `needs_review` rows from MPI "A Program in Flux" and the NCLR brief are matched on signing date against the sources that parse exactly (CRS, GAO, MPI D&D) and against ICE's own roster, accepting only an unambiguous match whose agency name ends with the damaged fragment. That repairs 28 of the 33 flagged rows — including their state — and records it in a `repaired` column; 5 remain flagged.

Note that this repair does **not** address a separate, larger problem: roughly half the MPI "A Program in Flux" and NCLR rows carry the *wrong state* without being flagged at all, because those reports centre the state label vertically within each block and the parser carries the previous block's state forward to every row above the label. Alamance County NC is labelled Missouri (MPI Flux) and New Mexico (NCLR); Los Angeles County is labelled Utah. Fixing it needs a two-pass state resolution in `parse_blocked`, not a date match.
