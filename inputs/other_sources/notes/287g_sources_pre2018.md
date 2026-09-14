# Pre-2018 lists of 287(g) agreements — annotated source guide

Compiled 16 August 2026. Every URL below was tested. Where a link is dead or blocked,
the working substitute is given.

The short version: **the single best source is ICE's own 287(g) fact-sheet page, captured
repeatedly by the Wayback Machine between 2010 and 2017.** Each capture is a complete,
dated roster (state / agency / support type / date signed) *and* every row links to the
signed MOA PDF — and those PDFs are themselves archived. That gives you both halves of what
you asked for. Everything else in this guide is corroboration or gap-filling.

---

## 1. ICE fact-sheet snapshots — complete rosters WITH links to the agreements

ICE maintained a live table titled "Mutually Signed Agreements (N) as of MM/DD/YYYY" on its
287(g) fact sheet. It moved URLs twice. Columns: `STATE | LAW ENFORCEMENT AGENCY |
SUPPORT TYPE | DATES SIGNED | MOA`, where MOA is a hyperlink to the signed PDF.

| Era | URL to feed the Wayback Machine |
|---|---|
| 2011–2014 | `http://www.ice.gov/news/library/factsheets/287g.htm` |
| 2015–2017 | `http://www.ice.gov/factsheets/287g` |
| 2006–2010 | `http://www.ice.gov/pi/news/factsheets/070622factsheet287gprogover.htm` |
| landing page | `http://www.ice.gov/287g` |

Snapshot browser: `https://web.archive.org/web/*/ice.gov/news/library/factsheets/287g.htm*`

### Verified distinct editions

| Wayback timestamp | Caption on page | Rows in table | MOA links |
|---|---|---|---|
| 20110220090259 | as of 10/29/2010 | 71 | 70 |
| 20110818151202 | as of 10/29/2010 | 69 | 68 |
| 20111020094214 | as of 09/02/2011 | 69 | 68 |
| 20120304155702 | as of 09/02/2011 | 68 | 67 |
| 20121007043419 | as of 10/02/2012 | 64 | 63 |
| 20130118070325 | as of 10/16/2012 | 57 | 57 |
| 20130402163405 | signed as of 12/31/2012 | 39 | 39 |
| 20130904035853 | signed as of 08/07/2013 | 36 | 36 |
| 20140910162129 | signed as of 08/13/2014 | 35 | 35 |
| 20150106221351 | signed as of 08/13/2014 | 34 | 34 |
| 20150725035210 | (caption stale) | 32 | 32 |
| 20160119003252 | (caption stale) | 32 | 32 |
| 20160820000823 | (caption stale) | 32 | 32 |
| 20161211024641 | (caption stale) | 34 | 34 |
| 20170305225317 | (caption stale) | 37 | 37 |
| 20170702181319 | (caption stale) | 45 | 45 |
| 20170801021304 | (caption stale) | 58 | 58 |

Example of a working capture:
`https://web.archive.org/web/20110220090259/http://www.ice.gov/news/library/factsheets/287g.htm`

**Caveat that matters for citation.** From 2015 onward ICE stopped updating the caption
text — every capture still reads "(35) signed as of 07/25/2014" while the table underneath
kept changing. **Trust the row count and the per-row signed dates, not the caption.** The
2017 growth (32 → 58 rows) is the Trump-era expansion showing up in real time.

**Second caveat.** The `DATES SIGNED` column shows the *current* agreement's date, not the
original. Etowah County reads 2008-07-08 through 2013, then 2013-06-28, then 2016-06-08 —
those are the 2009 standardisation, the 2013 re-signing, and the 2016 re-signing. To recover
first-adoption dates, use the earliest snapshot in which an agency appears.

---

## 2. The signed MOAs themselves — ~170 archived PDFs

ICE hosted the agreements at
`http://www.ice.gov/doclib/foia/memorandumsofAgreementUnderstanding/<file>.pdf`.
That directory is gone from the live site, **but the Wayback Machine has it.**

Get the full inventory:

```
https://web.archive.org/cdx/search/cdx?url=ice.gov/doclib/foia/memorandumsofAgreementUnderstanding*&output=text&fl=original,timestamp,statuscode&filter=statuscode:200&collapse=urlkey&limit=400
```

Roughly 170 captures in four generations:

- **`<agencyname>.pdf`** — original MOAs, crawled 2009-05-06. The pre-2009-revision agreements.
- **`r_287g<agency>MMDDYY.pdf`** — the 2009 standardised re-signings, crawled 2010-05-28.
- **`r_287g<agency>.pdf`** — the 2013–14 re-signings, crawled 2014-04-04 / 2014-09-19.
- **`<Agency>_MOA_<date>.pdf`** and similar — 2016–17 agreements, crawled 2016-12 / 2017-04.

Both formats verified as retrievable, real PDFs:

- `https://web.archive.org/web/20090506010725/http://www.ice.gov/doclib/foia/memorandumsofAgreementUnderstanding/sheriffsofficeofalamancecounty.pdf` (658 KB)
- `https://web.archive.org/web/20100528005110/http://www.ice.gov/doclib/foia/memorandumsofAgreementUnderstanding/r_287getowah100509.pdf` (1.67 MB)

A number of the newer MOAs are *also* still live on ice.gov under the modern path
`https://www.ice.gov/doclib/287gMOA/<lowercaseagencyname>.pdf` — e.g. `pinalcountysheriffsoffice.pdf`,
`pimacountysheriffsoffice.pdf`, `mecklenburgcountysheriffsoffice.pdf`,
`sheriffsofficeofalamancecounty.pdf`. Filenames are not fully predictable; some 403.
ICE's index pages (`/foia/old-287g-memorandums-agreementunderstanding`,
`/identify-and-arrest/287g/287g-archive`) block scripted access but load in a browser.

---

## 3. FOIA drops

**Yale Law School, Worker & Immigrant Rights Advocacy Clinic (2007–08 FOIA)** — the earliest
dedicated 287(g) FOIA release. WIRAC filed 8 March 2007; ICE produced **34 MOUs** on
17 January 2008 plus two cover letters, and WIRAC published them all.

`https://web.archive.org/web/20080807125556/http://islandia.law.yale.edu/wirc/287g_foia.html`

The page — and therefore the 34-agency roster as of January 2008 — is fully readable. The
agencies: Alabama DPS; Alamance Cty NC; Arizona DOC; Arizona DPS; Barnstable Cty MA; Benton
Cty AR; Cabarrus Cty NC; Cobb Cty GA; Collier Cty FL; Colorado DPS; El Paso TX; Florida;
Framingham MA; Gaston Cty NC; Georgia DPS; Herndon VA; Hudson NH; LA Cty CA; Maricopa Cty AZ;
Massachusetts DOC; Mecklenburg Cty NC; Nashville & Davidson Cty TN; New Mexico Corrections;
Orange Cty CA; Prince William–Manassas Regional ADC VA; Riverside Cty CA; Rockingham Cty VA;
Rogers AR; San Bernardino Cty CA; Shenandoah Cty VA; Springdale AR; Tulsa Cty OK; Washington
Cty AR; York Cty SC.

**Bad news:** the PDFs and the 60 MB ZIP are *not* recoverable. Wayback only crawled those
URLs after Yale took the site down — every capture is a 404. Confirmed via CDX. The MOA
documents themselves have to come from Section 2 above.

**ILRC, "287(g) FOIA Documents"** — `https://www.ilrc.org/resources/287g-foia-documents`,
PDF `https://www.ilrc.org/sites/default/files/resources/ice_foia_287g_excerpts_-_complete_final_2.pdf`.
Published April 2019, so outside your window, but the underlying records are pre-2018:
applications to join the program, ICE needs assessments, internal email, and MOAs. The richest
FOIA product on 287(g) that exists. ~30 MB.

**Dead ends, so you don't repeat them:** MuckRock has no pre-2018 287(g) MOA request (only a
2025-vintage one, ID 208503). DocumentCloud has no pre-2018 collection. governmentattic.org has
nothing. NDLON's uncoverthetruth.org no longer resolves, and NDLON v. ICE was Secure
Communities and detainers, not 287(g) — a common misattribution.

---

## 4. Government reports with full rosters

| Source | Roster | As of | URL |
|---|---|---|---|
| **CRS RL32270** (11 Mar 2009) | **App. A — 67 MOAs, 23 states; state / name / type / signed date** | Feb 2009 | `https://www.everycrsreport.com/files/20090311_RL32270_a7bbe8763684424b48f0d4b1d61c92412ac50d0c.pdf` |
| House Homeland Security hearing (4 Mar 2009) | Riley testimony, Attachment 1, pp. 13–14 — the source table behind CRS | Feb 2009 | `https://www.govinfo.gov/content/pkg/CHRG-111hhrg49374/pdf/CHRG-111hhrg49374.pdf` |
| **GAO-09-109** (Jan 2009) | App. III, pp. 33–34 — 29 agencies, names only | 1 Sep 2007 | `https://www.gao.gov/assets/gao-09-109.pdf` |
| **DHS OIG-10-63** (Mar 2010) | App. E, Table 3, p. 83 — 66 agreements, 23 states | Jun 2009 | `https://tracreports.org/tracker/dynadata/2010_04/OIG_10-63_Mar10.pdf` |
| CRS RL32270 (30 Aug 2007) | App. A, p. 31 — agencies + officer counts, no signed dates | 29 Aug 2007 | `https://www.everycrsreport.com/files/20070830_RL32270_137cbfcfdb2783a66987c265ab32bdf06fb9e40b.pdf` |

CRS RL32270's landing page (all versions): `https://www.everycrsreport.com/reports/RL32270.html`

**Counts only, no roster** — useful as cross-checks: OIG-10-124 (71 MOAs as of 1 Aug 2010;
26 TFO / 32 Detention / 13 Joint); OIG-11-119 (69 MOAs, 24 states, as of 1 Jun 2011; 34/20/15);
OIG-12-130 (64 MOAs, 24 states, Aug 2012; 35/20/9); CRS R44627 (32 LEAs in 16 states, Jan 2016,
down from a peak of 64 in 2012). All at `https://tracreports.org/tracker/dynadata/...`.

`oig.dhs.gov` and `ice.gov` block scripted fetches; the TRAC mirrors serve identical files.

---

## 5. Nongovernmental rosters

**With signed dates — usable as standalone point-in-time lists:**

- **Kostandini, Mykerezi & Escalante (2014)**, *Am. J. Agricultural Economics* 96(1):172–192.
  **Table A1 lists every 287(g) jurisdiction with adoption dates, 2002–2010** (69 jurisdictions
  incl. 10 states). The most panel-ready published reconstruction.
  `https://www.ncaeonline.org/wp-content/uploads/2021/03/Konstandini-et-al.-2014-Impact-of-Immigration-Enforcement-on-U.S.-Farming.pdf`
- **MPI — "A Program in Flux" (Mar 2010)**, Appendix 1: "Active MOAs and Those in Negotiation,
  January 2010" — 71 total across 30 states, with signing dates.
  `https://www.migrationpolicy.org/sites/default/files/publications/287g-March2010.pdf`
- **MPI — "Delegation and Divergence" (Jan 2011)**, Appendix 2: "Active 287(g) MOAs, August 2010"
  — 72 jurisdictions, model type, original signing date.
  `https://www.migrationpolicy.org/sites/default/files/publications/287g-divergence.pdf`
  (the widely-cited `/pubs/287g-divergence.pdf` path now 404s)
- **NCLR/UnidosUS — Lacayo (2010)**, "Appendix A: Comprehensive List of 287(g) Agencies as of
  August 2, 2010" — ~71 agreements, model, date signed.
  `https://unidosus.org/wp-content/uploads/2021/07/287g_issuebrief_pubstore.pdf`
- **ALIPAC forum mirror (Feb 2011)** — a verbatim copy of ICE's table as of 29 October 2010,
  71 agreements in 25 states. Crude source, but it is a genuine contemporaneous mirror and
  independently corroborates the Wayback capture above.
  `https://www.alipac.us/f12/287-g-participating-agencies-71-25-states-217118/`
- **MyAttorneyUSA (pub. 2021, table frozen 1 Aug 2017)** — 60 agencies, 18 states, all Jail
  Enforcement, with signing dates. Post-2018 publication but a clean pre-Trump-expansion endpoint.
  `https://myattorneyusa.com/immigration-blog/deportation-and-removal/criminal-aliens/list-of-current-section-287g-agreements-between-ice-and-local-authorities/`

**Partial or agency-level:**

- **CIS — Vaughan & Edwards (Oct 2009)**, "The 287(g) Program: Protecting Home Towns and
  Homeland." Table 4 names 56 agencies with arrests by FY, officers trained, and MOA type;
  Table 14 lists the 9 agreements approved July 2009. Explicitly incomplete (non-reporting
  agencies omitted). `https://cis.org/sites/cis.org/files/articles/2009/287g.pdf`
- **Cato WP 52 — Forrester & Nowrasteh (Apr 2018)**, Table A1: 9 NC counties with MOA signing
  dates 2006–2009. `https://www.cato.org/sites/cato.org/files/pubs/pdf/working-paper-52-updated.pdf`
- **ACLU-NC + UNC Immigration & Human Rights Policy Clinic (Feb 2009)**, "The Policies and
  Politics of Local Immigration Enforcement Laws: 287(g) in North Carolina." Appendices A–C with
  exhibits span pp. 101–144, built from public-records requests — worth opening manually.
  `https://law.unc.edu/wp-content/uploads/2019/10/287gpolicyreview.pdf`
- **UNC Latino Migration Project — Nguyen & Gill (Feb 2010)**: names the 9 NC jurisdictions,
  no dates table. `https://migration.unc.edu/wp-content/uploads/sites/1383/2019/10/287g_report_final.pdf`
- **ACLU of Georgia (Oct 2009)**, "Terror and Isolation in Cobb" — report only, no roster.
  `https://assets.aclu.org/live/uploads/document/asset_upload_file306_41281.pdf`
- **McCann, Boateng & Schimchak (2024)**, *J. Int. Migration & Integration* — appendix of 142+
  MOAs with direct ICE doclib URLs, but coverage is 2016–2020.
  `https://link.springer.com/article/10.1007/s12134-024-01122-3`

**Checked, nothing there:** ILRC's national map (current-only Datawrapper embed, no history);
ACLU national 287(g) document hub; Immigration Policy Center Nov 2012 brief (aggregates only);
Police Foundation 2009; Pham 2018 W&L L. Rev.; Michaud 2010 Ariz. L. Rev.; TRAC (detainers and
removals, not agreements); GitHub `appelson/Tracking_287g` (scrapes the *current* list only);
NCSL, Brookings, Urban, Pew, Heritage, Manhattan Institute, National Immigration Forum, MALDEF,
LULAC, SPLC. Also note: the American Immigration Council's 2012 overview PDF was **overwritten
in place** — that 2012 URL now serves a 2025 fact sheet, so cite the Wayback copy.

---

## 6. People who hold unpublished pre-2018 panels

Worth an email; none deposited data publicly.

- **Amuedo-Dorantes & Arenas-Arroyo**, IZA DP 10850 (2017) — state they assembled a complete
  **2001–2015** 287(g) dataset from archived ICE pages plus Amuedo-Dorantes & Bansak (2014) and
  Kostandini et al. Probably the most complete national agency-year panel in existence.
  `https://docs.iza.org/dp10850.pdf`
- **Ian G. Peacock**, *RSF Journal* 11(4):26 (2025) — FOIA'd ICE and sheriff **application
  letters, 2002–2011**. `https://www.rsfjournal.org/content/11/4/26/tab-supplemental`
- **Capellan & Sorg**, NIJ final report (Nov 2022) — FOIA'd ICE data on **167 counties that
  applied 2005–2010**, with accepted/rejected/implemented status.
  `https://www.ojp.gov/pdffiles1/nij/grants/305488.pdf`
- **Wesley McCann** — offers per-agency powers and model breakdowns from the MOAs on request.

---

## 7. Suggested order of attack

1. Run `harvest_287g.py` (accompanying) to rebuild the full ICE roster panel and pull every
   archived MOA PDF. That is the backbone.
2. Layer CRS RL32270 App. A (Feb 2009) and the Yale WIRAC list (Jan 2008) underneath it —
   they cover the 2007–2009 period before ICE's own table starts.
3. Cross-check counts against GAO-09-109 (Sep 2007), OIG-10-63 (Jun 2009), OIG-10-124
   (Aug 2010), OIG-11-119 (Jun 2011), OIG-12-130 (Aug 2012).
4. Use Kostandini Table A1 and the two MPI appendices to validate first-adoption dates, which
   the ICE table overwrites on re-signing.
5. Email Amuedo-Dorantes/Arenas-Arroyo before rebuilding anything from scratch.
