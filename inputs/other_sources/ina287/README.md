# ina287.org, from the Wayback Machine

ina287.org ("INA 287: Enforcement activities of Immigration and Customs
Enforcement") was a small WordPress site run by J Cox, captured 2011–2016.
Everything the Wayback Machine holds of it is here, one file per distinct
version, fetched by `../scripts/ina287_wayback.sh` on 26 September 2026.

## What the site was

Seven pages (home, About, 287(g), 287(g) training materials, Raids, FOIA, an
empty "Uncategorized" archive). Their text never changed across 2013–2016
captures; the differences between captures are spam links injected into
the header and sidebar. The site hosted no documents of its own. Its
documents were Scribd uploads by the same author, and those are the
substance:

| Scribd id | Document | Editions we hold |
|---|---|---|
| 23298564 | *ICE 287(g) Primary Source Documents*: every agency in the program, original MOA date, Oct 2009 standard MOA date, withdrew / in negotiations, IGSA | 15 Feb 2010 (Wayback, 29 Mar 2010) and 8 May 2010 (live) |
| 23298577 | *New ICE 287(g) MOAs*: the agencies that signed the Oct 2009 standard MOA | 8 May 2010 only (the 11 Dec 2009 edition ina287 linked is lost) |
| 26902042 | *State & Local Jurisdictions Applying / Expressing Interest*: ICE's FOIA'd application file, with request, disposition (denied, withdrew, ?) and date | 15 Feb 2010 (live, and three 2010 captures) |
| 21968xxx, 23355697, 23355706 | 287(g) officer training workbooks, LESC manual, M-69, refresher-course and state-training summaries | live pages for most; 10 are deleted from Scribd (410) and only a few have Wayback copies |

The uploader replaced files in place on Scribd, so a live page shows the
latest edition only. Earlier editions survive only where Wayback caught
the page's text in 2010. Scribd pages carry the document's text but not its
hyperlinks, so the per-agency MOA and "FOIA docs" links in the chart are not
recoverable here, and the PDFs themselves need a Scribd login to download.

Not archived anywhere we could find: the Google spreadsheet and the drop.io
"foiadatabases" folder linked from the FOIA page.

## Files

| Path | Contents |
|---|---|
| `site/<page>/<timestamp>_<status>.html` | every distinct capture of every page, raw (Wayback `id_`) |
| `captures.tsv` | the site captures fetched |
| `links.tsv` | every link on the site, first and last capture it appeared in, and the pages carrying it |
| `docs.tsv` | on-site documents (none) |
| `scribd/<id>/wayback_<timestamp>.html`, `live_<date>.html` | every distinct Wayback copy of each linked Scribd page, and the page today, each with a `.txt` of its text |
| `scribd.tsv` | per Scribd id: title as linked, Wayback copies, live status and current URL |
| `primary_source_chart_2010-05-08.csv` | the 8 May 2010 chart as a table (78 agencies) |

## Checked against our data (26 September 2026)

All 76 signed agencies in the 8 May 2010 chart are in `data/agreements.parquet`,
with the same original signing date, except one:

- **Durham Police Department (NC)**: the chart says 02/02/2008; ours says 2008-02-01.

The chart also gives Florida Department of Law Enforcement a second date,
12/8/03, beside 07/02/2002. That is the Governor's signature on the 2003
renewal MOU (`stateofflorida.pdf`, recorded in `inputs/moa-signatures.csv`),
which moved the 2002 INS pilot to the new DHS: DHS 11/15/03, ICE 11/26/03,
FDLE Commissioner 12/2/03, Governor 12/8/03. It renews the 2002 agreement
rather than starting a new one, which is why we carry only 2002-07-02.

The two chart rows we lack were never signed: Morristown PD (NJ) and Rhode
Island DOC, both "in negotiations(?)" with no prior MOA. The chart's names
for Herndon ("Herndon County Sheriff's Office") and a few others are the
author's slips; they match our rows by state and date. The Oct–Nov 2009
standard-MOA dates (and Orange County's 4/16/2010) are re-signings of
existing agreements, which our data records under the original date.

The applications file (26902042) has no counterpart in the pipeline, which
records agreements only.
