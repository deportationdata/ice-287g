# Other sources — kept for reference, not read by the pipeline

Everything here was gathered while reconstructing the pre-2018 history of the
287(g) program and was, until 12 September 2026, parsed into source claims and
counted against stated roster totals. It was then retired from the pipeline,
and the files moved from `inputs/historical/` to this folder, after a check
showed that none of it is load-bearing:

- **No agency.** Every agency any of these sources names is on an ICE
  roster capture we hold (`sheets/`), in ICE's own undated lists of Sep 2007
  and Mar 2008, in the DHS OIG appendix of Oct 2009, or in ICE's MOA archive
  index.
- **No signing date.** Compared against ICE's sheets, the secondary sources
  contribute exactly two dates ICE does not carry, and both are errors:
  Kostandini's Maricopa `2008-03-14` (ICE: 2007-02-07) and NCLR's Riverside
  `2010-04-28` (a typo for 2006-04-28).
- **No activity window.** ICE's captures run every few months from April 2008
  and the OIG appendix covers the Aug 2009 – Apr 2010 gap; the Aug–Oct 2010
  rosters here transcribe ICE's own 29 Oct 2010 sheet.

What the sources did carry was the count machinery itself — stated totals,
tolerances, source counts and a `confidence` grade — which went with them.
The pipeline now records presence (is an agency on a given ICE or OIG
list or not) and conflicts (`data/intermediate/agency-disagreements.csv`), nothing
else. Citations for every file remain in `inputs/historical/SOURCES.csv`.

## What is here

| Folder | Contents |
|---|---|
| `reports/` | GAO-09-109; CRS RL32270 (2007 and 2009 editions); DHS OIG 10-124, 11-119, 12-130; the March 2009 House Homeland Security hearing; MPI *A Program in Flux* and *Delegation and Divergence*; NCLR; CIS; Kostandini et al. (2014) and Charlton & Kostandini (2021); Cato WP 52; McCann, Boateng & Schimchak (2024); ACLU-NC/UNC; Capellan & Sorg (NIJ 305488) |
| `gapfill/` | ICE's per-agency removal counts FY2006–2013 (Marshall Project FOIA compilation); the ALIPAC and MyAttorneyUSA roster mirrors; the 2005, 2006, 2011 and 2012 congressional hearings; the unredacted CRS 2009; the ILRC FOIA compilation; UNC, ACLU-GA, Justice Strategies and HSAC reports |
| `panels/` | East et al. (2023) county-month 287(g) / Secure Communities / E-Verify panel |
| `yale_wirac_287g_foia_2008.html` | Yale WIRAC's index of ICE's January 2008 FOIA production (34 MOUs) |
| `prebuilt/` | The parsed outputs of the retired readers, as they last ran |
| `notes/` | The research narrative: the annotated source guide, the completeness verdict, the gap audit and its status, the ACLU-NC appendix finding. Read them as a record of the search, with the corrections below |
| `correspondence/` | A draft FOIA request to Yale WIRAC, never sent |
| `scripts/` | Shell helpers used to fetch and OCR the gap-fill material |

## Corrections to the notes

The notes were written before the pre-2011 captures of ICE's partners page
were pulled, and three claims in them are wrong:

- ICE's roster series does **not** begin in October 2010. The partners page
  carries a dated STATE / AGENCY / SUPPORT / SIGNED table from **29 April
  2008**, with signing dates back to Florida DLE on 2 July 2002, and undated
  agency lists as of 19 September 2007 and 10 March 2008.
- Barnstable County MA, Framingham MA and Hudson NH are on that April 2008
  table; they are not "orphans" outside ICE's own record. "El Paso, TX" in
  the Yale list is El Paso County, CO.
- Brevard and Manatee County FL appear on ICE's capture of 16 September 2008
  and every capture after it, not "on no roster capture we hold".

## What is still worth having

- **Massachusetts State Police**, MOA signed 13 Dec 2006 and rescinded by Gov.
  Patrick around 12 Jan 2007 with no troopers trained, is the one agreement
  no roster records. It enters the data through
  `inputs/historical/press-claims.csv`, sourced to ICE's own release.
- **Capellan & Sorg's FOIA data** (167 county applications 2005–2010 with
  accepted / denied / implemented status) is the only known source on
  applications that did not become agreements. Never deposited; contact
  Joel Capellan, Rowan University (capellan@rowan.edu).
- **ICE's removal counts** (`gapfill/ICE287g-removals/`) are a descriptive
  dataset of identifications and removals by agency and fiscal year. They
  name no agency and no date the rosters lack.

**Added 13 September 2026:** `ice_agency_to_county.csv`, the hand-built crosswalk
of 105 ICE agency names to a county FIPS or "statewide". An audit found that every
county it supplied is already known from ICE's sheet, the matched geometry or the
rosters, and every statewide level from ICE's TYPE or the agency's name, so the
pipeline stopped reading it. Two of its rows put the independent cities of
Manassas and Manassas Park in Prince William County, and 18 store a FIPS code
without its leading zero.
