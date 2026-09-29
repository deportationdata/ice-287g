# Historical inputs — ICE's records of the program before the live scraper

The pipeline reads one file here: the newest `ice_live_287gMOA_index_*.tsv`
snapshot, which `code/0-acquire-archive-index.R` compares against ICE's live
`/287g-archive` index to find newly posted MOAs. The rest is kept for
reference and citation; the secondary sources live in `inputs/other_sources/`
(see its README).

| File | What it is |
|---|---|
| `ice_live_287gMOA_index_<date>.tsv` | ICE's `/287g-archive` index of MOA documents with original signing dates, one snapshot per change. It also files non-287(g) documents (a Cook County forfeiture MOU, a Morristown application letter). Seven signings ICE's sheets never dated take their date from it, by hand, in `inputs/unlisted-signings.csv` (`signed_source` `ICE archive index`) |
| `reports/DHS-OIG-10-63_Mar2010.pdf` | DHS OIG-10-63, Appendix E Table 3: all 67 jurisdictions as of 28 Oct 2009 with model, original signing date and signed/pending status, from ICE OSLC data. The only roster between the Aug 2009 and Apr 2010 captures |
| `SOURCES.csv` | The citation and provenance record for every file here and under `inputs/other_sources/` |

`sheets/sheets_wayback_pre2011/` also holds ICE's undated agency lists that
preceded its dated table: "Signed MOAs as of 9-19-07" (28) and "Agencies with
signed MOAs (updated 3-10-08)" (41).

## Why none of this feeds the agencies file

Until 29 September 2026, `7-make-agencies.R` reduced the undated lists, the
archive index, the OIG appendix and one ICE press release to per-agency
claims and arbitrated them against ICE's sheets, so the agencies file could
carry an earlier signing date or a wider active window than the agreements
file. A check of every claim against the agreements showed:

- **No missing agreement.** Every agency on the undated lists and in the OIG
  appendix already holds a signed agreement in the agreements file by that
  list's date. Every dated entry in the archive index is an MOA the
  agreements file already links, sometimes dated a few days differently
  (Cobb, Benton, Frederick, Cabarrus, Rensselaer, Waukesha), once with month
  and day swapped (Cape May, `2017-10-04` for `2017-04-10`), and once an
  addendum already recorded in `inputs/moa-addenda.csv` (Monmouth,
  2019-03-08). The OIG appendix's pending entries (Mesa, Charleston, Rhode
  Island DOC) were not agreements on its date; the first two signed later
  and the third never did.
- **One agreement no ICE list records.** Massachusetts State Police signed
  a task force MOA on 13 Dec 2006 (ICE news release) that Gov. Patrick
  rescinded on 11 Jan 2007 with no troopers trained. It is on no ICE list and
  is left out of both files; its citation is in `SOURCES.csv`.

The agencies file is now a summary of the agreements file, so the two cannot
disagree. A signing date the archive index or the OIG appendix gives that
should replace ICE's sheet date goes in `inputs/signed-date-fixes.csv` once
verified against the MOA's signature page, so both files change together.

## What the record can and cannot show

- ICE's own count of every agreement ever signed through mid-2009 — OIG-10-63
  reports 66 approved of 117 applications plus one approved by INS, one since
  terminated (Barnstable) — matches the 67 agencies on ICE's captures by
  May 2009 exactly. An agreement rescinded before anyone trained is evidently
  booked as a withdrawn application, which is why Massachusetts State Police
  is outside that count.
- ICE's monthly release indexes from Mar 2003 to Aug 2008 carry no 287(g)
  release before Feb 2005; Florida (2002) and Alabama (2003) have no release.
- Alabama's 2003 agreement is dated 10 Sep 2003 by ICE's table and OIG, and
  "November 2003" by ICE's 2006–07 fact-sheet prose. Florida's was a one-year
  pilot from 2002 that expired 1 Sep 2003 and was renewed 26 Nov 2003; ICE's
  table carries only the 2002 date.
