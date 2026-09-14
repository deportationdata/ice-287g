# Historical inputs — the program's record before the live scraper

The agency history is built from ICE's own records plus one DHS OIG
appendix. Every file here is read by `code/1-read-historical-*.R` or
`code/2-make-agencies.R`; the secondary sources the pipeline does not read
live in `inputs/other_sources/` (see its README).

## Sources the pipeline reads

| Source (`source_id`) | What it is | Where |
|---|---|---|
| `ice_sheet` | Every archived capture of ICE's 287(g) roster, from the partners-page table of 29 Apr 2008 (signing dates back to Florida DLE, 2 Jul 2002) through the fact-sheet series to today's participating-agencies sheet | `sheets/` |
| `ice_lists` | ICE's undated agency lists that preceded the table: "Signed MOAs as of 9-19-07" (28) and "Agencies with signed MOAs (updated 3-10-08)" (41) | `sheets/sheets_wayback_pre2011/` |
| `ice_archive_index` | ICE's live `/287g-archive` index of MOA documents with original signing dates, retrieved 20 Apr 2026. It also files non-287(g) documents (a Cook County forfeiture MOU, a Morristown application letter), so it may not add an agency | `ice_live_287gMOA_index_2026-04-20.tsv` |
| `oig_2009` | DHS OIG-10-63, Appendix E Table 3: all 67 jurisdictions as of 28 Oct 2009 with model, original signing date and signed/pending status, from ICE OSLC data. The only roster between the Aug 2009 and Apr 2010 captures, and the record of the Oct 2009 re-signing wave | `reports/DHS-OIG-10-63_Mar2010.pdf` |
| `ice_press` | Agreements ICE announced that never reached a roster, one row per claim with its evidence url. Today: Massachusetts State Police, signed 13 Dec 2006 (ICE release), rescinded 12 Jan 2007 (press) | `press-claims.csv` |

`source-registry.csv` registers each with its as-of date and provenance file;
only `ice_press` may add an agency. `SOURCES.csv` is the citation
and provenance record for every file, including those under
`inputs/other_sources/`. `crosswalk/ice_agency_to_county.csv` is the hand-built
jurisdiction crosswalk for agencies the modern sheet never typed.

## How the record is built

- `2-make-agencies.R` first reduces each source to typed claims (`listed`,
  `pending`, `signed`, `model`, `moa_file`, `rescinded`) and resolves each to a
  agency by rule — the registry of ICE's spellings and the alias table,
  then a name that begins exactly one agency, then a signing date unique
  in the state. Every claim is written to `data/intermediate/historical-source-claims.csv`; what no
  rule resolves is in `data/intermediate/historical-source-claims-unresolved.csv`, never dropped.
- It then writes one row per agency: ICE's listing window
  and removal window, the earliest signing date and its source, every signing
  date every source gives, the window the evidence speaks to
  (`attested_active_from/to`), MOA archive status and `source_ids`. Where a
  source and ICE differ — a date, a model, a state, or presence on the nearest
  ICE publication — the difference is recorded in
  `data/intermediate/agency-disagreements.csv`. There are no stated-total checks,
  source counts or confidence grades: an agency is on a list or it is not.

## What the record can and cannot show

- ICE's own count of every agreement ever signed through mid-2009 — OIG-10-63
  reports 66 approved of 117 applications plus one approved by INS, one since
  terminated (Barnstable) — matches the 67 agencies on ICE's captures by
  May 2009 exactly. An agreement rescinded before anyone trained is evidently
  booked as a withdrawn application, which is why Massachusetts State Police
  is outside that count and enters through `press-claims.csv`.
- ICE's monthly release indexes from Mar 2003 to Aug 2008 carry no 287(g)
  release before Feb 2005; Florida (2002) and Alabama (2003) have no release.
- Alabama's 2003 agreement is dated 10 Sep 2003 by ICE's table and OIG, and
  "November 2003" by ICE's 2006–07 fact-sheet prose. Florida's was a one-year
  pilot from 2002 that expired 1 Sep 2003 and was renewed 26 Nov 2003; ICE's
  table carries only the 2002 date.
