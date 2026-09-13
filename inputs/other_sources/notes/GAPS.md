# What I could not get — audit of every source raised in this project

Compiled 12 September 2026, by walking back through the whole investigation.
Ordered by what is worth your time, not by when it came up.

---

## A. Confirmed to exist, not obtained, worth chasing

**1. The Yale WIRAC production — 34 MOU PDFs, 2 ICE cover letters, the 60 MB ZIP**
Definitely existed; the index page proves it. Wayback holds only 404s for the file
directory. Four agencies in it — Barnstable County MA, Framingham MA, Hudson NH,
El Paso TX — appear in no later ICE roster, so for those the clinic's copies may be
the only surviving record of the agreements. Draft email in `correspondence/`.

**2. The 18 never-archived 2017 MOAs**
ICE's own Aug-2017 table links them; Wayback has only 404 captures (verified by CDX).
Flagged `moa_pdf_present = FALSE` in `output/agreements_best_guess.csv`. Three routes:
a FOIA request to ICE naming the agencies and the filenames; the counties themselves,
since a sheriff's MOA is usually a public record locally and often went through a
county commission agenda; or ice.gov's live server, which 403s scripted requests but
may serve them to a browser. All 18 are 2017 jurisdictions — Clay FL, East Baton
Rouge LA, Anne Arundel MD, Horry SC, Knox TN, and thirteen Texas counties.

**3. ICE's own FOIA index pages — never actually viewed**
`https://www.ice.gov/foia/old-287g-memorandums-agreementunderstanding` and
`https://www.ice.gov/identify-and-arrest/287g/287g-archive`. Both 403 to every
scripted request from every network tried, and I never opened them in a browser.
They are ICE's own index of the old agreements and could name MOAs absent from the
170 recovered here. **Cheapest unexplored lead in the project — just open them.**

**4. Amuedo-Dorantes & Arenas-Arroyo's assembled 2001–2015 panel**
IZA DP 10850 states they built a complete 2001–2015 287(g) dataset by reviewing old
websites and prior research. Never deposited; not in any of their three openICPSR
packages. Probably the most complete national agency-year file in existence. Email
the authors.

**5. Capellan & Sorg's FOIA'd application data**
167 counties that *applied* 2005–2010, with accepted / rejected / implemented status
(NIJ 305488). The only known source on **non-adopters**, which is a different and
arguably more useful object than a roster of adopters. No county appendix in the
report; never archived at ICPSR. Email the authors.

**6. Ian Peacock's FOIA'd application letters, 2002–2011**
RSF Journal 11(4). ICE and sheriff-department correspondence at county level. The
online appendix is variable documentation and robustness tables only.

**7. Wesley McCann's per-agency MOA coding**
Offers on request "a more nuanced breakdown of each agency's specific powers and the
model they adhere to, per the MOA." Coverage 2016–2020, so only the tail is pre-2018.

**8. Charlton & Kostandini (2021), AJAE 103(1): 70–89**
Never obtained. Wiley paywalled, no open version, no replication deposit found — the
outcome data are confidential USDA records, which likely explains the absence. Its
287(g) input is the Kostandini 2014 table we already have, so this is about checking
their county panel, not about new data. Get it through a library.

---

## B. Identified and never downloaded — easy, I simply didn't

**9. ILRC, "287(g) FOIA Documents"** — `https://www.ilrc.org/resources/287g-foia-documents`
~30 MB. Published April 2019 but the *records* are pre-2018: applications to join the
program, ICE needs assessments, internal email, and MOAs. The richest FOIA product on
287(g) that exists, and I never pulled it. **Highest-value item in this section.**

**10. UNC Latino Migration Project, Nguyen & Gill (Feb 2010)** —
`https://migration.unc.edu/wp-content/uploads/sites/1383/2019/10/287g_report_final.pdf`
Directly relevant now: it names **Cumberland County NC**, the jurisdiction Cato dates
to 2008-06-25 that appears in no ICE capture. A second independent attestation.

**11. ACLU of Georgia, "Terror and Isolation in Cobb" (Oct 2009)** —
`https://assets.aclu.org/live/uploads/document/asset_upload_file306_41281.pdf`
Report only, no roster. Low value.

**12. Justice Strategies, Shahani & Greene, "Local Democracy on Ice" (Feb 2009)** —
`https://www.prisonlegalnews.org/media/publications/justice_strategies_immigration_law_enforcement_report_2009.pdf`
Cites 63 MOAs as of Aug 2008; appendices start p. 65 and were never checked.

**13. ALIPAC forum mirror of ICE's 29 Oct 2010 table** —
`https://www.alipac.us/f12/287-g-participating-agencies-71-25-states-217118/`
A contemporaneous verbatim mirror. Independent corroboration of a capture we have.

**14. MyAttorneyUSA table frozen at 1 Aug 2017** — 60 agencies, 18 states, with dates.

**15. McCann, Boateng & Schimchak (2024) appendix** — 142+ MOAs with direct ICE doclib
URLs, 2016–2020. `https://link.springer.com/article/10.1007/s12134-024-01122-3`

**16. The Marshall Project, `themarshallproject/ICE287g-removals`** — ICE FOIA removals
by participating department, FY2006–2013, with addresses. Implies participation by
year, though not signing dates.

**17. `appelson/Tracking_287g`** — daily scrape of ICE's current lists plus Wayback back
to Jan 2021. Modern era only; irrelevant pre-2018 but the right tool going forward.

---

## C. Have a mirror, not the canonical original

Everything here is almost certainly byte-identical, but if provenance matters for
citation you may want the publisher's own copy.

| Source | What I have | Canonical |
|---|---|---|
| GAO-09-109 | TRAC mirror | gao.gov — 403s scripted requests, even with a Referer |
| 4 DHS OIG reports | TRAC mirrors | oig.dhs.gov — 403s |
| CRS RL32270 (2009) | everycrsreport.com | **Two co-author names are redacted in this copy.** crsreports.congress.gov 403s; try the UNT Digital Library or FAS for an unredacted scan |
| Kostandini et al. 2014 | JSTOR scan posted by ncaeonline.org | Wiley/AJAE version of record; also check AJAE for supplementary files |
| 170 MOAs | Wayback | ice.gov live server (403s) |

---

## D. Plausibly exists, never pursued at all

**18. DHS Homeland Security Advisory Council, 287(g) Task Force report (2011).** Flagged
in my first research brief and then dropped. A task force report of that kind often
carries a jurisdiction appendix. Unchecked.

**19. ICE congressional budget justifications and annual reports.** These routinely list
program partners and would give an independent annual series. Never looked.

**20. Congressional hearing records beyond the two I checked.** I confirmed the Mar 2009
Homeland Security hearing has the Riley table and the Apr 2009 Judiciary hearing does
not. There were other 287(g) hearings; their submitted-for-the-record material is
unexamined.

**21. Pham & Van's Immigration Climate Index** underlying jurisdiction-level data, and
**Tom K. Wong's (2012)** 287(g) data. Both papers exist; neither dataset surfaced.

---

## E. Confirmed gone — do not spend time

- **Yale's MOU PDFs and ZIP** — Wayback crawled the directory only after removal.
- **18 of the 2017 MOAs** — same failure mode, verified by CDX.
- **A pre-October-2010 ICE roster page.** I checked this today. ICE's 2008–2010 fact
  sheet (`/pi/news/factsheets/070622factsheet287gprogover.htm`, captures 2008-07-23,
  2010-01-07, 2010-02-08) is **narrative only — no roster table**, and the
  `287gparticipatingagencies.pdf` lead was **never archived at all**. So the roster
  series genuinely cannot be pushed back before Oct 2010 from ICE's own site. That
  period rests on GAO (Sep 2007), Yale (Jan 2008), CRS (Feb 2009), OIG (Jun 2009),
  MPI (Jan and Aug 2010) and Kostandini. The three narrative captures are saved as
  `sources/ice_factsheets/ice_287g_pre2011_*.html` for completeness.
- **NDLON / uncoverthetruth.org** — domain no longer resolves, and NDLON v. ICE was
  Secure Communities and detainers, not 287(g). A common misattribution.

---

## F. Not missing — just not yet parsed

I have these PDFs; the pipeline does not read them. This is the cheapest way to
strengthen the reconstruction, and item 1 is the single highest-value addition.

1. **CRS RL32270 App. A** — 67 agreements, Feb 2009, with model **and signing date**.
   The best independent pre-2010 roster in the archive.
2. **MPI, Delegation and Divergence, App. 2** — 72 jurisdictions, Aug 2010, with dates.
3. **MPI, A Program in Flux, App. 1** — 71 active or in negotiation, Jan 2010.
4. **NCLR/Lacayo App. A** — comprehensive list as of 2 Aug 2010, model and date.
5. **Cato WP 52, Table A1** — 9 NC counties with MOA signed dates; the Cumberland row.
6. **GAO-09-109 App. III** — 29 agency names, Sep 2007.
7. **CIS Table 4** — 56 agencies with model; explicitly incomplete.
8. **ACLU-NC / UNC appendices**, pp. 101–144.
