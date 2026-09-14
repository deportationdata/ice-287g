# Can we assert a *complete* list of 287(g) agreements for any period?

Answer for the Amuedo-Dorantes corpus first, then the periods where a complete-census
claim actually holds, and the one trap that invalidates the most tempting claim.

---

## 1. The Amuedo-Dorantes corpus: no

I checked ~22 of her papers in full text (not abstracts), plus every replication package
she has deposited. **None contains a list of 287(g) jurisdictions with adoption dates.**
Not in an appendix, not in supplementary materials, not in posted data.

What her papers do contain is a *policy-description* table — usually "Table A1: Description
of Enforcement Laws" — explaining what 287(g) is (task force / jail / hybrid) and naming
sources. No jurisdictions, no dates. The 287(g) variable is always collapsed into an
aggregate enforcement index before anything is reported.

Her three openICPSR deposits:

| Paper | Deposit | 287(g) file? |
|---|---|---|
| Amuedo-Dorantes & Arenas-Arroyo, AEA P&P 2018 | [10.3886/E114487V1](https://doi.org/10.3886/E114487V1) | No — `.do` files only, no data |
| Amuedo-Dorantes & Lopez, AER P&P 2015 | [10.3886/E113421V1](https://doi.org/10.3886/E113421V1) | No — enforcement pre-merged as an MSA index |
| Amuedo-Dorantes, Arenas-Arroyo & Schmidpeter, AEA P&P 2021 | [10.3886/E139341V1](https://doi.org/10.3886/E139341V1) | No — enforcement pre-merged into analysis files |

### The broken citation chain — worth knowing before you cite her method

From ~2016 onward her papers use a boilerplate sentence, e.g. [IZA DP 10850](https://docs.iza.org/dp10850.pdf), §3.1.2:

> "Data on the implementation of 287(g) agreements at the state level is gathered for the
> 2001 through 2015 period from the [ICE] 287(g) Fact Sheet website, Amuedo-Dorantes and
> Bansak (2014), and Kostandini et al. (2013). Since the ICE website contains only a list of
> the current active agreements, we review old websites and prior research using these
> agreements to ensemble a complete dataset spanning from 2001 to 2015."

**"Amuedo-Dorantes and Bansak (2014)" does not contain 287(g) data.** It is
*Contemporary Economic Policy* 32(3): 671–680, an E-Verify paper. Its working-paper version
([IZA DP 7419](https://docs.iza.org/dp7419.pdf)) mentions 287(g) exactly once, in footnote 3,
to say: *"Unfortunately, we lack the geographic detail needed to identify the counties."* Its
only appendix table is state E-Verify enactment dates.

So the stated three-source method reduces to two real sources: **ICE's fact-sheet website**
(which is exactly the Wayback material from the previous round) and **Kostandini et al.**
A parallel series of her papers cites "Amuedo-Dorantes & Puttitanun (2014)" instead — that
one *does* use 287(g) ([IZA J. Migration 3:6](https://link.springer.com/article/10.1186/2193-9039-3-6),
open access), but it also publishes no list, and it too sources from ICE + Kostandini.

Bottom line: her corpus is downstream of sources you already have. There is nothing to
extract from it that you cannot get better elsewhere.

---

## 2. Periods where a complete-census claim IS defensible

Each of these is a full enumeration of agreements **active on a stated date**, from a source
that was trying to be exhaustive.

| As-of date | N | Unit | Dates? | Source |
|---|---|---|---|---|
| 1 Sep 2007 | 29 agencies | agency | no | GAO-09-109 App. III, pp. 33–34 |
| 17 Jan 2008 | 34 MOUs | agency | in docs | Yale WIRAC FOIA release |
| Feb 2009 | 67 agreements | agency | **yes** + model | CRS RL32270 App. A / Riley testimony |
| Jun 2009 | 66 agreements | agency | no | DHS OIG-10-63 App. E, Table 3 |
| ~2002–Oct 2009 | 69 jurisdictions | county + state | **yes, exact** | Kostandini et al. 2014, Table A1 |
| Aug 2010 | 72 jurisdictions | agency | **yes** | MPI "Delegation and Divergence" App. 2 |
| 17 dates, Oct 2010 → Aug 2017 | 71 → 32 → 58 | agency | **yes** + MOA PDF links | ICE fact-sheet Wayback captures |

The ICE captures are the strongest: they are the agency's own operational roster, they carry
signing dates and model type, and every row links to the signed MOA.

---

## 3. The trap: point-in-time rosters read backwards are survivor-biased

This is the thing most likely to bite you, and it is exactly what Kostandini's Table A1 does.

Kostandini et al. Table A1 ("287(g) Contracts Signed," 69 jurisdictions, 2 Jul 2002 –
15 Oct 2009) is **not** an independently assembled history. Its source note is
`http://www.ice.gov/news/library/factsheets/287g.htm` — it is a transcription of ICE's
roster as it stood around 2010–11, with the signing-date column read backwards.

That means it enumerates *agreements signed 2002–2009 that were still active in 2010*, not
*all agreements signed 2002–2009*. Agreements that started and ended inside the window are
invisible.

Concretely, these appear in the January 2008 Yale FOIA release but are **absent from ICE's
October 2010 table**, and therefore absent from Kostandini:

- Barnstable County Sheriff's Office, MA
- Framingham Police Department, MA
- Hudson Police Department, NH
- El Paso, TX

So: **"Kostandini gives a complete list for 2002–2009" is false.** The defensible version is
"complete for agreements signed 2002–2009 and still active as of ICE's 2010 roster."

The fix is to **stack the censuses** in §2. Their union — GAO 2007 ∪ Yale 2008 ∪ CRS 2009 ∪
OIG 2009 ∪ MPI 2010 ∪ 17 ICE captures ∪ the 2009 MOA file crawl — supports a genuine
"these are all the agreements" claim for **roughly 2007 through 2017**. Before 2007 the
program is small enough (Florida 2002, Alabama 2003, LA County 2005, Arizona DOC 2005) that
the early CRS versions plus the 2009 MOA crawl close it out.

---

## 4. The one continuous panel that exists — and how good it is

**East, Hines, Luck, Mansour & Velásquez (2023),** *JOLE* 41(4), publish their policy data
openly on GitHub. This is the only county-level continuous 287(g) panel I found deposited
anywhere.

- Repo: https://github.com/cneast/East_etal_2022
- File: `data/287g_SC_EVerify_5_13_22.dta` — **county × year × month**, 3,140 counties,
  1996–2016, with `jail287g`, `task287g`, `state287g`, plus Secure Communities and E-Verify
- Real coverage of local 287(g): **Feb 2005 – Dec 2015**, 51 counties ever, 19 terminations captured
- Provenance (Appendix A.1): ICE reports, DHS, MPI, Kostandini et al., and news articles —
  i.e. the Kostandini list *extended with termination dates*, which is its main value-add

### Validation against ICE's own rosters

I diffed it against the ICE captures. Totals track well; membership does not always.

| ICE table date | ICE counties | East counties | In ICE, missing from East | In East, absent from ICE |
|---|---|---|---|---|
| 2010-10 | 50 | 50 | Lexington SC | Hillsborough NH |
| 2011-09 | 49 | 50 | Lexington SC | Hillsborough NH, Guilford NC |
| 2012-10 | 48 | 49 | Alamance NC, Lexington SC | Pima AZ, Hillsborough NH, Guilford NC |
| 2012-12 | 38 | 48 | Cabarrus NC, Lexington SC | **12 counties** (task-force terminations not yet applied) |
| 2013-08 | 36 | 39 | Maricopa AZ, Cabarrus NC | Pima AZ, Riverside CA, San Bernardino CA, Davidson TN, Rockingham VA |
| 2014-08 | 35 | 37 | Maricopa AZ, Cabarrus NC, Lexington SC | Pima AZ, Riverside CA, Davidson TN, Rockingham VA, Shenandoah VA |
| 2015-07 | 32 | 32 | Maricopa AZ, Cabarrus NC, Lexington SC | Pima AZ, Rockingham VA, Shenandoah VA |

Read this as: East is a good approximation, not a census. Two systematic problems —
**Lexington County SC is missing entirely until Aug 2013** despite signing 19 Aug 2010, and
**terminations lag**, badly around the Dec 2012 task-force shutdown (10 counties still coded
active that ICE had dropped). Maricopa's Dec 2012 termination is likewise late.

Caveat on the other direction: "in East, absent from ICE" is partly a mapping artifact —
I collapse municipal agencies into their county — and partly ICE's own sloppiness (its tables
contain duplicate and misspelled agency rows). Treat those as flags to check, not proven errors.

---

## 5. What I would actually claim in print

- **"Complete roster of active agreements"** — yes, for any of the ~24 dated snapshots in §2.
  Cite the source and the as-of date. This is safe.
- **"All agreements signed in 2007–2017"** — yes, from the stacked union. Defensible with the
  audit trail.
- **"All agreements signed 2002–2009"** — only with the survivor-bias caveat spelled out.
- **"Continuous county-month treatment 2005–2015"** — use East et al., but correct Lexington SC
  and re-date the 2012–13 terminations off the ICE captures first.
- **Anything sourced to Amuedo-Dorantes** — don't. Her papers publish no list, and her cited
  chain resolves to ICE + Kostandini anyway.

If you want the applied-but-rejected counties (a genuinely different question), Capellan &
Sorg FOIA'd ICE for **167 counties that applied 2005–2010** with accepted/rejected status
([NIJ 305488](https://www.ojp.gov/pdffiles1/nij/grants/305488.pdf)). Never deposited — email them.
