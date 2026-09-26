# Getting the ORI list: what to request, from whom, and how to know it is complete

Drafted 2026-09-26. Companion to `inputs/manual-agency-ori.csv`, whose blank rows
this is meant to close. Nothing here has been filed.

## 1. Where the ORI list lives (the PIAs, SORNs and manuals)

**The master copy is one file inside the NCIC: the Originating Agency Identifier
(ORI) File, created and kept by the FBI's Criminal Justice Information Services
(CJIS) Division.**

- FBI, *Privacy Impact Assessment, FBI/NCIC* (approved Nov 7, 2022; posted Feb
  2023; fbi.gov/file-repository/pias/pia-ncic-020723.pdf), section 3, lists it
  among NCIC's files: "An ORI is a nine-character identifier assigned to an
  agency to identify the agency in transactions on CJIS systems. Agencies must
  have an ORI to access the NCIC. The ORI File contains contact information
  (such as an agency's address and telephone number) for agencies and their
  associated ORIs." Footnote: "The FBI creates all records in the ORI file;
  however, both the FBI and the agency assigned the ORI can update the agency
  information in the ORI file." Also: "Records in the ORI File can be retrieved
  by full or partial ORI number"; "Any agency with an assigned ORI may query
  the ORI file"; Nlets "has query access to the ORI file". Retention follows
  NARA schedule N1-065-11-3.
- SORN *National Crime Information Center (NCIC), JUSTICE/FBI-001*, 64 FR 52343
  (Sept 28, 1999), modified at 84 FR 47533 (Sept 10, 2019). Its categories of
  records (files A through AA) are all records about people or property; the
  ORI File is not among them. ORIs appear only under safeguards: "Each agency
  is assigned an originating agency identifier (ORI) to access the NCIC."
  So the ORI File is an agency directory outside the Privacy Act's reach, which
  matters for exemptions (section 3). System location includes "FBI Criminal
  Justice Information Services (CJIS) Division, 1000 Custer Hollow Road,
  Clarksburg, WV 26306". Requests go to "FBI, ATTN: FOI/PA Request,
  Record/Information Dissemination Section, 170 Marcel Drive, Winchester, VA
  22602-4843; facsimile: 540-868-4995/6/7".
- Modification at 91 FR 27082 (May 13, 2026; FR Doc. 2026-09522): NCIC moved to
  "a FedRAMP approved cloud environment". System location is now "secure cloud
  computing environments. The cloud computing service provider on the date of
  this publication is Amazon Web Services, located at 12900 Worldgate Drive,
  Herndon, VA 20170", with the CJIS Division still listed. The custodian is
  unchanged; only the hardware moved.
- *NCIC Operating Manual*, chapter "Originating Agency Identifier File" (public
  copies posted by state CSAs, e.g.
  site.utah.gov/dps-tac/wp-content/uploads/sites/38/2017/10/ORI.pdf):
  - Section 2: "All ORI record entries are made by the FBI CJIS staff."
    Section 1.2: requests come only from a state or federal CJIS Systems
    Agency (CSA) to the "NCIC Operations and Policy Unit, Module D3, 1000
    Custer Hollow Road, Clarksburg, WV 26306, Attention: Systems Access"
    (ORI@leo.gov; fax 304 625-2924).
  - Section 1.5: law-enforcement ORIs end in 00; other criminal-justice ORIs
    end in a level digit (1 local, 3 county, 5 state, 7 federal, 9 private)
    and a type letter: A prosecutors and attorneys general, B pretrial, C
    "jails, prisons and detention centers", D civil courts, E private railroad
    and campus police, F child-protection social services, G probation and
    parole, J criminal courts, K coroners, N dispatch centers, P
    nongovernmental police, Z fingerprint-only. Our blanks are almost all
    types A, C and G, or constables, which no public roster carries.
  - Section 1.7: "ORIs are validated on a biennial basis." Every ORI record
    is sent to its CSA as a `$.C.` administrative message, "with all $.C.
    administrative messages for a CSA grouped together in a file", and the
    manual speaks of "fixed format validation files". So a per-state
    extract of the whole file already exists as a discrete record every two
    years.
  - Section 1.8, the fields of an ORI record: ORI; AN1, AN2, AN3 (agency
    name); ATR (agency translation, name and city); COU (county); CTY; STA;
    ZIP; SNU, SNA (street); CRY, FPP (foreign); TNO, CT1, CT2, CT3
    (telephones); EML; FOC (FBI field office); TYP (1 state, 2 county,
    3 local, 4 federal, 5 = ORIs ending D, H, I, K, N, O, P, Q, R, U, V or
    W, 6 criminal justice, 7 foreign local, 8 federal noncriminal justice,
    9 foreign state, R retired, S state CSA, and a few others); DTE (date
    entered); DLU (date of last update); VLD (date of last validation); VLN
    (name of the validating person); NLC, TUC, OMC, CDC (publication counts).

**Copies of the file, and who holds them.**

- Every state CSA receives its own state's validation files and can query the
  whole file. For Pennsylvania the CSA is the State Police's CLEAN
  Administrative Section, "Pennsylvania State Police Headquarters 1800 Elmerton
  Ave, Harrisburg Pa. 17110" (CLEAN Administrative Regulations, Feb 2011,
  section H and Appendix D). Texas DPS publishes its slice outright
  (dps.texas.gov/administration/crime_records/docs/cjis/arrestingagencyoris.xls
  and courtoris.xls); OSBI publishes per-type lists; FDLE's UCR agency list
  carries ORIs for reporting agencies.
- Nlets keeps a parallel directory, ORION, with the same FBI ORIs plus the
  state-created sub-ORIs (section 1.5: variants of positions 8 and 9 that a
  CSA enters "in the Nlets system" and the FBI never sees). Nlets is a
  nongovernmental nonprofit, so no FOIA reaches it; members recertify their
  ORION entries every two years. NCJRS digitized the paper predecessor, the
  *NLETS ORI Directory* (NCJ 75873, 726 pp., ojp.gov/pdffiles1/Digitization/75873NCJRS.pdf),
  which this repo OCR'd on 2026-08-16 (commit 8cc2a43, files since removed);
  it is 1980 vintage and garbled, not a substitute.
- BJS/NACJD hold an FBI extract: LEAIC 2012 (ICPSR 35158) flags 32,444 of its
  36,490 rows as `SOURCE_NCIC2012`, and 11,862 rows exist only because of that
  extract, but every ORI in it ends in 00. The CJIS Division released the
  law-enforcement portion of the ORI File for public research use in 2012; the
  criminal-justice types (A, C, G, J) were left out, which is exactly the gap.

**Where it is not.**

- ICE's 287(g) Program Database (DHS/ICE/PIA-014, 2009) stores officer
  candidates, LEA points of contact, agreements and EID arrest statistics. The
  PIA text has no ORI field (an automated summary said otherwise; the PDF does
  not). A FOIA to ICE would not yield ORIs.
- ICE's ACRIMe/LESC (DHS/ICE/PIA-020, Sept 2018 update) receives Immigration
  Alien Queries from agencies via Nlets but describes no agency directory and
  never mentions the ORI.
- NGI SORN JUSTICE/FBI-009, 84 FR 54182 (Oct 9, 2019): no agency table; the
  ORI is an access key, not a stored directory.
- NARA schedule N1-065-05-003 (superseding N1-065-04-2) describes NCIC
  ("Data is currently stored in 19 files"; "An Originating Agency Identifier is
  assigned to each agency authorized to access the system") but schedules no
  ORI file separately.
- MuckRock shows no prior request for the ORI File (searches for
  "originating agency identifier" and "NCIC ORI" return nothing), so there is
  no precedent response to point to, for or against.

## 2. Whom to ask

| Route | What it yields | Speed | Notes |
|---|---|---|---|
| FBI FOIA (RIDS, Winchester VA; eFOIA portal) | the whole file: every state, territory (GU, MP), type and retired ORIs | slow: the FBI ended FY2025 with roughly 6,300 backlogged requests and says it now favors its small-track queues (DOJ 2026 Chief FOIA Officer Report) | the only custodian of the complete file; frame it as one existing electronic file so it lands in the small track |
| State public-records requests to the CSAs | that state's slice, same FBI-assigned ORIs, plus any Nlets-only sub-ORIs | weeks: most statutes set 3 to 10 business-day response deadlines | seven states hold 85 of the 126 blank agreements; a CSA can also say "this agency has never held an ORI", which the FBI file only implies |
| Nlets ORION | same as FBI plus sub-ORIs | n/a | private nonprofit; not requestable |
| BJS/NACJD | 2012 LE-only extract | done | already in the pipeline as LEAIC |
| ICE | nothing | n/a | its 287(g) database carries no ORI |

Recommendation: file the FBI request now, and the same day file state requests
with the seven CSAs below (Pennsylvania, Florida, Tennessee, Arkansas,
Louisiana, Mississippi, Virginia). The state responses close two-thirds of the
gap within weeks and later serve as an independent check on the FBI release;
the FBI release closes the singletons (Alaska, Delaware, Guam, CNMI, and so on)
and lets us audit the 2,886 ORIs already shipped.

State CSAs, ordered by blank agreements (deadlines from general knowledge;
confirm each statute and the records officer before filing):

| State | Blank agreements | CSA | Records statute, response deadline |
|---|---|---|---|
| Pennsylvania | 26 | Pennsylvania State Police, CLEAN Administrative Section (CSO), 1800 Elmerton Ave, Harrisburg PA 17110; file with PSP's Agency Open Records Officer | Right-to-Know Law, 65 P.S. § 67.101 et seq.; 5 business days |
| Florida | 14 | Florida Department of Law Enforcement, CJIS, Tallahassee | Fla. Stat. ch. 119; no fixed deadline, prompt; FDLE already publishes UCR ORIs |
| Tennessee | 12 | Tennessee Bureau of Investigation, CJIS Support Center, Nashville | Tenn. Code § 10-7-503; 7 business days; the TBI confidentiality rule (§ 10-7-504) covers investigative records, not a directory |
| Arkansas | 11 | Arkansas Crime Information Center (ACIC), Little Rock | Ark. Code § 25-19-105; 3 business days |
| Louisiana | 8 | Louisiana State Police, Bureau of Criminal Identification and Information | La. R.S. 44:31 et seq. |
| Mississippi | 7 | Mississippi Department of Public Safety, Mississippi Justice Information Center (MJIC) | Miss. Code § 25-61-5; 7 working days |
| Virginia | 7 | Virginia State Police, CJIS Division (VCIN) | Va. Code § 2.2-3704; 5 working days |
| Kentucky | 5 | Kentucky State Police | KRS 61.872; 5 business days |
| Massachusetts | 5 | Department of Criminal Justice Information Services (DCJIS) | G.L. c. 66 § 10; 10 business days |
| Indiana | 4 | Indiana State Police (IDACS) | Ind. Code 5-14-3 |
| Georgia | 3 | GBI, Georgia Crime Information Center (GCIC) | O.C.G.A. § 50-18-71; 3 business days |
| New Jersey | 3 | New Jersey State Police | OPRA; 7 business days |
| one or two each | 22 | AL (ALEA), AK (DPS), AZ (DPS), CNMI (DPS), DE (DELJIS), GU (Guam Police Dept), ID (ISP), MD (DPSCS), MO (MSHP), MT (DOJ), NE (NSP), NV (DPS), NM (DPS), NC (SBI, DCIN), OK (OSBI), SD (DCI), WV (WVSP) | leave to the FBI release |

## 3. Draft FBI FOIA request

Submit through the FBI eFOIA portal (efoia.fbi.gov) or to FBI, ATTN: FOI/PA
Request, Record/Information Dissemination Section, 170 Marcel Drive,
Winchester, VA 22602-4843 (fax 540-868-4995).

---

Subject: FOIA request: electronic copy of the NCIC Originating Agency Identifier (ORI) File

Dear FOIA Officer,

Under the Freedom of Information Act, 5 U.S.C. § 552, I request the following
records of the FBI Criminal Justice Information Services (CJIS) Division.

**Records requested**

1. A complete electronic copy of the National Crime Information Center (NCIC)
   Originating Agency Identifier (ORI) File as of the date of your search: every
   ORI record, active and retired (TYP value R), for every state, the District
   of Columbia, Puerto Rico, Guam, the Northern Mariana Islands, the U.S. Virgin
   Islands, American Samoa, and federal agencies, of every type (law-enforcement
   ORIs ending in 00 and criminal-justice ORIs ending in a level digit and type
   letter). For each record I request the data elements defined in section 1.8
   of the "Originating Agency Identifier File" chapter of the NCIC Operating
   Manual: ORI; Agency Name (AN1, AN2, AN3); Agency Translation (ATR); County
   (COU); City (CTY); State (STA); ZIP; Street Number (SNU); Street Name (SNA);
   Country (CRY) and Foreign Postal Code (FPP) where present; Telephone Number
   (TNO) and Confirmation Telephone Numbers (CT1, CT2, CT3); E-mail Address
   (EML); FBI Field Office Code (FOC); Type (TYP); Date Entered (DTE); Date of
   Last Update (DLU); and Date of Last Validation (VLD). I do not request the
   Name of Validator (VLN) field, which names an individual, and I do not
   request any transaction-log data.

2. If the CJIS Division does not maintain the file in a form from which item 1
   can be extracted, then the most recent ORI validation file generated for
   each CJIS Systems Agency under section 1.7 of that chapter (the file of $.C.
   administrative messages, one per ORI record), which together contain the
   same records.

3. The record layout or data dictionary accompanying the extract, and the
   current version of the "Originating Agency Identifier File" chapter of the
   NCIC Operating Manual (or the Technical and Operational Update that revised
   it), so that the TYP codes and the characters in positions 8 and 9 can be
   read correctly.

4. Severable, and to be processed after items 1 to 3 rather than delaying
   them: if the CJIS Division retains earlier versions of the ORI File or
   earlier validation cycles, one retained version from before 2016, so that
   agencies whose ORIs were retired since can be identified.

**Where the records are**

The ORI File is described in the FBI's Privacy Impact Assessment for the NCIC
(approved November 7, 2022), section 3 ("Originating Agency Identifier (ORI)
File"), which states that "the FBI creates all records in the ORI file" and
that "records in the ORI File can be retrieved by full or partial ORI number".
Under the NCIC Operating Manual, ORI records are entered only by the CJIS
Division's NCIC Operations and Policy Unit, Module D3, 1000 Custer Hollow Road,
Clarksburg, WV 26306. The NCIC is covered by system of records notice
JUSTICE/FBI-001, 64 FR 52343, as modified at 84 FR 47533 (Sept. 10, 2019) and
91 FR 27082 (May 13, 2026).

**Format**

Under 5 U.S.C. § 552(a)(3)(B), please provide the extract as delimited text
(CSV) or a spreadsheet, delivered through the eFOIA portal or by e-mail, rather
than as page images. The file is one existing electronic record; I ask that the
request be placed in the simple or small processing track.

**Why the records are releasable in full**

- The ORI File is a directory of agencies, not of people. Its fields are agency
  names, business addresses, business telephone numbers and dates. The NCIC
  SORN's categories of records (files A through AA at 84 FR 47533) do not
  include it, because it holds no record about an individual. Exemptions 6 and
  7(C) therefore do not apply, and I have excluded the one field (VLN) that
  names a person.
- It is not criminal history record information under 28 C.F.R. § 20.3, and it
  is not "criminal justice information" as the CJIS Security Policy defines the
  term (biometric, identity history, biographic, property, and case or incident
  history data).
- The identifiers and their structure are already public. The FBI publishes the
  ORIs of every Uniform Crime Reporting agency through the Crime Data Explorer;
  the NCIC Operating Manual's ORI File chapter, with the type codes, is posted
  by state CJIS Systems Agencies (for example the Utah Department of Public
  Safety and the Illinois State Police); the Texas Department of Public Safety
  publishes its complete list of arresting-agency and court ORIs; the Oklahoma
  State Bureau of Investigation publishes ORI lists by agency type; the
  Department of Justice's own NCJRS digitized the 726-page NLETS ORI Directory
  (NCJ 75873); and the Bureau of Justice Statistics built the Law Enforcement
  Agency Identifiers Crosswalk (ICPSR 35158) on a 2012 extract of this same
  file supplied by the CJIS Division, which is distributed to the public.
- Exemption 7(E) does not reach a published identifier scheme. If the Bureau
  nonetheless regards a particular field (for example TYP, insofar as it
  reflects access level) as sensitive, please release the remaining fields, as
  5 U.S.C. § 552(b) requires reasonably segregable portions to be disclosed.

**Fee category and fee waiver**

I am a university researcher [affiliation] working with the Deportation Data
Project, which obtains government data and publishes it free of charge. I ask
to be treated as an educational institution requester (or, alternatively, a
representative of the news media) under 28 C.F.R. § 16.10, and I request a fee
waiver under 5 U.S.C. § 552(a)(4)(A)(iii). The ORI is the key that links the
roughly 3,000 agreements under which ICE has delegated federal immigration
authority to state and local agencies (the 287(g) program, whose agreement list
ICE publishes) to the FBI's own crime-reporting data and to BJS's censuses of
law-enforcement agencies. Disclosure will therefore contribute significantly to
public understanding of the operations of government: which agencies hold
delegated federal authority and how their activity relates to the crime data
those same agencies report. The use is noncommercial and the results will be
published openly. If the waiver is denied, please notify me before incurring
fees above $100.

Please acknowledge this request with a tracking number. I prefer electronic
correspondence.

Sincerely,

Graeme Blair
[affiliation, postal address]
graeme.blair@proton.me

---

## 4. Draft state public-records request (one per CSA)

---

Subject: Public records request under [statute]: [State]'s NCIC ORI records

Dear Records Officer,

Under [statute], I request the following records held by [CSA] as [State]'s
CJIS Systems Agency.

1. The most recent NCIC ORI validation file [CSA] received from the FBI CJIS
   Division for [State] agencies (the file of $.C. administrative messages
   described in section 1.7 of the "Originating Agency Identifier File" chapter
   of the NCIC Operating Manual), or, equivalently, [CSA]'s current list of every
   FBI-assigned ORI for agencies located in [State], of every type
   (law-enforcement ORIs ending in 00 and criminal-justice ORIs ending in a
   letter: prosecutors, corrections and jails, courts, probation and others),
   including retired ORIs, with each record's agency name, address, county,
   type (TYP) and dates entered, updated and validated. I do not request the
   Name of Validator field.

2. Any state-assigned ORI suffixes or sub-ORIs [CSA] has entered in Nlets
   ORION for [State] agencies (the CSA-created variants described in section
   1.5 of that chapter), with their agency names.

3. For convenience only: the agencies listed below are of particular interest.
   If any of them has never been assigned an ORI, a note to that effect would
   be welcome, though it is not part of the request.

Please provide the records electronically (CSV or spreadsheet). The records
contain no criminal history record information and no personal information;
they are the agency directory that the Texas Department of Public Safety
already publishes for Texas (dps.texas.gov/administration/crime_records/docs/cjis/arrestingagencyoris.xls)
and that the FBI publishes for reporting agencies through the Crime Data
Explorer. The request is for noncommercial research whose results will be
published free of charge; I ask that any fees be waived, and that you contact
me before incurring fees above $50.

[agency list for this state, from section 6]

Sincerely,
Graeme Blair
[affiliation]
graeme.blair@proton.me

---

## 5. Knowing the release is complete, and using it

The request asks for the whole file rather than lookups of named agencies for
three reasons: an absence then proves the agency has no ORI (a verified blank
to record in the csv note, not a "not found"); the 2,886 ORIs already shipped
can be audited against the FBI's own record; and agencies renamed or merged
since 2002 turn up under their retired ORIs.

Checks when a release arrives:

- Row count and extract date stated in the response; state counts against
  LEAIC 2012 (NCIC-flagged rows per state) and against the Texas DPS list,
  which must be a subset.
- Every ORI in LEAIC with `SOURCE_NCIC2012 = 1` appears, active or retired
  (TYP R). Missing ones mean an incomplete extract: appeal.
- Every ORI in `data/agreements.parquet` appears; any that do not are stale
  and need a manual row.
- Type letters present: A, C, G and J rows exist for every state (they are
  what LEAIC lacks). Territories GU and MP present.
- Field check: DTE and DLU populated; TYP present (if withheld, note it and
  decode type from position 9 instead).
- For each state release, the same checks on its slice, plus a comparison
  with the FBI release once both exist; disagreements go in the csv note.

Pipeline: add the file under `inputs/` with its extract date, a
`1-read-agency-roster-ncic.R` reader, and a `ncic` roster in
`5-match-agency-identifiers.R` ranked above LEAIC (newer). Corrections
departments, prosecutors and probation offices match on type letter plus
county, which the existing key logic does not yet do. Blank rows in
`inputs/manual-agency-ori.csv` whose agency the release lacks get a note
"no ORI in the NCIC ORI File extract of YYYY-MM-DD" rather than deletion.

## 6. The 126 agreements (111 agencies) with no ORI, by state

Counts are agreements; agencies with several agreements appear once.


### Alabama (1 agreements)
- State Constable Office, Talladega County (Talladega County) — County

### Alaska (1 agreements)
- Alaska Department of Corrections — State

### Arizona (1 agreements)
- Pinal County Attorney's Office (Pinal County) — County

### Arkansas (11 agreements)
- 16th Judicial Distric Drug Task Force (Cleburne County; Fulton County; Independence County; Izard County; Stone County) — Judicial District
- 6th Judicial District Prosecuting Attorney's Office (Pulaski County; Perry County) — Judicial District
- 8th Judicial District South (Lafayette County; Miller County) — Judicial District
- 9th West Judicial District (Howard County; Little River County; Pike County; Sevier County) — Judicial District
- Taylor Police Department (Columbia County) — Municipal
- Arkansas Department of Corrections — State
- Arkansas Department of Public Safety — State
- Arkansas Department of Public Safety-Crime Information Center — State
- Arkansas Division of Law Enforcement Standards and Training — State
- Arkansas National Guard — State

### Commonwealth of the Northern Mariana Islands (2 agreements)
- CNMI Department of Corrections — State
- CNMI Department of Public Safety — State

### Delaware (1 agreements)
- Delaware Department of Corrections — State

### Florida (14 agreements)
- Escambia County Board of County Commissioners/ Department of Corrections (Escambia County) — County
- Gulf County Board of County Commissioners /Detention Facility (Gulf County) — County
- Jackson County Correctional Facility (Jackson County) — County
- Miami-Dade Corrections and Rehabilitation (Miami-Dade County) — County
- Okaloosa County Board of County Commissioners/ Department of Corrections (Okaloosa County) — County
- Orange County Corrections Department (Orange County) — County
- Osceola County Corrections Department (Osceola County) — County
- Pasco County Board of County Commissioners/ Pasco County Corrections (Pasco County) — County
- Volusia County Division of Corrections (Volusia County) — County
- Florida Department of Corrections — State
- Florida Department of Law Enforcement — State
- Florida National Guard — State
- Florida State Guard — State

### Georgia (3 agreements)
- Northern Judicial Circuit Probation Office (Elbert County; Franklin County; Hart County; Madison County; Oglethorpe County) — Judicial District
- Georgia Department of Corrections — State

### Guam (1 agreements)
- Office Of The Attorney General — State

### Idaho (1 agreements)
- Idaho Department of Correction — State

### Indiana (4 agreements)
- Hendricks County Prosecutor's Office (Hendricks County) — County
- Greensboro Police Department (Henry County) — Municipal
- Indiana Department of Homeland Security — State
- Indiana Office of Attorney General — State

### Kentucky (5 agreements)
- Bullitt County Detention Center (Bullitt County) — County
- Grayson County Detention Center (Grayson County) — County
- Oldham County Detention Center (Oldham County) — County

### Louisiana (8 agreements)
- Denham Springs City Marshal (Livingston Parish) — Municipal
- Jackson Marshals Office (East Feliciana Parish) — Municipal
- Ward 5 Marshal's Office (Allen Parish) — Municipal
- Ward 6 Marshal's Office (St. Mary Parish) — Municipal
- Louisiana Department of Justice - Office of the Attorney General — State
- Louisiana Department of Public Safety & Corrections — State
- Louisiana Military Department — State

### Maryland (1 agreements)
- Wicomico County Corrections Center (Wicomico County) — County

### Massachusetts (5 agreements)
- Massachusetts Department of Correction — State
- Massachusetts Department of Corrections — State

### Mississippi (7 agreements)
- Prentiss County Constable Northern District (Prentiss County) — Constable District
- Hickory Flat Police Department (Benton County) — Municipal
- Mississippi Attorney General's Office — State
- Mississippi Department of Corrections — State
- Mississippi Office of the State Auditor — State

### Missouri (2 agreements)
- COMET Drug Task Force (Greene County) — Regional
- Missouri Department of Corrections — State

### Montana (1 agreements)
- Montana Department of Corrections — State

### Nebraska (1 agreements)
- Nebraska Department of Correctional Services — State

### Nevada (1 agreements)
- Nevada Department of Corrections — State

### New Jersey (3 agreements)
- Hudson County Department of Corrections (Hudson County) — County

### New Mexico (1 agreements)
- New Mexico Department of Corrections — State

### North Carolina (1 agreements)
- Albemarle District Jail (Camden County; Pasquotank County; Perquimans County) — Regional

### Oklahoma (1 agreements)
- Caney Valley Public Schools Police Department (Washington County) — Campus

### Pennsylvania (26 agreements)
- Cambria County Prison (Cambria County) — County
- County of Franklin / Franklin County Jail (Franklin County) — County
- Pennsylvania State Constables Office Cambria County (Cambria County) — County
- Addison Borough Constable's Office (Somerset County) — Municipal
- Coolspring Township Constable's Office (Mercer County) — Municipal
- Damascus Township Constable (Wayne County) — Municipal
- Derry Township Constable's Office (Dauphin County) — Municipal
- East Donegal Township Constables Office (Lancaster County) — Municipal
- East Penn Township Constable Office (Carbon County) — Municipal
- Fannett Township Constables Office (Franklin County) — Municipal
- Gaines Township PA State Constable (Tioga County) — Municipal
- Greene Township Constable's Office (Erie County) — Municipal
- Lansdowne Borough Constable's Office (Delaware County) — Municipal
- Lewistown Borough Constable's Office (Mifflin County) — Municipal
- Lower Burrell Fourth Ward Constable (Westmoreland County) — Municipal
- Office of Constable Monroeville 6th Ward (Allegheny County) — Municipal
- Pennsylvania State Constable Office Honey Brook Precinct 1 (Chester County) — Municipal
- Pennsylvania State Constable's Office So Middleton Twp (Cumberland County) — Municipal
- Pennsylvania State Constable's Office, Cooke Township (Cumberland County) — Municipal
- Preston Township Constable's Office (Wayne County) — Municipal
- Sewickley Township Constable's Office (Westmoreland County) — Municipal
- Shohola Township Constable's Office (Pike County) — Municipal
- South Pymatuning TWP State Constable (Mercer County) — Municipal
- Southampton Twp. Cumberland City, Constables Office (Cumberland County) — Municipal
- Troy Township PA State Constable (Bradford County) — Municipal
- Unity Township Constable Office (Westmoreland County) — Municipal

### South Dakota (2 agreements)
- South Dakota Department of Corrections — State

### Tennessee (12 agreements)
- Bradley County Constable District 1 (Bradley County) — County
- Bradley County Constable District 2 (Bradley County) — County
- Bradley County Constable District 5 (Bradley County) — County
- Bradley County Constable District 6 (Bradley County) — County
- Bradley County Constable District 7 (Bradley County) — County
- Grundy County Constable District 3 (Grundy County) — County
- Monroe County Constable District 2 (Monroe County) — County
- Polk County Constable District 2 (Polk County) — County
- Polk County Constable District 4 (Polk County) — County
- Sevier County Constable District 5 (Sevier County) — County
- Sullivan County Constable District 1 (Sullivan County) — County
- 23rd Judicial District Attorney's Office (Cheatham County; Dickson County; Houston County; Humphreys County; Stewart County) — Judicial District

### Virginia (7 agreements)
- Prince William-Manassas Regional Adult Detention Center (Prince William County; Manassas City) — Regional
- Prince William-Manassas Regional Jail (Prince William County; Manassas City) — Regional
- Rappahannock, Shenandoah, Warren Regional Jail Authority (Rappahannock County; Shenandoah County; Warren County) — Regional
- Southwest Virginia Regional Jail Authority (Buchanan County; Dickenson County; Lee County; Russell County; Scott County; Smyth County; Tazewell County; Washington County; Wise County; Norton City) — Regional
- Virginia DOC Law Enforcement Services — State
- Virginia Department of Corrections — State

### West Virginia (2 agreements)
- West Virginia Division of Corrections and Rehabilitation — State
- West Virginia National Guard — State
