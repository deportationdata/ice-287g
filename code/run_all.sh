#!/bin/bash
set -e

# optional: script to start from, e.g. `bash code/run_all.sh 3-match-state.R`
start="${1:-}"
run() { [[ -n "$start" && "$start" != "$1" ]] && return 0; start=""; Rscript "code/$1"; }

# read sources into normalized parquets under data/
run 1-read-reference-state-codes.R
run 1-read-reference-counties.R
run 1-read-agency-roster-leaic.R
run 1-read-agency-roster-lear.R
run 1-read-agency-roster-cde.R
run 1-read-agency-roster-hifld.R
run 1-read-facility-list-jails-prisons.R
run 1-read-manual-inputs.R
run 1-read-facility-list-ice-detention.R
run 1-read-reference-university-campuses.R

# every archived ICE sheet -> publications -> identities -> the agreements dataset
run 1-read-sheets.R
run 2-make-identities.R
run 2-make-agreements.R

# ICE's other records (undated lists, MOA archive index, press releases) and the
# OIG roster, resolved to partnerships and arbitrated against ICE's own record
run 1-read-historical-ice-lists.R
run 1-read-historical-oig-2009.R
run 1-read-historical-ice-archive-index.R
run 2-make-partnerships.R

# one script per geometry class; these read only 1-read outputs, so any order
run 3-match-state.R
run 3-match-county.R
run 3-match-municipal.R
run 3-match-pa-constable.R
run 3-match-university.R
run 3-match-facility.R

# bind the non-facility layers, annotate with roster identifiers, format
run 4-match-non-facility.R
run 5-match-agency-identifiers.R
run 6-make-agreement-level-sf.R
run 7-match-missing-identifiers.R
run 7-make-qa-report.R
