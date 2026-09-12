#!/bin/bash
set -e

# fact-sheet roster captures -> sheets/ snapshots; idempotent
Rscript code/0-287g-pre2018-rosters.R

# read sources into normalized parquets under data/
Rscript code/1-read-state-xwalk.R
Rscript code/1-read-leaic.R
Rscript code/1-read-lear.R
Rscript code/1-read-crime.R
Rscript code/1-read-agreement-history.R
Rscript code/1-read-agreements.R
Rscript code/1-read-hifld-law-enforcement.R
Rscript code/1-read-jails-prisons.R
Rscript code/1-read-manual-inputs.R
Rscript code/1-read-facilities.R
Rscript code/1-read-university-boundaries.R

# pre-2018 reconstruction; independent of the modern chain
Rscript code/1-read-pre2018-factsheets.R
Rscript code/1-read-pre2018-kostandini.R
Rscript code/1-read-pre2018-yale-foia.R
Rscript code/1-read-pre2018-east-panel.R
Rscript code/1-read-pre2018-secondary-rosters.R
# must follow the fact-sheet reader, whose output it uses to resolve states
Rscript code/1-read-pre2018-ice-removals.R
Rscript code/2-make-pre2018-best-guess.R

# one script per geometry class; these read only 1-read outputs, so any order
Rscript code/2-make-state-sf.R
Rscript code/2-make-county-sf.R
Rscript code/2-make-municipal-sf.R
Rscript code/2-make-pa-constable-sf.R
Rscript code/2-make-university-sf.R
Rscript code/2-make-facility-sf.R

# bind the non-facility layers, annotate with roster identifiers, format
Rscript code/3-make-non-facility-sf.R
Rscript code/4-match-rosters.R
Rscript code/5-format-agreements-dataset.R
Rscript code/6-make-missing-identifiers.R
