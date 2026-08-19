#!/bin/bash
set -e

# read sources into normalized parquets under data/
Rscript code/1-read-state-xwalk.R
Rscript code/1-read-leaic.R
Rscript code/1-read-lear.R
Rscript code/1-read-crime.R
Rscript code/1-read-agreements.R
Rscript code/1-read-hifld-law-enforcement.R
Rscript code/1-read-jails-prisons.R
Rscript code/1-read-manual-inputs.R
Rscript code/1-read-facilities.R
Rscript code/1-read-university-boundaries.R

# match agreements to geometry, one script per geometry class. these read only
# the 1-read outputs and never each other, so they can run in any order
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
