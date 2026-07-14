#!/bin/bash
set -e

Rscript code/0-process-state-xwalk.R
Rscript code/1-process-agencies.R
Rscript code/1-process-agency-ori.R
Rscript code/1-process-hifld-tables.R
Rscript code/1-process-manual-inputs.R
Rscript code/1-process-facility-tables.R
Rscript code/2-make-non-facility-sf-by-type.R
Rscript code/3-make-pa-constable-sf.R
Rscript code/5-make-university-sf.R
Rscript code/6-make-facility-sf.R
Rscript code/7-make-non-facility-sf.R
Rscript code/8-format-agreements-dataset.R
Rscript code/9-make-review-qa.R
