#!/bin/bash
set -e

Rscript code/1-process-state-xwalk.R
Rscript code/1-process-agencies.R
Rscript code/1-process-agency-ori.R
Rscript code/1-process-hifld-tables.R
Rscript code/1-process-manual-inputs.R
Rscript code/1-process-facility-tables.R
Rscript code/1-process-university-boundaries.R
Rscript code/2-make-non-facility-sf.R
Rscript code/4a-make-pa-constable-sf.R
Rscript code/5-make-university-sf.R
Rscript code/6-make-facility-sf.R
Rscript code/7-make-non-facility-sf.R
Rscript code/8-format-agreements-dataset.R
Rscript code/9-make-review-qa.R
