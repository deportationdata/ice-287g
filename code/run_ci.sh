#!/bin/bash
set -e

# separate entrypoint so CI can diverge; .Rprofile still runs, so renv and the allocator setting apply
export R_ENVIRON_USER=/dev/null

bash code/run_all.sh
