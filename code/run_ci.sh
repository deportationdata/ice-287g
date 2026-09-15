#!/bin/bash
set -e

# separate entrypoint so CI can diverge without changing local usage; .Rprofile
# still runs, so renv and the allocator setting apply as they do locally
export R_ENVIRON_USER=/dev/null

bash code/run_all.sh
