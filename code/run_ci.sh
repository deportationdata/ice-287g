#!/bin/bash
set -e

# separate entrypoint so CI can diverge without changing local usage
export R_PROFILE_USER=/dev/null
export R_ENVIRON_USER=/dev/null
export RENV_CONFIG_AUTOLOADER_ENABLED=FALSE

bash code/run_all.sh
