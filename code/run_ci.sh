#!/bin/bash
set -e

# CI currently has the same committed inputs as the local pipeline. Keep this
# as a separate entrypoint so automation can diverge later without changing
# local usage.
export R_PROFILE_USER=/dev/null
export R_ENVIRON_USER=/dev/null
export RENV_CONFIG_AUTOLOADER_ENABLED=FALSE

bash code/run_all.sh
