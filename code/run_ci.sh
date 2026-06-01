#!/bin/bash
set -e

# CI currently has the same committed inputs as the local pipeline. Keep this
# as a separate entrypoint so automation can diverge later without changing
# local usage.
bash code/run_all.sh
