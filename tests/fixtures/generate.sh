#!/bin/sh
# Regenerate the QuantLib fixtures. Run inside the fixtures shell:
#
#   nix develop .#fixtures -c sh tests/fixtures/generate.sh
set -eu
dir=$(dirname "$0")
bin=$(mktemp)
trap 'rm -f "$bin"' EXIT
c++ -std=c++17 -O1 "$dir/generate_day_counters.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/day-counters.sexp"
