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
c++ -std=c++17 -O1 "$dir/generate_calendars.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/calendars.sexp"
c++ -std=c++17 -O1 "$dir/generate_schedules.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/schedules.sexp"
c++ -std=c++17 -O1 "$dir/generate_rates.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/rates.sexp"
c++ -std=c++17 -O1 "$dir/generate_legs.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/legs.sexp"
c++ -std=c++17 -O1 "$dir/generate_bootstrap.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/bootstrap.sexp"
c++ -std=c++17 -O1 "$dir/generate_bonds_swaps.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/bonds-swaps.sexp"
c++ -std=c++17 -O1 "$dir/generate_caps_swaptions.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/caps-swaptions.sexp"
c++ -std=c++17 -O1 "$dir/generate_hull_white.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/hull-white.sexp"
c++ -std=c++17 -O1 "$dir/generate_sobol.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/sobol.sexp"
c++ -std=c++17 -O1 "$dir/generate_sabr.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/sabr.sexp"
c++ -std=c++17 -O1 "$dir/generate_dividends.cpp" -lQuantLib -o "$bin"
"$bin" "$dir/dividends.sexp"
