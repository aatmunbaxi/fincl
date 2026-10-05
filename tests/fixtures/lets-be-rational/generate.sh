#!/bin/sh
# Regenerate tests/fixtures/lets-be-rational.sexp from Peter Jäckel's
# reference C++ (the 2013-2014 LetsBeRational sources, mirrored by vollib).
# Run from the repository root inside the dev shell:
#
#   nix develop -c sh tests/fixtures/lets-be-rational/generate.sh
set -eu
here=tests/fixtures/lets-be-rational
work=$(mktemp -d)
mirror=https://raw.githubusercontent.com/vollib/lets_be_rational/master/src
for f in LetsBeRational.cpp erf_cody.cpp normaldistribution.cpp normaldistribution.h \
         rationalcubic.cpp rationalcubic.h importexport.h; do
  curl -sSfL -o "$work/$f" "$mirror/$f"
done
cp "$here/harness.cpp" "$work/"
g++ -O2 -ffp-contract=off -w -fpermissive "$work/harness.cpp" "$work/LetsBeRational.cpp" \
    "$work/erf_cody.cpp" "$work/normaldistribution.cpp" "$work/rationalcubic.cpp" -o "$work/lbr-ref"
python3 "$here/vectors.py" "$work/lbr-ref" > tests/fixtures/lets-be-rational.sexp
rm -r "$work"
