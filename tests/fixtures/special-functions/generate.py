# Reference values for Bessel K and the noncentral chi-square CDF (34 digits),
# written to tests/fixtures/special-functions.sexp:
#
#   nix shell --impure --expr 'with import (builtins.getFlake "nixpkgs") {};
#     python3.withPackages (ps: [ps.mpmath])' \
#     -c python3 tests/fixtures/special-functions/generate.py \
#     > tests/fixtures/special-functions.sexp
import mpmath as mp
mp.mp.dps = 34
print(";;;; special-functions.sexp --- mpmath %s reference values (34 digits)" % mp.__version__)
print("(:bessel-k")
for nu in ["0", "0.25", "0.5", "1", "1.5", "2", "3.7", "10", "25.5"]:
    for x in ["0.001", "0.1", "0.5", "1.0", "1.999", "2.0", "2.5", "5", "20", "100", "600"]:
        v = mp.besselk(mp.mpf(nu), mp.mpf(x))
        print(" (%s %s %s)" % (nu, x, mp.nstr(v, 30, min_fixed=0, max_fixed=0)))
print(")")
def ncx2(x, f, lam):
    x, f, lam = mp.mpf(x), mp.mpf(f), mp.mpf(lam)
    if lam == 0:
        return (mp.gammainc(f/2, 0, x/2, regularized=True),
                mp.gammainc(f/2, x/2, mp.inf, regularized=True))
    h = lam/2
    # Direct sum over the Poisson weights; those left out are below 1e-34.
    top = int(h + 14 * mp.sqrt(h) + 60)
    p = q = mp.mpf(0)
    for k in range(top):
        w = mp.exp(-h + k * mp.log(h) - mp.loggamma(k + 1))
        p += w * mp.gammainc(f/2 + k, 0, x/2, regularized=True)
        q += w * mp.gammainc(f/2 + k, x/2, mp.inf, regularized=True)
    return p, q
print("(:noncentral-chi-square")
cases = []
for f in ["0.5", "2", "3.3", "50"]:
    for lam in ["0", "0.01", "7.5", "300"]:
        for x in ["0.01", "1", "20", "80", "400"]:
            p, q = ncx2(x, f, lam)
            print(" (%s %s %s %s %s)" % (x, f, lam, mp.nstr(p, 30, min_fixed=0, max_fixed=0),
                                          mp.nstr(q, 30, min_fixed=0, max_fixed=0)))
print(")")
