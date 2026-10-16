// Generate tests/fixtures/rates.sexp from QuantLib's InterestRate and the
// interpolated yield curves.
//
//   nix develop .#fixtures -c sh tests/fixtures/generate.sh
//
// Two lists:
//   (:interest-rates case...)  each case a plist: the rate and its
//       conventions, two dates, then :compound and :discount between them,
//       :compound-time over :time, and the :implied and :equivalent rates
//       under the second set of conventions.
//   (:curves curve...)  each curve a plist: :kind, :day-counter, :dates,
//       :values (and :compounding/:frequency for zero curves), then :queries,
//       a list of (:discount "D" value), (:zero "D" dc comp freq value) and
//       (:forward "D1" "D2" dc comp freq value).

#include <ql/quantlib.hpp>
#include <ql/version.hpp>

#include <cstdio>
#include <random>
#include <string>
#include <vector>

using namespace QuantLib;

namespace {

std::string iso(const Date& d) {
    char buffer[16];
    std::snprintf(buffer, sizeof buffer, "%04d-%02d-%02d", d.year(), int(d.month()),
                  d.dayOfMonth());
    return buffer;
}

struct NamedDC { const char* name; DayCounter dc; };
struct NamedComp { const char* name; Compounding c; };
struct NamedFreq { const char* name; Frequency f; };

const std::vector<NamedDC>& dayCounters() {
    static std::vector<NamedDC> v = {
        {":actual-360", Actual360()},
        {":actual-365-fixed", Actual365Fixed()},
        {":thirty-360-bond-basis", Thirty360(Thirty360::BondBasis)},
        {":thirty-360-european", Thirty360(Thirty360::European)},
        {":actual-actual-isda", ActualActual(ActualActual::ISDA)}};
    return v;
}

const std::vector<NamedComp>& compoundings() {
    static std::vector<NamedComp> v = {
        {":simple", Simple}, {":compounded", Compounded}, {":continuous", Continuous},
        {":simple-then-compounded", SimpleThenCompounded},
        {":compounded-then-simple", CompoundedThenSimple}};
    return v;
}

const std::vector<NamedFreq>& frequencies() {
    static std::vector<NamedFreq> v = {
        {":annual", Annual}, {":semiannual", Semiannual},
        {":every-fourth-month", EveryFourthMonth}, {":quarterly", Quarterly},
        {":bimonthly", Bimonthly}, {":monthly", Monthly},
        {":every-fourth-week", EveryFourthWeek}, {":biweekly", Biweekly},
        {":weekly", Weekly}, {":daily", Daily}};
    return v;
}

void interestRates(FILE* out) {
    std::mt19937 rng(20261009);
    std::uniform_real_distribution<double> rate(-0.01, 0.15), time(0.0, 12.0);
    std::uniform_int_distribution<int> start(0, 15 * 365), length(1, 30 * 365),
        shortLength(1, 120), dcPick(0, int(dayCounters().size()) - 1),
        compPick(0, int(compoundings().size()) - 1), freqPick(0, int(frequencies().size()) - 1),
        coin(0, 3);
    std::fprintf(out, "(:interest-rates\n");
    for (int i = 0; i < 200; ++i) {
        const NamedDC& dc = dayCounters()[dcPick(rng)];
        const NamedComp& comp = compoundings()[compPick(rng)];
        const NamedFreq& freq = frequencies()[freqPick(rng)];
        const NamedDC& dc2 = dayCounters()[dcPick(rng)];
        const NamedComp& comp2 = compoundings()[compPick(rng)];
        const NamedFreq& freq2 = frequencies()[freqPick(rng)];
        double r = rate(rng), t = time(rng);
        Date d1 = Date(1, January, 2020) + start(rng);
        // A quarter of the periods are short, to reach the simple branches.
        Date d2 = d1 + (coin(rng) == 0 ? shortLength(rng) : length(rng));
        InterestRate ir(r, dc.dc, comp.c, freq.f);
        Real compound = ir.compoundFactor(d1, d2);
        std::fprintf(out,
                     " (:rate %.17g :day-counter %s :compounding %s :frequency %s"
                     " :start \"%s\" :end \"%s\" :time %.17g\n"
                     "  :compound %.17g :discount %.17g :compound-time %.17g\n"
                     "  :result-day-counter %s :result-compounding %s :result-frequency %s\n"
                     "  :implied %.17g :equivalent %.17g)\n",
                     r, dc.name, comp.name, freq.name, iso(d1).c_str(), iso(d2).c_str(), t,
                     compound, ir.discountFactor(d1, d2), ir.compoundFactor(t),
                     dc2.name, comp2.name, freq2.name,
                     Real(InterestRate::impliedRate(compound, dc2.dc, comp2.c, freq2.f, d1, d2)),
                     Real(ir.equivalentRate(dc2.dc, comp2.c, freq2.f, d1, d2)));
    }
    std::fprintf(out, ")\n");
}

void queries(FILE* out, const YieldTermStructure& curve, const std::vector<Date>& knots) {
    std::mt19937 rng(7);
    Date ref = knots.front(), last = knots.back();
    std::uniform_int_distribution<int> day(0, int(last - ref) + 2000);
    std::uniform_int_distribution<int> dcPick(0, int(dayCounters().size()) - 1),
        compPick(0, int(compoundings().size()) - 1), freqPick(0, int(frequencies().size()) - 1);
    std::vector<Date> dates(knots);
    for (int i = 0; i < 25; ++i) dates.push_back(ref + day(rng));
    std::fprintf(out, "  :queries (\n");
    for (const Date& d : dates)
        std::fprintf(out, "   (:discount \"%s\" %.17g)\n", iso(d).c_str(), curve.discount(d));
    for (const Date& d : dates) {
        const NamedDC& dc = dayCounters()[dcPick(rng)];
        const NamedComp& comp = compoundings()[compPick(rng)];
        const NamedFreq& freq = frequencies()[freqPick(rng)];
        std::fprintf(out, "   (:zero \"%s\" %s %s %s %.17g)\n", iso(d).c_str(), dc.name, comp.name,
                     freq.name, Real(curve.zeroRate(d, dc.dc, comp.c, freq.f)));
    }
    for (int i = 0; i < 30; ++i) {
        Date d1 = ref + day(rng), d2 = (i % 6 == 0) ? d1 : d1 + 1 + day(rng) / 4;
        const NamedDC& dc = dayCounters()[dcPick(rng)];
        const NamedComp& comp = compoundings()[compPick(rng)];
        const NamedFreq& freq = frequencies()[freqPick(rng)];
        std::fprintf(out, "   (:forward \"%s\" \"%s\" %s %s %s %.17g)\n", iso(d1).c_str(),
                     iso(d2).c_str(), dc.name, comp.name, freq.name,
                     Real(curve.forwardRate(d1, d2, dc.dc, comp.c, freq.f)));
    }
    std::fprintf(out, "  ))\n");
}

void printKnots(FILE* out, const std::vector<Date>& dates, const std::vector<Real>& values) {
    std::fprintf(out, "  :dates (");
    for (const Date& d : dates) std::fprintf(out, " \"%s\"", iso(d).c_str());
    std::fprintf(out, ")\n  :values (");
    for (Real v : values) std::fprintf(out, " %.17g", v);
    std::fprintf(out, ")\n");
}

void curves(FILE* out) {
    Date ref(24, September, 2026);
    Settings::instance().evaluationDate() = ref;
    std::vector<Date> dates = {ref, ref + 7, ref + 35, ref + 98, ref + 190, ref + 370,
                               ref + 735, ref + 1830, ref + 3655, ref + 7310, ref + 10960};
    std::fprintf(out, "(:curves\n");

    std::vector<Real> dfs = {1.0};
    for (Size i = 1; i < dates.size(); ++i)
        dfs.push_back(std::exp(-(0.03 + 0.002 * i - 0.0001 * i * i) * (dates[i] - ref) / 365.0));
    {
        InterpolatedDiscountCurve<LogLinear> curve(dates, dfs, Actual360());
        curve.enableExtrapolation();
        std::fprintf(out, " (:kind :discount :interpolation :log-linear :day-counter :actual-360\n");
        printKnots(out, dates, dfs);
        queries(out, curve, dates);
    }

    std::vector<Real> zeros;
    for (Size i = 0; i < dates.size(); ++i)
        zeros.push_back(0.025 + 0.003 * std::sqrt(double(i)) - 0.0004 * i);
    {
        InterpolatedZeroCurve<Linear> curve(dates, zeros, Actual365Fixed(), Linear());
        curve.enableExtrapolation();
        std::fprintf(out, " (:kind :zero :interpolation :linear :day-counter :actual-365-fixed"
                          " :compounding :continuous :frequency :annual\n");
        printKnots(out, dates, zeros);
        queries(out, curve, dates);
    }
    {
        InterpolatedZeroCurve<Linear> curve(dates, zeros, Thirty360(Thirty360::BondBasis),
                                            Calendar(), std::vector<Handle<Quote> >(),
                                            std::vector<Date>(), Linear(), Compounded,
                                            Semiannual);
        curve.enableExtrapolation();
        std::fprintf(out, " (:kind :zero :interpolation :linear :day-counter :thirty-360-bond-basis"
                          " :compounding :compounded :frequency :semiannual\n");
        printKnots(out, dates, zeros);
        queries(out, curve, dates);
    }

    std::vector<Real> forwards;
    for (Size i = 0; i < dates.size(); ++i)
        forwards.push_back(0.02 + 0.004 * i - 0.0003 * i * i);
    {
        InterpolatedForwardCurve<BackwardFlat> curve(dates, forwards,
                                                     ActualActual(ActualActual::ISDA));
        curve.enableExtrapolation();
        std::fprintf(out, " (:kind :forward :interpolation :backward-flat"
                          " :day-counter :actual-actual-isda\n");
        printKnots(out, dates, forwards);
        queries(out, curve, dates);
    }
    std::fprintf(out, ")\n");
}

}

int main(int argc, char** argv) {
    if (argc != 2) {
        std::fprintf(stderr, "usage: %s output.sexp\n", argv[0]);
        return 1;
    }
    FILE* out = std::fopen(argv[1], "w");
    std::fprintf(out, ";;;; rates.sexp --- generated by tests/fixtures/generate.sh from QuantLib %s\n",
                 QL_VERSION);
    interestRates(out);
    curves(out);
    std::fclose(out);
    return 0;
}
