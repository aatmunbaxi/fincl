// Generate tests/fixtures/bonds-swaps.sexp from QuantLib: fixed-rate bonds
// (BondFunctions), vanilla and overnight indexed swaps (DiscountingSwapEngine)
// and forward rate agreements.
//
//   nix develop .#fixtures -c sh tests/fixtures/generate.sh
//
// Valued on 2026-09-24 on the log-linear curves of bootstrap.sexp: USD SOFR,
// EUR ESTR, and EURIBOR 6M discounted on ESTR, rebuilt here from the same
// quotes. The file is (:bonds (plist ...) :swaps (plist ...) :fras (plist ...)).
// Yields and z-spreads are solved to 1e-14, so they are exact for comparison.

#include "fixture_curves.hpp"

#include <ql/version.hpp>

#include <cstdio>
#include <map>
#include <string>
#include <vector>

using namespace QuantLib;

namespace {

using fixture::iso;
using fixture::today;
using fixture::parsePeriod;
using fixture::parseDate;

std::map<std::string, Calendar> calendars = {
    {":target", TARGET()},
    {":united-states-government-bond", UnitedStates(UnitedStates::GovernmentBond)},
    {":united-states-settlement", UnitedStates(UnitedStates::Settlement)},
    {":united-kingdom-settlement", UnitedKingdom(UnitedKingdom::Settlement)}};

std::map<std::string, BusinessDayConvention> conventions = {
    {":following", Following}, {":modified-following", ModifiedFollowing},
    {":preceding", Preceding}, {":unadjusted", Unadjusted}};

std::map<std::string, DateGeneration::Rule> rules = {
    {":backward", DateGeneration::Backward}, {":forward", DateGeneration::Forward}};

std::map<std::string, Frequency> frequencies = {
    {":annual", Annual}, {":semiannual", Semiannual}, {":quarterly", Quarterly},
    {":monthly", Monthly}};

DayCounter dayCounter(const std::string& name, const Schedule& schedule) {
    if (name == ":actual-360") return Actual360();
    if (name == ":actual-365-fixed") return Actual365Fixed();
    if (name == ":thirty-360-bond-basis") return Thirty360(Thirty360::BondBasis);
    if (name == ":actual-actual-isda") return ActualActual(ActualActual::ISDA);
    if (name == ":actual-actual-icma") return ActualActual(ActualActual::ISMA, schedule);
    throw std::runtime_error("unknown day counter " + name);
}

struct ScheduleSpec {
    std::string effective, termination, tenor, calendar, convention, terminationConvention, rule;
    bool endOfMonth;
    std::string firstDate;
};

Schedule makeSchedule(const ScheduleSpec& s) {
    return Schedule(parseDate(s.effective), parseDate(s.termination), parsePeriod(s.tenor),
                    calendars.at(s.calendar), conventions.at(s.convention),
                    conventions.at(s.terminationConvention), rules.at(s.rule), s.endOfMonth,
                    s.firstDate.empty() ? Date() : parseDate(s.firstDate));
}

void printSchedule(FILE* out, const ScheduleSpec& s) {
    std::fprintf(out, "   :schedule (:effective \"%s\" :termination \"%s\" :tenor \"%s\" :calendar %s "
                      ":convention %s :termination-convention %s :rule %s :end-of-month %s "
                      ":first-date %s%s%s)\n",
                 s.effective.c_str(), s.termination.c_str(), s.tenor.c_str(), s.calendar.c_str(),
                 s.convention.c_str(), s.terminationConvention.c_str(), s.rule.c_str(),
                 s.endOfMonth ? "t" : "nil", s.firstDate.empty() ? "nil" : "\"",
                 s.firstDate.c_str(), s.firstDate.empty() ? "" : "\"");
}

struct BondSpec {
    std::string name, currency;
    ScheduleSpec schedule;
    double coupon;
    std::string dayCounter;
    double face;
    int settlementDays;
    std::string paymentConvention;
    double redemption;
    std::string issueDate;
    std::string yieldDayCounter, yieldFrequency;
    double priceOffset;   // the quoted clean price is the curve's plus this
};

void printBond(FILE* out, const BondSpec& b, const ext::shared_ptr<YieldTermStructure>& curve) {
    Schedule schedule = makeSchedule(b.schedule);
    DayCounter dc = dayCounter(b.dayCounter, schedule);
    FixedRateBond bond(b.settlementDays, b.face, schedule, {b.coupon}, dc,
                       conventions.at(b.paymentConvention), b.redemption,
                       b.issueDate.empty() ? Date() : parseDate(b.issueDate));
    Handle<YieldTermStructure> handle(curve);
    bond.setPricingEngine(ext::make_shared<DiscountingBondEngine>(handle));
    Date settlement = bond.settlementDate();
    Real clean = BondFunctions::cleanPrice(bond, *curve, settlement);
    Real dirty = BondFunctions::dirtyPrice(bond, *curve, settlement);
    Real price = clean + b.priceOffset;
    DayCounter ydc = dayCounter(b.yieldDayCounter, schedule);
    Frequency freq = frequencies.at(b.yieldFrequency);
    Rate y = BondFunctions::yield(bond, Bond::Price(price, Bond::Price::Clean), ydc, Compounded,
                                  freq, settlement, 1.0e-14, 200);
    InterestRate rate(y, ydc, Compounded, freq);
    Spread z = BondFunctions::zSpread(bond, Bond::Price(price, Bond::Price::Clean), curve, ydc,
                                      Compounded, freq, settlement, 1.0e-14, 200);
    std::fprintf(out, "  (:name \"%s\" :currency %s\n", b.name.c_str(), b.currency.c_str());
    printSchedule(out, b.schedule);
    std::fprintf(out, "   :coupon %.17g :day-counter %s :face %.17g :settlement-days %d "
                      ":payment-convention %s :redemption %.17g :issue-date %s%s%s\n",
                 b.coupon, b.dayCounter.c_str(), b.face, b.settlementDays,
                 b.paymentConvention.c_str(), b.redemption, b.issueDate.empty() ? "nil" : "\"",
                 b.issueDate.c_str(), b.issueDate.empty() ? "" : "\"");
    std::fprintf(out, "   :yield-day-counter %s :yield-frequency %s\n", b.yieldDayCounter.c_str(),
                 b.yieldFrequency.c_str());
    std::fprintf(out, "   :settlement \"%s\" :notional %.17g :accrued %.17g\n", iso(settlement).c_str(),
                 bond.notional(settlement), bond.accruedAmount(settlement));
    std::fprintf(out, "   :npv %.17g :clean %.17g :dirty %.17g :bps %.17g\n", bond.NPV(), clean, dirty,
                 BondFunctions::bps(bond, *curve, settlement));
    std::fprintf(out, "   :price %.17g :yield %.17g :yield-clean %.17g :yield-dirty %.17g\n", price, y,
                 BondFunctions::cleanPrice(bond, rate, settlement),
                 BondFunctions::dirtyPrice(bond, rate, settlement));
    std::fprintf(out, "   :modified %.17g :macaulay %.17g :simple %.17g :convexity %.17g\n",
                 BondFunctions::duration(bond, rate, Duration::Modified, settlement),
                 BondFunctions::duration(bond, rate, Duration::Macaulay, settlement),
                 BondFunctions::duration(bond, rate, Duration::Simple, settlement),
                 BondFunctions::convexity(bond, rate, settlement));
    std::fprintf(out, "   :z-spread %.17g :z-clean %.17g\n", z,
                 BondFunctions::cleanPrice(bond, curve, z, ydc, Compounded, freq, settlement));
    std::fprintf(out, "   :cashflows (");
    for (const auto& cf : bond.cashflows())
        std::fprintf(out, "(\"%s\" %.17g) ", iso(cf->date()).c_str(), cf->amount());
    std::fprintf(out, "))\n");
}

}

int main(int argc, char** argv) {
    if (argc != 2) {
        std::fprintf(stderr, "usage: %s output.sexp\n", argv[0]);
        return 1;
    }
    Settings::instance().evaluationDate() = today;
    FILE* out = std::fopen(argv[1], "w");
    std::fprintf(out, ";;;; bonds-swaps.sexp --- generated by tests/fixtures/generate.sh from QuantLib %s\n",
                 QL_VERSION);

    fixture::Curves c = fixture::buildCurves();
    auto sofrCurve = c.sofr; auto estrCurve = c.estr; auto euriborCurve = c.euribor;
    Handle<YieldTermStructure> sofrHandle = c.sofrHandle, estrHandle = c.estrHandle,
                               euriborHandle = c.euriborHandle;
    std::map<std::string, ext::shared_ptr<YieldTermStructure> > curves = {
        {":usd", sofrCurve}, {":eur", estrCurve}};

    // ---------------------------------------------------------------- bonds
    std::vector<BondSpec> bonds = {
        {"treasury 10Y", ":usd",
         {"2024-05-15", "2034-05-15", "6M", ":united-states-government-bond", ":unadjusted",
          ":unadjusted", ":backward", false, ""},
         0.04375, ":actual-actual-icma", 100, 1, ":following", 100, "", ":actual-actual-icma",
         ":semiannual", 0.75},
        {"euro 5Y annual", ":eur",
         {"2026-06-15", "2031-06-15", "1Y", ":target", ":modified-following", ":modified-following",
          ":backward", false, ""},
         0.025, ":thirty-360-bond-basis", 100, 2, ":modified-following", 100, "",
         ":thirty-360-bond-basis", ":annual", -0.40},
        {"short first stub", ":usd",
         {"2026-08-10", "2030-07-15", "6M", ":united-states-settlement", ":modified-following",
          ":modified-following", ":backward", false, "2027-01-15"},
         0.03, ":actual-360", 100, 2, ":following", 100, "", ":actual-360", ":semiannual", 1.10},
        {"long last stub, 101 redemption", ":usd",
         {"2025-11-20", "2029-02-05", "3M", ":united-states-settlement", ":following", ":following",
          ":forward", false, ""},
         0.05, ":actual-365-fixed", 1000, 2, ":following", 101, "", ":actual-365-fixed",
         ":quarterly", -2.0},
        {"euro 30Y low coupon", ":eur",
         {"2026-03-01", "2056-03-01", "1Y", ":target", ":following", ":following", ":backward", false,
          ""},
         0.005, ":actual-actual-icma", 100, 2, ":following", 100, "", ":actual-actual-icma",
         ":annual", 0.0},
        {"forward issue", ":usd",
         {"2026-10-15", "2036-10-15", "6M", ":united-states-government-bond", ":following",
          ":following", ":backward", false, ""},
         0.04, ":thirty-360-bond-basis", 100, 2, ":following", 100, "2026-10-15",
         ":thirty-360-bond-basis", ":semiannual", 0.25},
        {"coupon on settlement", ":usd",
         {"2024-03-25", "2031-03-25", "6M", ":united-states-government-bond", ":unadjusted",
          ":unadjusted", ":backward", false, ""},
         0.0375, ":actual-actual-icma", 100, 1, ":following", 100, "", ":actual-actual-icma",
         ":semiannual", -0.15},
        {"euro monthly, T+0", ":eur",
         {"2026-01-20", "2029-01-20", "1M", ":target", ":modified-following", ":modified-following",
          ":backward", false, ""},
         0.02, ":actual-360", 100, 0, ":modified-following", 100, "", ":actual-360", ":monthly",
         0.30},
        {"30Y, ACT/365F yield", ":usd",
         {"2025-08-15", "2055-08-15", "6M", ":united-states-government-bond", ":unadjusted",
          ":unadjusted", ":backward", false, ""},
         0.0475, ":thirty-360-bond-basis", 100, 1, ":following", 100, "", ":actual-365-fixed",
         ":semiannual", -1.0},
        {"short dated", ":usd",
         {"2025-03-31", "2027-03-31", "6M", ":united-states-government-bond", ":unadjusted",
          ":unadjusted", ":backward", true, ""},
         0.0125, ":actual-actual-icma", 100, 1, ":following", 100, "", ":actual-actual-icma",
         ":semiannual", 0.05}};
    std::fprintf(out, "(:bonds\n (\n");
    for (const BondSpec& b : bonds) printBond(out, b, curves.at(b.currency));
    std::fprintf(out, " )\n");

    // ---------------------------------------------------------------- swaps
    std::fprintf(out, " :swaps\n (\n");
    auto euribor = ext::make_shared<Euribor6M>(euriborHandle);
    auto sofrForecast = ext::make_shared<Sofr>(sofrHandle);
    struct VanillaSpec {
        std::string name; bool payer; double nominal, rate, spread;
        std::string start, end;
    };
    std::vector<VanillaSpec> vanillas = {
        {"5Y payer at 2.4%", true, 1e6, 0.024, 0.0, "2026-09-28", "2031-09-28"},
        {"10Y receiver at 2.6%, spread 10bp", false, 5e6, 0.026, 0.001, "2026-09-28", "2036-09-29"},
        {"2Y into 3Y payer at 2.5%", true, 1e7, 0.025, 0.0, "2028-09-28", "2031-09-29"},
        {"30Y receiver at 2.7%", false, 1e6, 0.027, -0.0005, "2026-09-28", "2056-09-28"}};
    DiscountingSwapEngine estrEngine(estrHandle), sofrEngine(sofrHandle);
    for (const VanillaSpec& v : vanillas) {
        Date start = parseDate(v.start), end = parseDate(v.end);
        Schedule fixed(start, end, 1 * Years, TARGET(), ModifiedFollowing, ModifiedFollowing,
                       DateGeneration::Backward, false);
        Schedule floating(start, end, 6 * Months, TARGET(), ModifiedFollowing, ModifiedFollowing,
                          DateGeneration::Backward, false);
        VanillaSwap swap(v.payer ? Swap::Payer : Swap::Receiver, v.nominal, fixed, v.rate,
                         Thirty360(Thirty360::BondBasis), floating, euribor, v.spread, Actual360());
        swap.setPricingEngine(ext::make_shared<DiscountingSwapEngine>(estrHandle));
        std::fprintf(out, "  (:name \"%s\" :kind :vanilla :type %s :nominal %.17g :fixed-rate %.17g "
                          ":spread %.17g :start \"%s\" :end \"%s\"\n",
                     v.name.c_str(), v.payer ? ":payer" : ":receiver", v.nominal, v.rate, v.spread,
                     v.start.c_str(), v.end.c_str());
        std::fprintf(out, "   :npv %.17g :fair-rate %.17g :fair-spread %.17g\n", swap.NPV(),
                     swap.fairRate(), swap.fairSpread());
        std::fprintf(out, "   :fixed-npv %.17g :floating-npv %.17g :fixed-bps %.17g :floating-bps %.17g)\n",
                     swap.fixedLegNPV(), swap.floatingLegNPV(), swap.fixedLegBPS(),
                     swap.floatingLegBPS());
    }
    struct OisSpec {
        std::string name; bool payer; double nominal, rate, spread;
        std::string start, end; int paymentLag;
    };
    std::vector<OisSpec> oises = {
        {"SOFR 5Y payer at 3.3%", true, 1e6, 0.033, 0.0, "2026-09-28", "2031-09-29", 2},
        {"SOFR 1Y receiver at 3.5%", false, 2e7, 0.035, 0.0, "2026-09-28", "2027-09-28", 2},
        {"SOFR 6M into 2Y payer, spread 5bp", true, 1e6, 0.031, 0.0005, "2027-03-29", "2029-03-29", 0}};
    for (const OisSpec& o : oises) {
        Date start = parseDate(o.start), end = parseDate(o.end);
        Schedule schedule(start, end, 1 * Years, UnitedStates(UnitedStates::SOFR), ModifiedFollowing,
                          ModifiedFollowing, DateGeneration::Backward, false);
        OvernightIndexedSwap swap(o.payer ? Swap::Payer : Swap::Receiver, o.nominal, schedule, o.rate,
                                  Actual360(), sofrForecast, o.spread, o.paymentLag, Following);
        swap.setPricingEngine(ext::make_shared<DiscountingSwapEngine>(sofrHandle));
        std::fprintf(out, "  (:name \"%s\" :kind :ois :type %s :nominal %.17g :fixed-rate %.17g "
                          ":spread %.17g :start \"%s\" :end \"%s\" :payment-lag %d\n",
                     o.name.c_str(), o.payer ? ":payer" : ":receiver", o.nominal, o.rate, o.spread,
                     o.start.c_str(), o.end.c_str(), o.paymentLag);
        std::fprintf(out, "   :npv %.17g :fair-rate %.17g :fair-spread %.17g\n", swap.NPV(),
                     swap.fairRate(), swap.fairSpread());
        std::fprintf(out, "   :fixed-npv %.17g :floating-npv %.17g :fixed-bps %.17g :floating-bps %.17g)\n",
                     swap.fixedLegNPV(), swap.overnightLegNPV(), swap.fixedLegBPS(),
                     swap.overnightLegBPS());
    }
    std::fprintf(out, " )\n");

    // ----------------------------------------------------------------- FRAs
    std::fprintf(out, " :fras\n (\n");
    struct FraSpec { std::string name, valueDate, maturity; bool longPosition; double strike, notional; };
    std::vector<FraSpec> fras = {
        {"3x9 long at 2%", "2026-12-29", "", true, 0.02, 1e6},
        {"6x12 short at 2.1%", "2027-03-29", "", false, 0.021, 5e6},
        {"12x18 long at 2.2%, explicit maturity", "2027-09-28", "2028-03-28", true, 0.022, 1e7},
        {"1x7 short at 1.9%", "2026-10-28", "", false, 0.019, 1e6}};
    for (const FraSpec& f : fras) {
        Date value = parseDate(f.valueDate);
        Position::Type type = f.longPosition ? Position::Long : Position::Short;
        ext::shared_ptr<ForwardRateAgreement> fra =
            f.maturity.empty()
                ? ext::make_shared<ForwardRateAgreement>(euribor, value, type, f.strike, f.notional,
                                                         estrHandle)
                : ext::make_shared<ForwardRateAgreement>(euribor, value, parseDate(f.maturity), type,
                                                         f.strike, f.notional, estrHandle);
        std::fprintf(out, "  (:name \"%s\" :value-date \"%s\" :maturity %s%s%s :type %s :strike %.17g "
                          ":notional %.17g\n",
                     f.name.c_str(), f.valueDate.c_str(), f.maturity.empty() ? "nil" : "\"",
                     f.maturity.c_str(), f.maturity.empty() ? "" : "\"",
                     f.longPosition ? ":long" : ":short", f.strike, f.notional);
        std::fprintf(out, "   :forward-rate %.17g :amount %.17g :npv %.17g)\n",
                     fra->forwardRate().rate(), fra->amount(), fra->NPV());
    }
    std::fprintf(out, " ))\n");
    std::fclose(out);
    return 0;
}
