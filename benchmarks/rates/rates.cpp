// rates.cpp --- curves, bonds, swaps, caps and swaptions (see rates.lisp)
//
// The curves come from tests/fixtures/fixture_curves.hpp; the quote tables
// below are its twins, for the bootstrap rows. Setups force the curves' lazy
// bootstraps, so the timed calls price on built curves.

#include "../common.hpp"
#include "../../tests/fixtures/fixture_curves.hpp"

using namespace QuantLib;
using namespace common;

namespace {

const std::vector<std::string> oisTenors = {"1W", "2W", "1M", "2M", "3M", "4M", "6M",
                                            "9M", "1Y", "18M", "2Y", "3Y", "4Y", "5Y",
                                            "7Y", "10Y", "12Y", "15Y", "20Y", "25Y", "30Y"};
const std::vector<double> sofrQuotes = {3.85, 3.84, 3.82, 3.79, 3.76, 3.72, 3.66,
                                        3.57, 3.50, 3.38, 3.30, 3.24, 3.25, 3.28,
                                        3.36, 3.48, 3.55, 3.62, 3.68, 3.66, 3.62};
const std::vector<double> estrQuotes = {1.93, 1.93, 1.92, 1.91, 1.90, 1.89, 1.88,
                                        1.88, 1.89, 1.93, 1.98, 2.07, 2.15, 2.23,
                                        2.37, 2.53, 2.61, 2.68, 2.70, 2.63, 2.55};
const std::vector<fixture::Quoted> euriborQuotes = {
    {":deposit", "6M", 0.0210}, {":fra", "1", 0.0208},  {":fra", "2", 0.0206},
    {":fra", "3", 0.0205},      {":fra", "6", 0.0204},  {":fra", "9", 0.0206},
    {":fra", "12", 0.0210},     {":swap", "2Y", 0.0215}, {":swap", "3Y", 0.0222},
    {":swap", "4Y", 0.0230},    {":swap", "5Y", 0.0237}, {":swap", "6Y", 0.0243},
    {":swap", "7Y", 0.0249},    {":swap", "8Y", 0.0254}, {":swap", "9Y", 0.0258},
    {":swap", "10Y", 0.0262},   {":swap", "12Y", 0.0269}, {":swap", "15Y", 0.0276},
    {":swap", "20Y", 0.0279},   {":swap", "25Y", 0.0274}, {":swap", "30Y", 0.0268}};

const Date curveEnd(24, September, 2056);

std::vector<fixture::Quoted> ois(const std::vector<double>& quotes) {
    std::vector<fixture::Quoted> result;
    for (Size i = 0; i < quotes.size(); ++i)
        result.push_back({":ois", oisTenors[i], quotes[i] / 100});
    return result;
}

double sum(const std::vector<double>& xs) {
    double s = 0;
    for (double x : xs)
        s += x;
    return s;
}

// A bootstrap's value (the discount factor 30 years out) and details (the
// discount factors at the pillars).
template <class Traits, class Interpolator>
bench::Result bootstrap(const std::vector<ext::shared_ptr<RateHelper> >& helpers) {
    auto curve = ext::make_shared<PiecewiseYieldCurve<Traits, Interpolator> >(fixture::today, helpers,
                                                                             Actual365Fixed());
    curve->enableExtrapolation();
    Real value = curve->discount(curveEnd);
    std::string pillars;
    for (const auto& h : helpers)
        pillars += (pillars.empty() ? "" : " ") + bench::number(curve->discount(h->pillarDate()));
    return {value, std::nan(""), bench::Plist().raw("pillar-discounts", "(" + pillars + ")")};
}

fixture::Curves builtCurves() {
    fixture::Curves c = fixture::buildCurves();
    for (const auto& curve : {c.sofr, c.estr, c.euribor})
        curve->discount(1.0);
    return c;
}

ext::shared_ptr<VanillaSwap> euroSwap(Swap::Type type, Real nominal, Rate rate, const Date& start,
                                      const Date& end, const ext::shared_ptr<IborIndex>& index,
                                      Spread spread = 0.0) {
    Schedule fixed(start, end, 1 * Years, TARGET(), ModifiedFollowing, ModifiedFollowing,
                   DateGeneration::Backward, false);
    Schedule floating(start, end, 6 * Months, TARGET(), ModifiedFollowing, ModifiedFollowing,
                      DateGeneration::Backward, false);
    return ext::make_shared<VanillaSwap>(type, nominal, fixed, rate, Thirty360(Thirty360::BondBasis),
                                         floating, index, spread, Actual360());
}

Schedule treasurySchedule() {
    return Schedule(Date(15, May, 2024), Date(15, May, 2034), 6 * Months,
                    UnitedStates(UnitedStates::GovernmentBond), Unadjusted, Unadjusted,
                    DateGeneration::Backward, false);
}

const std::vector<Date> resets = {Date(28, September, 2027), Date(28, September, 2028),
                                  Date(28, September, 2029), Date(30, September, 2030),
                                  Date(29, September, 2031)};

}

int main(int argc, char** argv) {
    init(argc, argv);
    using Plist = bench::Plist;

    bench::run("bootstrap-ois-log-linear", "PiecewiseYieldCurve<Discount, LogLinear>",
               Plist()("helpers", 21)("quote-sum", sum(sofrQuotes)), Plist(), [] {
                   auto helpers = fixture::helpers(ois(sofrQuotes), ext::make_shared<Sofr>(), 2, {});
                   return std::function<bench::Result()>(
                       [helpers] { return bootstrap<Discount, LogLinear>(helpers); });
               });

    bench::run("bootstrap-ois-monotone", "PiecewiseYieldCurve<ForwardRate, ConvexMonotone>",
               Plist()("helpers", 21)("quote-sum", sum(sofrQuotes)), Plist(), [] {
                   auto helpers = fixture::helpers(ois(sofrQuotes), ext::make_shared<Sofr>(), 2, {});
                   return std::function<bench::Result()>(
                       [helpers] { return bootstrap<ForwardRate, ConvexMonotone>(helpers); });
               });

    std::vector<double> euriborValues;
    for (const auto& q : euriborQuotes)
        euriborValues.push_back(q.quote);
    bench::run("bootstrap-euribor-on-estr", "PiecewiseYieldCurve<Discount, LogLinear>",
               Plist()("helpers", 21)("quote-sum", sum(euriborValues)), Plist(), [] {
                   auto estr = ext::make_shared<PiecewiseYieldCurve<Discount, LogLinear> >(
                       fixture::today,
                       fixture::helpers(ois(estrQuotes), ext::make_shared<Estr>(), 1, {}),
                       Actual365Fixed());
                   estr->enableExtrapolation();
                   estr->discount(1.0);
                   Handle<YieldTermStructure> discount(estr);
                   auto helpers =
                       fixture::helpers(euriborQuotes, ext::make_shared<Estr>(), 1, discount);
                   return std::function<bench::Result()>([helpers, estr] {
                       return bootstrap<Discount, LogLinear>(helpers);
                   });
               });

    Plist treasury = Plist()("coupon", 0.04375)("maturity", "2034-05-15")("settlement-days", 1);
    bench::run("bond-clean-price", "BondFunctions::cleanPrice", treasury, Plist(), [] {
        auto curves = builtCurves();
        Schedule schedule = treasurySchedule();
        auto bond = ext::make_shared<FixedRateBond>(1, 100.0, schedule, std::vector<Rate>{0.04375},
                                                    ActualActual(ActualActual::ISMA, schedule),
                                                    Following, 100.0);
        return std::function<bench::Result()>([bond, curves] {
            return bench::Result{
                BondFunctions::cleanPrice(*bond, *curves.sofr, bond->settlementDate())};
        });
    });

    bench::run("bond-yield", "BondFunctions::yield (Brent)",
               Plist()("coupon", 0.04375)("maturity", "2034-05-15")("settlement-days", 1)(
                   "clean-price", 99.5),
               Plist(), [] {
                   Schedule schedule = treasurySchedule();
                   DayCounter dc = ActualActual(ActualActual::ISMA, schedule);
                   auto bond = ext::make_shared<FixedRateBond>(
                       1, 100.0, schedule, std::vector<Rate>{0.04375}, dc, Following, 100.0);
                   return std::function<bench::Result()>([bond, dc] {
                       return bench::Result{BondFunctions::yield(
                           *bond, Bond::Price(99.5, Bond::Price::Clean), dc, Compounded,
                           Semiannual, bond->settlementDate(), 1.0e-14, 200)};
                   });
               });

    Plist swapInputs = Plist()("nominal", 5e6)("fixed-rate", 0.026)("spread", 0.001)(
        "start", "2026-09-28")("end", "2036-09-29");
    auto swapSetup = [] {
        auto curves = builtCurves();
        auto swap = euroSwap(Swap::Receiver, 5e6, 0.026, Date(28, September, 2026),
                             Date(29, September, 2036),
                             ext::make_shared<Euribor6M>(curves.euriborHandle), 0.001);
        swap->setPricingEngine(ext::make_shared<DiscountingSwapEngine>(curves.estrHandle));
        return std::make_pair(swap, curves);
    };
    bench::run("swap-npv", "VanillaSwap + DiscountingSwapEngine", swapInputs, Plist(), [swapSetup] {
        auto [swap, curves] = swapSetup();
        return std::function<bench::Result()>([swap = swap, curves = curves] {
            swap->recalculate();
            return bench::Result{swap->NPV()};
        });
    });
    bench::run("swap-fair-rate", "VanillaSwap + DiscountingSwapEngine", swapInputs, Plist(),
               [swapSetup] {
                   auto [swap, curves] = swapSetup();
                   return std::function<bench::Result()>([swap = swap, curves = curves] {
                       swap->recalculate();
                       return bench::Result{swap->fairRate()};
                   });
               });

    bench::run("cap-black", "Cap + BlackCapFloorEngine",
               Plist()("nominal", 1e6)("cap-rate", 0.025)("vol", 0.25)("start", "2026-09-28")(
                   "end", "2031-09-29"),
               Plist(), [] {
                   auto curves = builtCurves();
                   Schedule schedule(Date(28, September, 2026), Date(29, September, 2031),
                                     6 * Months, TARGET(), ModifiedFollowing, ModifiedFollowing,
                                     DateGeneration::Backward, false);
                   Leg leg = IborLeg(schedule, ext::make_shared<Euribor6M>(curves.euriborHandle))
                                 .withNotionals(1e6)
                                 .withPaymentDayCounter(Actual360());
                   auto cap = ext::make_shared<Cap>(leg, std::vector<Rate>{0.025});
                   cap->setPricingEngine(ext::make_shared<BlackCapFloorEngine>(
                       curves.estrHandle, 0.25, Actual365Fixed()));
                   return std::function<bench::Result()>(
                       [cap, curves] { return npv(cap)(); });
               });

    bench::run("swaption-black", "Swaption + BlackSwaptionEngine",
               Plist()("nominal", 1e6)("fixed-rate", 0.025)("vol", 0.2)("exercise", "2027-09-24"),
               Plist(), [] {
                   auto curves = builtCurves();
                   auto swaption = ext::make_shared<Swaption>(
                       euroSwap(Swap::Payer, 1e6, 0.025, Date(28, September, 2027),
                                Date(28, September, 2032),
                                ext::make_shared<Euribor6M>(curves.euriborHandle)),
                       ext::make_shared<EuropeanExercise>(Date(24, September, 2027)));
                   swaption->setPricingEngine(ext::make_shared<BlackSwaptionEngine>(
                       curves.estrHandle, 0.2, Actual365Fixed(), 0.0));
                   return std::function<bench::Result()>(
                       [swaption, curves] { return npv(swaption)(); });
               });

    bench::run("swaption-hw-jamshidian", "JamshidianSwaptionEngine",
               Plist()("nominal", 1e6)("fixed-rate", 0.027)("a", 0.05)("sigma", 0.01)(
                   "exercise", "2027-09-28"),
               Plist(), [] {
                   auto curves = builtCurves();
                   auto model = ext::make_shared<HullWhite>(curves.euriborHandle, 0.05, 0.01);
                   auto swaption = ext::make_shared<Swaption>(
                       euroSwap(Swap::Payer, 1e6, 0.027, resets.front(), Date(28, September, 2036),
                                ext::make_shared<Euribor6M>(curves.euriborHandle)),
                       ext::make_shared<EuropeanExercise>(resets.front()));
                   swaption->setPricingEngine(ext::make_shared<JamshidianSwaptionEngine>(model));
                   return std::function<bench::Result()>(
                       [swaption, curves] { return npv(swaption)(); });
               });

    bench::run("swaption-bermudan-hw-tree", "TreeSwaptionEngine",
               Plist()("nominal", 1e6)("fixed-rate", 0.027)("a", 0.05)("sigma", 0.01)(
                   "exercise-dates", 5),
               Plist()("time-steps", 100), [] {
                   auto curves = builtCurves();
                   auto model = ext::make_shared<HullWhite>(curves.euriborHandle, 0.05, 0.01);
                   auto swaption = ext::make_shared<Swaption>(
                       euroSwap(Swap::Payer, 1e6, 0.027, resets.front(), Date(28, September, 2036),
                                ext::make_shared<Euribor6M>(curves.euriborHandle)),
                       ext::make_shared<BermudanExercise>(resets));
                   swaption->setPricingEngine(ext::make_shared<TreeSwaptionEngine>(model, 100));
                   return std::function<bench::Result()>(
                       [swaption, curves] { return npv(swaption)(); });
               });
}
