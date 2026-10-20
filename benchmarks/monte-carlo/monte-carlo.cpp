// monte-carlo.cpp --- Monte Carlo engines, one thread, fixed seed (see
// monte-carlo.lisp). 50,000 antithetic samples are 100,000 paths.

#include "../common.hpp"

using namespace QuantLib;
using namespace common;

int main(int argc, char** argv) {
    init(argc, argv);
    using Plist = bench::Plist;
    const Size samples = 50000;
    const BigNatural seed = 42;

    bench::run("mc-european-gbm", "MCEuropeanEngine<PseudoRandom>", equityInputs()("kind", "call"),
               Plist()("paths", 100000)("steps", 1), [=] {
                   auto option = vanilla(Option::Call);
                   option->setPricingEngine(MakeMCEuropeanEngine<PseudoRandom>(blackScholes())
                                                .withSteps(1)
                                                .withSamples(samples)
                                                .withAntitheticVariate()
                                                .withSeed(seed));
                   return npvWithError(option);
               });

    bench::run("mc-european-gbm-sobol", "MCEuropeanEngine<LowDiscrepancy>",
               equityInputs()("kind", "call"), Plist()("paths", 100000)("steps", 1), [=] {
                   auto option = vanilla(Option::Call);
                   option->setPricingEngine(MakeMCEuropeanEngine<LowDiscrepancy>(blackScholes())
                                                .withSteps(1)
                                                .withSamples(2 * samples)
                                                .withSeed(seed));
                   // No valid standard error for a low-discrepancy sequence.
                   return std::function<bench::Result()>([option] {
                       option->recalculate();
                       return bench::Result{option->NPV()};
                   });
               });

    bench::run("mc-european-heston", "MCEuropeanHestonEngine<PseudoRandom>",
               hestonInputs()("kind", "call"), Plist()("paths", 100000)("steps", 50), [=] {
                   auto option = vanilla(Option::Call);
                   option->setPricingEngine(MakeMCEuropeanHestonEngine<PseudoRandom>(hestonProcess())
                                                .withSteps(50)
                                                .withSamples(samples)
                                                .withAntitheticVariate()
                                                .withSeed(seed));
                   return npvWithError(option);
               });

    bench::run("mc-asian-arith-cv", "MCDiscreteArithmeticAPEngine<PseudoRandom>",
               equityInputs()("kind", "call")("average", "arithmetic")("fixings", 12),
               Plist()("paths", 100000)("steps", 12), [=] {
                   auto option = ext::make_shared<DiscreteAveragingAsianOption>(
                       Average::Arithmetic, 0.0, 0, monthlyDates(),
                       ext::make_shared<PlainVanillaPayoff>(Option::Call, strike),
                       ext::make_shared<EuropeanExercise>(expiry));
                   option->setPricingEngine(
                       MakeMCDiscreteArithmeticAPEngine<PseudoRandom>(blackScholes())
                           .withSamples(samples)
                           .withAntitheticVariate()
                           .withControlVariate()
                           .withSeed(seed));
                   return npvWithError(option);
               });

    bench::run("mc-barrier-down-out", "MCBarrierEngine<PseudoRandom>",
               equityInputs()("kind", "call")("barrier-type", "down-and-out")("barrier", 90)(
                   "rebate", 0),
               Plist()("paths", 100000)("steps", 252), [=] {
                   auto option = ext::make_shared<BarrierOption>(
                       Barrier::DownOut, 90.0, 0.0,
                       ext::make_shared<PlainVanillaPayoff>(Option::Call, strike),
                       ext::make_shared<EuropeanExercise>(expiry));
                   option->setPricingEngine(MakeMCBarrierEngine<PseudoRandom>(blackScholes())
                                                .withSteps(252)
                                                .withSamples(samples)
                                                .withAntitheticVariate()
                                                .withSeed(seed));
                   return npvWithError(option);
               });

    bench::run("mc-american-lsm", "MCAmericanEngine<PseudoRandom>",
               equityInputs()("kind", "put")("exercise", "american"),
               Plist()("paths", 100000)("calibration-paths", 100000)("steps", 50)("degree", 3), [=] {
                   auto option = vanilla(Option::Put, "american");
                   option->setPricingEngine(MakeMCAmericanEngine<PseudoRandom>(blackScholes())
                                                .withSteps(50)
                                                .withSamples(samples)
                                                .withCalibrationSamples(samples)
                                                .withAntitheticVariate()
                                                .withAntitheticVariateCalibration()
                                                .withPolynomialOrder(3)
                                                .withBasisSystem(LsmBasisSystem::Monomial)
                                                .withSeed(seed));
                   return npvWithError(option);
               });
}
