// monte-carlo.cpp --- Monte Carlo engines, one thread, fixed seed (see
// monte-carlo.lisp). 50,000 antithetic samples are 100,000 paths.

#include "../common.hpp"

using namespace QuantLib;
using namespace common;

// Box-Muller normals as a sequence generator for the Monte Carlo engines. QuantLib's
// BoxMullerGaussianRng is a scalar generator, so this supplies the sequence interface
// that GenericPseudoRandom provides for the inverse transform.
class BoxMullerRsg {
  public:
    typedef Sample<std::vector<Real>> sample_type;
    BoxMullerRsg(Size dimension, BigNatural seed)
    : dimension_(dimension), gaussian_(MersenneTwisterUniformRng(seed)),
      x_(std::vector<Real>(dimension), 1.0) {}
    const sample_type& nextSequence() const {
        for (Size i = 0; i < dimension_; i++)
            x_.value[i] = gaussian_.next().value;
        return x_;
    }
    const sample_type& lastSequence() const { return x_; }
    Size dimension() const { return dimension_; }

  private:
    Size dimension_;
    BoxMullerGaussianRng<MersenneTwisterUniformRng> gaussian_;
    mutable sample_type x_;
};

struct BoxMullerPseudoRandom {
    typedef BoxMullerRsg rsg_type;
    enum { allowsErrorEstimate = 1 };
    static rsg_type make_sequence_generator(Size dimension, BigNatural seed) {
        return rsg_type(dimension, seed);
    }
};

int main(int argc, char** argv) {
    init(argc, argv);
    using Plist = bench::Plist;
    const Size samples = 50000;
    const BigNatural seed = 42;

    bench::run("mc-european-gbm", "MCEuropeanEngine<BoxMullerPseudoRandom>", equityInputs()("kind", "call"),
               Plist()("paths", 100000)("steps", 1), [=] {
                   auto option = vanilla(Option::Call);
                   option->setPricingEngine(MakeMCEuropeanEngine<BoxMullerPseudoRandom>(blackScholes())
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

    bench::run("mc-european-heston", "MCEuropeanHestonEngine<BoxMullerPseudoRandom>",
               hestonInputs()("kind", "call"), Plist()("paths", 100000)("steps", 50), [=] {
                   auto option = vanilla(Option::Call);
                   option->setPricingEngine(MakeMCEuropeanHestonEngine<BoxMullerPseudoRandom>(hestonProcess())
                                                .withSteps(50)
                                                .withSamples(samples)
                                                .withAntitheticVariate()
                                                .withSeed(seed));
                   return npvWithError(option);
               });

    bench::run("mc-asian-arith-cv", "MCDiscreteArithmeticAPEngine<BoxMullerPseudoRandom>",
               equityInputs()("kind", "call")("average", "arithmetic")("fixings", 12),
               Plist()("paths", 100000)("steps", 12), [=] {
                   auto option = ext::make_shared<DiscreteAveragingAsianOption>(
                       Average::Arithmetic, 0.0, 0, monthlyDates(),
                       ext::make_shared<PlainVanillaPayoff>(Option::Call, strike),
                       ext::make_shared<EuropeanExercise>(expiry));
                   option->setPricingEngine(
                       MakeMCDiscreteArithmeticAPEngine<BoxMullerPseudoRandom>(blackScholes())
                           .withSamples(samples)
                           .withAntitheticVariate()
                           .withControlVariate()
                           .withSeed(seed));
                   return npvWithError(option);
               });

    bench::run("mc-barrier-down-out", "MCBarrierEngine<BoxMullerPseudoRandom>",
               equityInputs()("kind", "call")("barrier-type", "down-and-out")("barrier", 90)(
                   "rebate", 0),
               Plist()("paths", 100000)("steps", 252), [=] {
                   auto option = ext::make_shared<BarrierOption>(
                       Barrier::DownOut, 90.0, 0.0,
                       ext::make_shared<PlainVanillaPayoff>(Option::Call, strike),
                       ext::make_shared<EuropeanExercise>(expiry));
                   option->setPricingEngine(MakeMCBarrierEngine<BoxMullerPseudoRandom>(blackScholes())
                                                .withSteps(252)
                                                .withSamples(samples)
                                                .withBrownianBridge()
                                                .withAntitheticVariate()
                                                .withSeed(seed));
                   return npvWithError(option);
               });

    bench::run("mc-american-lsm", "MCAmericanEngine<BoxMullerPseudoRandom>",
               equityInputs()("kind", "put")("exercise", "american"),
               Plist()("paths", 100000)("calibration-paths", 100000)("steps", 50)("degree", 3), [=] {
                   auto option = vanilla(Option::Put, "american");
                   option->setPricingEngine(MakeMCAmericanEngine<BoxMullerPseudoRandom>(blackScholes())
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
