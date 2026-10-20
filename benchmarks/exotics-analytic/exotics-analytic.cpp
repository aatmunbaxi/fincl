// exotics-analytic.cpp --- closed forms for barriers and geometric Asians
// (see exotics-analytic.lisp)

#include "../common.hpp"

using namespace QuantLib;
using namespace common;

namespace {

ext::shared_ptr<BarrierOption> barrierOption(Barrier::Type type, Real level, Real rebate) {
    return ext::make_shared<BarrierOption>(
        type, level, rebate, ext::make_shared<PlainVanillaPayoff>(Option::Call, strike),
        ext::make_shared<EuropeanExercise>(expiry));
}

}

int main(int argc, char** argv) {
    init(argc, argv);
    using Plist = bench::Plist;

    bench::run("barrier-down-out-continuous", "AnalyticBarrierEngine",
               equityInputs()("kind", "call")("barrier-type", "down-and-out")("barrier", 90)(
                   "rebate", 0),
               Plist(), [] {
                   return priced<AnalyticBarrierEngine>(barrierOption(Barrier::DownOut, 90, 0),
                                                        blackScholes());
               });

    bench::run("barrier-up-in-rebate", "AnalyticBarrierEngine",
               equityInputs()("kind", "call")("barrier-type", "up-and-in")("barrier", 120)(
                   "rebate", 3),
               Plist(), [] {
                   return priced<AnalyticBarrierEngine>(barrierOption(Barrier::UpIn, 120, 3),
                                                        blackScholes());
               });

    bench::run("asian-geometric-discrete", "AnalyticDiscreteGeometricAveragePriceAsianEngine",
               equityInputs()("kind", "call")("average", "geometric")("fixings", 12), Plist(), [] {
                   auto option = ext::make_shared<DiscreteAveragingAsianOption>(
                       Average::Geometric, 1.0, 0, monthlyDates(),
                       ext::make_shared<PlainVanillaPayoff>(Option::Call, strike),
                       ext::make_shared<EuropeanExercise>(expiry));
                   return priced<AnalyticDiscreteGeometricAveragePriceAsianEngine>(option,
                                                                                   blackScholes());
               });
}
