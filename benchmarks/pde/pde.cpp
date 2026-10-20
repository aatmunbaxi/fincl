// pde.cpp --- finite differences, at the same grid on both sides (see pde.lisp)

#include "../common.hpp"

using namespace QuantLib;
using namespace common;

int main(int argc, char** argv) {
    init(argc, argv);
    using Plist = bench::Plist;
    auto grid = [](Size t, Size x) { return Plist()("time-steps", t)("space-nodes", x); };

    bench::run("fd-european-bs", "FdBlackScholesVanillaEngine", equityInputs()("kind", "call"),
               grid(200, 200), [] {
                   return priced<FdBlackScholesVanillaEngine>(vanilla(Option::Call), blackScholes(),
                                                              Size(200), Size(200));
               });

    bench::run("fd-american-put-bs", "FdBlackScholesVanillaEngine",
               equityInputs()("kind", "put")("exercise", "american"), grid(400, 400), [] {
                   return priced<FdBlackScholesVanillaEngine>(vanilla(Option::Put, "american"),
                                                              blackScholes(), Size(400), Size(400));
               });

    bench::run("fd-bermudan-put-bs", "FdBlackScholesVanillaEngine",
               equityInputs()("kind", "put")("exercise", "bermudan"), grid(400, 400), [] {
                   return priced<FdBlackScholesVanillaEngine>(vanilla(Option::Put, "bermudan"),
                                                              blackScholes(), Size(400), Size(400));
               });

    bench::run("fd-barrier-dko", "FdBlackScholesBarrierEngine",
               equityInputs()("kind", "call")("barrier-type", "down-and-out")("barrier", 90)(
                   "rebate", 0),
               grid(400, 400), [] {
                   auto option = ext::make_shared<BarrierOption>(
                       Barrier::DownOut, 90.0, 0.0,
                       ext::make_shared<PlainVanillaPayoff>(Option::Call, strike),
                       ext::make_shared<EuropeanExercise>(expiry));
                   return priced<FdBlackScholesBarrierEngine>(option, blackScholes(), Size(400),
                                                              Size(400));
               });

    SsviParameters ssvi;
    bench::run("fd-american-put-local", "FdBlackScholesVanillaEngine (local vol)",
               Plist()("spot", spot)("strike", strike)("rate", rate)("dividend", dividend)(
                   "vol", "ssvi")("expiry-days", 365)("kind", "put")("exercise", "american")(
                   "theta-per-year", ssvi.thetaPerYear)("rho", ssvi.rho)("eta", ssvi.eta)(
                   "gamma", ssvi.gamma),
               grid(400, 400), [ssvi] {
                   auto r = flatCurve(rate), q = flatCurve(dividend);
                   auto process = ext::make_shared<BlackScholesMertonProcess>(
                       Handle<Quote>(ext::make_shared<SimpleQuote>(spot)), q, r,
                       Handle<BlackVolTermStructure>(
                           ext::make_shared<SsviVolatility>(ssvi, spot, r, q)));
                   return priced<FdBlackScholesVanillaEngine>(
                       vanilla(Option::Put, "american"), process, Size(400), Size(400), Size(0),
                       FdmSchemeDesc::Douglas(), true);
               });

    auto hestonGrid = grid(100, 100)("variance-nodes", 50);
    bench::run("fd-heston-european", "FdHestonVanillaEngine", hestonInputs()("kind", "call"),
               hestonGrid, [] {
                   return priced<FdHestonVanillaEngine>(
                       vanilla(Option::Call), ext::make_shared<HestonModel>(hestonProcess()),
                       Size(100), Size(100), Size(50));
               });

    bench::run("fd-heston-american", "FdHestonVanillaEngine",
               hestonInputs()("kind", "put")("exercise", "american"), hestonGrid, [] {
                   return priced<FdHestonVanillaEngine>(
                       vanilla(Option::Put, "american"),
                       ext::make_shared<HestonModel>(hestonProcess()), Size(100), Size(100),
                       Size(50));
               });
}
