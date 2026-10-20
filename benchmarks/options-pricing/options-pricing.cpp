// options-pricing.cpp --- vanillas by closed form, tree and Fourier (see
// options-pricing.lisp for the table of benchmarks)

#include "../common.hpp"

#include <ql/experimental/variancegamma/analyticvariancegammaengine.hpp>
#include <ql/experimental/variancegamma/variancegammaprocess.hpp>

using namespace QuantLib;
using namespace common;

namespace {

ext::shared_ptr<HestonModel> hestonModel() {
    return ext::make_shared<HestonModel>(hestonProcess());
}

bench::Plist jumpInputs(bench::Plist p) {
    Jumps j;
    return p("intensity", j.intensity)("jump-mean", j.jumpMean)("jump-vol", j.jumpVol);
}

}

int main(int argc, char** argv) {
    init(argc, argv);
    using Plist = bench::Plist;
    typedef BinomialVanillaEngine<CoxRossRubinstein> Crr;

    bench::run("european-call-black", "AnalyticEuropeanEngine", equityInputs()("kind", "call"),
               Plist(), [] { return priced<AnalyticEuropeanEngine>(vanilla(Option::Call), blackScholes()); });

    bench::run("american-put-baw", "BaroneAdesiWhaleyApproximationEngine",
               equityInputs()("kind", "put")("exercise", "american"), Plist(), [] {
                   return priced<BaroneAdesiWhaleyApproximationEngine>(
                       vanilla(Option::Put, "american"), blackScholes());
               });

    for (const char* id : {"american-put-crr-1000", "american-put-bbsr"})
        bench::run(id, "BinomialVanillaEngine<CoxRossRubinstein>",
                   equityInputs()("kind", "put")("exercise", "american"), Plist()("steps", 1000),
                   [] { return priced<Crr>(vanilla(Option::Put, "american"), blackScholes(), 1000); });

    for (const char* id : {"bermudan-put-quarterly", "bermudan-put-quarterly-cos"})
        bench::run(id, "BinomialVanillaEngine<CoxRossRubinstein>",
                   equityInputs()("kind", "put")("exercise", "bermudan"), Plist()("steps", 1000),
                   [] { return priced<Crr>(vanilla(Option::Put, "bermudan"), blackScholes(), 1000); });

    bench::run("heston-european-analytic", "AnalyticHestonEngine (Gauss-Laguerre)",
               hestonInputs()("kind", "call"), Plist()("laguerre-points", 128), [] {
                   return priced<AnalyticHestonEngine>(vanilla(Option::Call), hestonModel(),
                                                       Size(128));
               });

    bench::run("heston-european-cos", "COSHestonEngine", hestonInputs()("kind", "call"),
               Plist()("terms", 256)("truncation", 12), [] {
                   return priced<COSHestonEngine>(vanilla(Option::Call), hestonModel(), 12.0,
                                                  Size(256));
               });

    bench::run("bates-european", "BatesEngine (Gauss-Laguerre)",
               jumpInputs(hestonInputs()("kind", "call")), Plist()("laguerre-points", 128), [] {
                   HestonParameters h;
                   Jumps j;
                   auto process = ext::make_shared<BatesProcess>(
                       flatCurve(rate), flatCurve(dividend),
                       Handle<Quote>(ext::make_shared<SimpleQuote>(spot)), h.v0, h.kappa,
                       h.theta, h.xi, h.rho, j.intensity, j.jumpMean, j.jumpVol);
                   return priced<BatesEngine>(vanilla(Option::Call),
                                              ext::make_shared<BatesModel>(process), Size(128));
               });

    bench::run("merton-european", "JumpDiffusionEngine",
               jumpInputs(equityInputs()("kind", "call")), Plist()("relative-accuracy", 1e-12), [] {
                   Jumps j;
                   auto bs = blackScholes();
                   auto process = ext::make_shared<Merton76Process>(
                       bs->stateVariable(), bs->dividendYield(), bs->riskFreeRate(),
                       bs->blackVolatility(),
                       Handle<Quote>(ext::make_shared<SimpleQuote>(j.intensity)),
                       Handle<Quote>(ext::make_shared<SimpleQuote>(j.jumpMean)),
                       Handle<Quote>(ext::make_shared<SimpleQuote>(j.jumpVol)));
                   return priced<JumpDiffusionEngine>(vanilla(Option::Call), process, 1e-12,
                                                      Size(1000));
               });

    VarianceGammaParameters vg;
    bench::run("vg-european", "VarianceGammaEngine",
               equityInputs()("kind", "call")("sigma", vg.sigma)("nu", vg.nu)("theta", vg.theta),
               Plist()("absolute-error", 1e-10), [vg] {
                   auto process = ext::make_shared<VarianceGammaProcess>(
                       Handle<Quote>(ext::make_shared<SimpleQuote>(spot)), flatCurve(dividend),
                       flatCurve(rate), vg.sigma, vg.nu, vg.theta);
                   return priced<VarianceGammaEngine>(vanilla(Option::Call), process, 1e-10);
               });

    // QuantLib's CEV is driftless in the forward; at r = q it is fincl's.
    bench::run("cev-european", "AnalyticCEVEngine",
               equityInputs(rate, 0.05)("kind", "call")("beta", 0.5)("sigma", 2.0), Plist(), [] {
                   return priced<AnalyticCEVEngine>(vanilla(Option::Call), spot, 2.0, 0.5,
                                                    flatCurve(rate));
               });
}
