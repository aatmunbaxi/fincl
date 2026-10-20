// calibration-models.cpp --- whole calibration jobs (see
// calibration-models.lisp). Quotes are generated in the setups, untimed.

#include "../common.hpp"
#include "../../tests/fixtures/fixture_curves.hpp"

#include <ql/experimental/volatility/sviinterpolation.hpp>

using namespace QuantLib;
using namespace common;

namespace {

const HestonParameters hestonTruth{0.05, 1.5, 0.06, 0.6, -0.65};
const std::vector<Period> tenors = {3 * Months, 6 * Months, 1 * Years, 18 * Months, 2 * Years};
const std::vector<Real> calibrationStrikes = {80, 90, 95, 100, 105, 110, 120};

const std::vector<Real> smileStrikes = {70, 76, 82, 88, 94, 100, 106, 112, 118, 124, 130};
const Real sabrAlpha = 2.0, sabrBeta = 0.5, sabrRho = -0.4, sabrNu = 0.6;
const Real sviA = 0.03, sviB = 0.1, sviRho = -0.4, sviM = 0.02, sviSigma = 0.15;
const Real hwA = 0.08, hwSigma = 0.009;

ext::shared_ptr<HestonModel> hestonModel(const HestonParameters& h) {
    return ext::make_shared<HestonModel>(hestonProcess(h));
}

Real oneYearForward() {
    return spot * flatCurve(dividend)->discount(1.0) / flatCurve(rate)->discount(1.0);
}

// Sets each helper's quote to the Black vol of its price under ENGINE.
void quoteFrom(const std::vector<ext::shared_ptr<BlackCalibrationHelper> >& helpers,
               const std::vector<ext::shared_ptr<SimpleQuote> >& vols,
               const ext::shared_ptr<PricingEngine>& engine) {
    for (Size i = 0; i < helpers.size(); ++i) {
        helpers[i]->setPricingEngine(engine);
        vols[i]->setValue(helpers[i]->impliedVolatility(helpers[i]->modelValue(), 1e-14, 1000, 1e-4, 4));
    }
}

}

int main(int argc, char** argv) {
    init(argc, argv);
    using Plist = bench::Plist;

    bench::run(
        "heston-calibration", "HestonModel + AnalyticHestonEngine (Laguerre 128), LM",
        equityInputs()("expiries", 5)("strikes", 7)("v0", hestonTruth.v0)("kappa", hestonTruth.kappa)(
            "theta", hestonTruth.theta)("xi", hestonTruth.xi)("rho", hestonTruth.rho),
        Plist(), [] {
            std::vector<ext::shared_ptr<BlackCalibrationHelper> > helpers;
            std::vector<ext::shared_ptr<SimpleQuote> > vols;
            for (const Period& tenor : tenors)
                for (Real k : calibrationStrikes) {
                    vols.push_back(ext::make_shared<SimpleQuote>(0.2));
                    helpers.push_back(ext::make_shared<HestonModelHelper>(
                        tenor, NullCalendar(), spot, k, Handle<Quote>(vols.back()), flatCurve(rate),
                        flatCurve(dividend), BlackCalibrationHelper::ImpliedVolError));
                }
            quoteFrom(helpers, vols,
                      ext::make_shared<AnalyticHestonEngine>(hestonModel(hestonTruth), Size(128)));
            std::vector<ext::shared_ptr<CalibrationHelper> > calibration(helpers.begin(),
                                                                         helpers.end());
            return std::function<bench::Result()>([helpers, calibration] {
                auto model = hestonModel(HestonParameters());
                auto engine = ext::make_shared<AnalyticHestonEngine>(model, Size(128));
                for (const auto& h : helpers)
                    h->setPricingEngine(engine);
                LevenbergMarquardt method(1e-8, 1e-8, 1e-8);
                model->calibrate(calibration, method, EndCriteria(400, 40, 1e-10, 1e-10, 1e-10));
                Real squares = 0;
                for (const auto& h : helpers)
                    squares += std::pow(h->calibrationError(), 2);
                return bench::Result{
                    std::sqrt(squares / helpers.size()), std::nan(""),
                    bench::Plist()("v0", model->v0())("kappa", model->kappa())(
                        "theta", model->theta())("xi", model->sigma())("rho", model->rho())(
                        "evaluations", model->functionEvaluation())};
            });
        });

    bench::run("sabr-smile-fit", "SABRInterpolation (Hagan)",
               equityInputs()("strikes", 11)("alpha", sabrAlpha)("beta", sabrBeta)("rho", sabrRho)(
                   "nu", sabrNu),
               Plist(), [] {
                   Real f = oneYearForward();
                   std::vector<Real> vols;
                   for (Real k : smileStrikes)
                       vols.push_back(sabrVolatility(k, f, 1.0, sabrAlpha, sabrBeta, sabrNu, sabrRho));
                   return std::function<bench::Result()>([f, vols] {
                       SABRInterpolation sabr(smileStrikes.begin(), smileStrikes.end(), vols.begin(),
                                              1.0, f, 1.0, sabrBeta, 0.3, 0.0, false, true, false,
                                              false, false, {}, {}, 0.0020, false, 1);
                       sabr.update();
                       return bench::Result{
                           sabr.rmsError(), std::nan(""),
                           bench::Plist()("alpha", sabr.alpha())("rho", sabr.rho())("nu", sabr.nu())};
                   });
               });

    bench::run("svi-fit", "SviInterpolation",
               equityInputs()("strikes", 11)("a", sviA)("b", sviB)("rho", sviRho)("m", sviM)(
                   "sigma", sviSigma),
               Plist(), [] {
                   Real f = oneYearForward();
                   std::vector<Real> vols;
                   for (Real k : smileStrikes) {
                       Real x = std::log(k / f) - sviM;
                       vols.push_back(std::sqrt(
                           sviA + sviB * (sviRho * x + std::sqrt(x * x + sviSigma * sviSigma))));
                   }
                   return std::function<bench::Result()>([f, vols] {
                       SviInterpolation svi(smileStrikes.begin(), smileStrikes.end(), vols.begin(), 1.0,
                                            f, 0.04, 0.1, 0.1, 0.0, 0.0, false, false, false, false,
                                            false, false, {}, {}, 0.0020, false, 1);
                       svi.update();
                       return bench::Result{svi.rmsError(), std::nan(""),
                                            bench::Plist()("a", svi.a())("b", svi.b())(
                                                "rho", svi.rho())("m", svi.m())("sigma", svi.sigma())};
                   });
               });

    bench::run("hull-white-calibration", "HullWhite + JamshidianSwaptionEngine, LM",
               Plist()("swaptions", 5)("fixed-rate", 0.027)("a", hwA)("sigma", hwSigma), Plist(), [] {
                   fixture::Curves curves = fixture::buildCurves();
                   Handle<YieldTermStructure> curve = curves.euriborHandle;
                   curve->discount(1.0);
                   auto index = ext::make_shared<Euribor6M>(curve);
                   std::vector<Date> resets = {Date(28, September, 2027), Date(28, September, 2028),
                                               Date(28, September, 2029), Date(30, September, 2030),
                                               Date(29, September, 2031)};
                   std::vector<ext::shared_ptr<BlackCalibrationHelper> > helpers;
                   std::vector<ext::shared_ptr<SimpleQuote> > vols;
                   for (const Date& reset : resets) {
                       vols.push_back(ext::make_shared<SimpleQuote>(0.2));
                       helpers.push_back(ext::make_shared<SwaptionHelper>(
                           reset, Date(28, September, 2036), Handle<Quote>(vols.back()), index,
                           1 * Years, Thirty360(Thirty360::BondBasis), Actual360(), curve,
                           BlackCalibrationHelper::RelativePriceError, 0.027, 1.0));
                   }
                   quoteFrom(helpers, vols,
                             ext::make_shared<JamshidianSwaptionEngine>(
                                 ext::make_shared<HullWhite>(curve, hwA, hwSigma)));
                   std::vector<ext::shared_ptr<CalibrationHelper> > calibration(helpers.begin(),
                                                                                helpers.end());
                   return std::function<bench::Result()>([helpers, calibration, curve, curves] {
                       auto model = ext::make_shared<HullWhite>(curve, 0.2, 0.02);
                       auto engine = ext::make_shared<JamshidianSwaptionEngine>(model);
                       for (const auto& h : helpers)
                           h->setPricingEngine(engine);
                       LevenbergMarquardt method(1e-8, 1e-8, 1e-8);
                       model->calibrate(calibration, method,
                                        EndCriteria(400, 40, 1e-10, 1e-10, 1e-10));
                       Real worst = 0;
                       for (const auto& h : helpers)
                           worst = std::max(worst, std::fabs(h->calibrationError()));
                       return bench::Result{model->a(), std::nan(""),
                                            bench::Plist()("sigma", model->sigma())(
                                                "max-relative-error", worst)(
                                                "evaluations", model->functionEvaluation())};
                   });
               });
}
