// common.hpp --- the shared inputs, written twice (here and common.lisp)
//
// Every value here has a twin in common.lisp; the input echo of each
// benchmark catches the two drifting apart.

#ifndef FINCL_BENCHMARK_COMMON_HPP
#define FINCL_BENCHMARK_COMMON_HPP

#include "harness.hpp"

#include <ql/quantlib.hpp>

namespace common {

using namespace QuantLib;

// Valuation date; the expiry, 365 days later, is 1.0 in ACT/365F.
const Date today(24, September, 2026);
const Date expiry(24, September, 2027);

const Real spot = 100, strike = 100, rate = 0.05, dividend = 0.02, vol = 0.2;

struct HestonParameters {
    Real v0 = 0.04, kappa = 2, theta = 0.04, xi = 0.3, rho = -0.7;
};
struct Jumps {  // Merton's and Bates's lognormal jumps
    Real intensity = 0.5, jumpMean = -0.1, jumpVol = 0.15;
};
struct VarianceGammaParameters {
    Real sigma = 0.2, nu = 0.2, theta = -0.14;
};
struct SsviParameters {  // ATM total variance theta(T) = 0.04 T
    Real thetaPerYear = 0.04, rho = -0.6, eta = 1.2, gamma = 0.4;
};

inline void init(int argc, char** argv) {
    bench::init(argc, argv);
    Settings::instance().evaluationDate() = today;
}

inline bench::Plist equityInputs(Real r = rate, Real q = dividend, Real sigma = vol) {
    return bench::Plist()("spot", spot)("strike", strike)("rate", r)("dividend", q)("vol", sigma)(
        "expiry-days", 365);
}

inline Handle<YieldTermStructure> flatCurve(Real r) {
    return Handle<YieldTermStructure>(ext::make_shared<FlatForward>(today, r, Actual365Fixed()));
}

inline ext::shared_ptr<BlackScholesMertonProcess>
blackScholes(Real r = rate, Real q = dividend, Real sigma = vol) {
    return ext::make_shared<BlackScholesMertonProcess>(
        Handle<Quote>(ext::make_shared<SimpleQuote>(spot)), flatCurve(q), flatCurve(r),
        Handle<BlackVolTermStructure>(
            ext::make_shared<BlackConstantVol>(today, NullCalendar(), sigma, Actual365Fixed())));
}

// The timed call: recalculate defeats LazyObject's cache.
inline std::function<bench::Result()> npv(const ext::shared_ptr<Instrument>& instrument) {
    return [instrument] {
        instrument->recalculate();
        return bench::Result{instrument->NPV()};
    };
}

// The same for Monte Carlo engines, with the standard error.
inline std::function<bench::Result()> npvWithError(const ext::shared_ptr<Instrument>& instrument) {
    return [instrument] {
        instrument->recalculate();
        return bench::Result{instrument->NPV(), instrument->errorEstimate()};
    };
}

template <class Engine, class... Args>
std::function<bench::Result()> priced(const ext::shared_ptr<Instrument>& instrument,
                                      Args&&... args) {
    instrument->setPricingEngine(ext::make_shared<Engine>(std::forward<Args>(args)...));
    return npv(instrument);
}

inline std::vector<Date> quarterlyDates() {
    return {Date(24, December, 2026), Date(24, March, 2027), Date(24, June, 2027), expiry};
}

inline std::vector<Date> monthlyDates() {
    std::vector<Date> dates;
    for (Integer m = 1; m <= 12; ++m)
        dates.push_back(today + Period(m, Months));
    return dates;
}

// A plain vanilla on the shared strike expiring at EXPIRY; EXERCISE is
// "european", "american" or "bermudan" (quarterly).
inline ext::shared_ptr<VanillaOption> vanilla(Option::Type type,
                                              const std::string& exercise = "european") {
    ext::shared_ptr<Exercise> e;
    if (exercise == "american")
        e = ext::make_shared<AmericanExercise>(today, expiry);
    else if (exercise == "bermudan")
        e = ext::make_shared<BermudanExercise>(quarterlyDates());
    else
        e = ext::make_shared<EuropeanExercise>(expiry);
    return ext::make_shared<VanillaOption>(ext::make_shared<PlainVanillaPayoff>(type, strike), e);
}

inline ext::shared_ptr<HestonProcess> hestonProcess(const HestonParameters& h = {}) {
    return ext::make_shared<HestonProcess>(flatCurve(rate), flatCurve(dividend),
                                           Handle<Quote>(ext::make_shared<SimpleQuote>(spot)),
                                           h.v0, h.kappa, h.theta, h.xi, h.rho);
}

inline bench::Plist hestonInputs(const HestonParameters& h = {}) {
    return equityInputs()("v0", h.v0)("kappa", h.kappa)("theta", h.theta)("xi", h.xi)("rho", h.rho);
}

// The SSVI surface of Gatheral and Jacquier with power-law phi, in
// log-forward moneyness k = ln(K / F(T)).
class SsviVolatility : public BlackVarianceTermStructure {
  public:
    SsviVolatility(const SsviParameters& p, Real s0, Handle<YieldTermStructure> r,
                   Handle<YieldTermStructure> q)
    : BlackVarianceTermStructure(today, NullCalendar(), Following, Actual365Fixed()), p_(p),
      s0_(s0), r_(std::move(r)), q_(std::move(q)) {}
    Date maxDate() const override { return Date::maxDate(); }
    Real minStrike() const override { return 0; }
    Real maxStrike() const override { return QL_MAX_REAL; }

  protected:
    Real blackVarianceImpl(Time t, Real strike) const override {
        if (t <= 0)
            return 0;
        Real theta = p_.thetaPerYear * t;
        Real forward = s0_ * q_->discount(t) / r_->discount(t);
        Real k = std::log(strike / forward);
        Real phi = p_.eta / (std::pow(theta, p_.gamma) * std::pow(1 + theta, 1 - p_.gamma));
        Real x = phi * k;
        return 0.5 * theta *
               (1 + p_.rho * x + std::sqrt((x + p_.rho) * (x + p_.rho) + 1 - p_.rho * p_.rho));
    }

  private:
    SsviParameters p_;
    Real s0_;
    Handle<YieldTermStructure> r_, q_;
};

}

#endif
