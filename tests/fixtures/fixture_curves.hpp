// Shared by the task 16 fixture generators: the log-linear curves of
// bootstrap.sexp (USD SOFR, EUR ESTR, EURIBOR 6M on ESTR), valued on
// 2026-09-24, and small parsing helpers.

#ifndef FINCL_FIXTURE_CURVES_HPP
#define FINCL_FIXTURE_CURVES_HPP

#include <ql/quantlib.hpp>

#include <cstdio>
#include <string>
#include <vector>

namespace fixture {

using namespace QuantLib;

inline std::string iso(const Date& d) {
    char buffer[16];
    std::snprintf(buffer, sizeof buffer, "%04d-%02d-%02d", d.year(), int(d.month()),
                  d.dayOfMonth());
    return buffer;
}

const Date today(24, September, 2026);

inline Period parsePeriod(const std::string& s) {
    int n = std::stoi(s.substr(0, s.size() - 1));
    switch (s.back()) {
      case 'D': return Period(n, Days);
      case 'W': return Period(n, Weeks);
      case 'M': return Period(n, Months);
      default: return Period(n, Years);
    }
}

struct Quoted { std::string kind, tenor; double quote; };

inline std::vector<ext::shared_ptr<RateHelper> > helpers(const std::vector<Quoted>& quotes,
                                                  const ext::shared_ptr<OvernightIndex>& on,
                                                  int paymentLag,
                                                  const Handle<YieldTermStructure>& discount) {
    std::vector<ext::shared_ptr<RateHelper> > result;
    auto euribor = ext::make_shared<Euribor6M>();
    for (const Quoted& q : quotes) {
        if (q.kind == ":ois")
            result.push_back(ext::make_shared<OISRateHelper>(
                2, parsePeriod(q.tenor), Handle<Quote>(ext::make_shared<SimpleQuote>(q.quote)), on,
                Handle<YieldTermStructure>(), false, paymentLag, Following, Annual));
        else if (q.kind == ":deposit")
            result.push_back(ext::make_shared<DepositRateHelper>(
                Handle<Quote>(ext::make_shared<SimpleQuote>(q.quote)), euribor));
        else if (q.kind == ":fra")
            result.push_back(ext::make_shared<FraRateHelper>(
                Handle<Quote>(ext::make_shared<SimpleQuote>(q.quote)),
                Natural(std::stoi(q.tenor)), euribor));
        else
            result.push_back(ext::make_shared<SwapRateHelper>(
                Handle<Quote>(ext::make_shared<SimpleQuote>(q.quote)), parsePeriod(q.tenor),
                TARGET(), Annual, Unadjusted, Thirty360(Thirty360::BondBasis), euribor,
                Handle<Quote>(), 0 * Days, discount));
    }
    return result;
}

inline Date parseDate(const std::string& s) {
    return Date(std::stoi(s.substr(8, 2)), Month(std::stoi(s.substr(5, 2))), std::stoi(s.substr(0, 4)));
}

struct Curves {
    ext::shared_ptr<YieldTermStructure> sofr, estr, euribor;
    Handle<YieldTermStructure> sofrHandle, estrHandle, euriborHandle;
};

// Bootstraps the three curves; call after setting the evaluation date.
inline Curves buildCurves() {
    // The curves of bootstrap.sexp (log-linear discount factors).
    std::vector<std::string> oisTenors = {"1W", "2W", "1M", "2M", "3M", "4M", "6M", "9M", "1Y",
                                          "18M", "2Y", "3Y", "4Y", "5Y", "7Y", "10Y", "12Y",
                                          "15Y", "20Y", "25Y", "30Y"};
    std::vector<double> sofr = {3.85, 3.84, 3.82, 3.79, 3.76, 3.72, 3.66, 3.57, 3.50, 3.38, 3.30,
                                3.24, 3.25, 3.28, 3.36, 3.48, 3.55, 3.62, 3.68, 3.66, 3.62};
    std::vector<double> estr = {1.93, 1.93, 1.92, 1.91, 1.90, 1.89, 1.88, 1.88, 1.89, 1.93, 1.98,
                                2.07, 2.15, 2.23, 2.37, 2.53, 2.61, 2.68, 2.70, 2.63, 2.55};
    std::vector<Quoted> sofrQuotes, estrQuotes;
    for (Size i = 0; i < oisTenors.size(); ++i) {
        sofrQuotes.push_back({":ois", oisTenors[i], sofr[i] / 100});
        estrQuotes.push_back({":ois", oisTenors[i], estr[i] / 100});
    }
    std::vector<Quoted> euriborQuotes = {
        {":deposit", "6M", 0.0210}, {":fra", "1", 0.0208}, {":fra", "2", 0.0206},
        {":fra", "3", 0.0205}, {":fra", "6", 0.0204}, {":fra", "9", 0.0206},
        {":fra", "12", 0.0210}, {":swap", "2Y", 0.0215}, {":swap", "3Y", 0.0222},
        {":swap", "4Y", 0.0230}, {":swap", "5Y", 0.0237}, {":swap", "6Y", 0.0243},
        {":swap", "7Y", 0.0249}, {":swap", "8Y", 0.0254}, {":swap", "9Y", 0.0258},
        {":swap", "10Y", 0.0262}, {":swap", "12Y", 0.0269}, {":swap", "15Y", 0.0276},
        {":swap", "20Y", 0.0279}, {":swap", "25Y", 0.0274}, {":swap", "30Y", 0.0268}};
    auto sofrIndex = ext::make_shared<Sofr>();
    auto estrIndex = ext::make_shared<Estr>();
    auto sofrCurve = ext::make_shared<PiecewiseYieldCurve<Discount, LogLinear> >(
        today, helpers(sofrQuotes, sofrIndex, 2, {}), Actual365Fixed());
    sofrCurve->enableExtrapolation();
    auto estrCurve = ext::make_shared<PiecewiseYieldCurve<Discount, LogLinear> >(
        today, helpers(estrQuotes, estrIndex, 1, {}), Actual365Fixed());
    estrCurve->enableExtrapolation();
    Handle<YieldTermStructure> sofrHandle(sofrCurve), estrHandle(estrCurve);
    auto euriborCurve = ext::make_shared<PiecewiseYieldCurve<Discount, LogLinear> >(
        today, helpers(euriborQuotes, estrIndex, 1, estrHandle), Actual365Fixed());
    euriborCurve->enableExtrapolation();
    Handle<YieldTermStructure> euriborHandle(euriborCurve);
    return {sofrCurve, estrCurve, euriborCurve, sofrHandle, estrHandle, euriborHandle};
}

}

#endif
