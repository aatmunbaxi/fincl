// harness.hpp --- the C++ twin of harness.lisp
//
// The protocol, the same as the Lisp side's: wall time on CLOCK_MONOTONIC
// (std::chrono::steady_clock); three untimed warm-up calls (one when a call
// takes over 0.5 s); a sample is a batch of calls lasting at least 20 ms,
// divided by the batch size; 30 samples (10 with --quick), fewer for slow
// calls so that a series lasts about 5 s (sampleCount). Each benchmark
// prints one s-expression on standard output:
//
//   (:id "european-call-black" :engine "AnalyticEuropeanEngine"
//    :inputs (:spot 100 ...) :work (...) :value 10.45... :std-error nil
//    :details (...) :samples (...) :batch-size 4096 :setup-seconds 3.1e-05)

#ifndef FINCL_BENCHMARK_HARNESS_HPP
#define FINCL_BENCHMARK_HARNESS_HPP

#include <ql/version.hpp>

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <exception>
#include <functional>
#include <string>
#include <thread>
#include <type_traits>
#include <vector>

namespace bench {

inline bool quick = false;
const double sampleSeconds = 0.02;
const double slowCallSeconds = 0.5;
const double seriesSeconds = 5.0;

// Samples for a call taking SINGLE seconds: 30 (10 when quick), as many as
// fit in seriesSeconds when fewer, but at least 5, and 3 above 10 s.
inline int sampleCount(double single) {
    if (single > 10.0)
        return 3;
    double fit = std::floor(seriesSeconds / std::max(single, 1e-9));
    return int(std::max(5.0, std::min(quick ? 10.0 : 30.0, fit)));
}

inline double now() {
    using namespace std::chrono;
    return duration<double>(steady_clock::now().time_since_epoch()).count();
}

// A number as the Lisp reader reads it back exactly; NIL for NaN or infinity.
inline std::string number(double x) {
    if (!std::isfinite(x))
        return "nil";
    char buffer[32];
    std::snprintf(buffer, sizeof buffer, "%.17g", x);
    return buffer;
}

inline std::string quoted(const std::string& s) {
    std::string out = "\"";
    for (char c : s) {
        if (c == '"' || c == '\\')
            out += '\\';
        out += (c == '\n') ? ' ' : c;
    }
    return out + "\"";
}

// A plist of keywords and numbers or strings: Plist()("spot", 100)("kind", "call").
class Plist {
  public:
    template <class T, class = std::enable_if_t<std::is_arithmetic_v<T> > >
    Plist& operator()(const std::string& key, T x) {
        return add(key, number(double(x)));
    }
    Plist& operator()(const std::string& key, const std::string& s) {
        return add(key, quoted(s));
    }
    Plist& operator()(const std::string& key, const char* s) {
        return add(key, quoted(s));
    }
    Plist& raw(const std::string& key, const std::string& form) {
        return add(key, form);
    }
    std::string str() const { return "(" + body_ + ")"; }

  private:
    Plist& add(const std::string& key, const std::string& value) {
        if (!body_.empty())
            body_ += ' ';
        body_ += ':' + key + ' ' + value;
        return *this;
    }
    std::string body_;
};

// What a timed call returns: the value, its standard error (NaN when
// deterministic) and details printed below the report's table.
struct Result {
    double value;
    double stdError = std::nan("");
    Plist details = Plist();
};

inline void init(int argc, char** argv) {
    for (int i = 1; i < argc; ++i)
        if (std::strcmp(argv[i], "--quick") == 0)
            quick = true;
    std::printf("(:header :quantlib %s :compiler %s)\n", quoted(QL_VERSION).c_str(),
                quoted(__VERSION__).c_str());
    std::fflush(stdout);
}

inline double timeBatch(const std::function<Result()>& fn, long n) {
    double start = now();
    for (long i = 0; i < n; ++i)
        fn();
    return now() - start;
}

// Runs one benchmark. SETUP builds the inputs (timed once) and returns the
// timed call.
template <class Setup>
void run(const std::string& id, const std::string& engine, const Plist& inputs, const Plist& work,
         Setup setup) {
    std::string head = "(:id " + quoted(id) + " :engine " + quoted(engine) + " :inputs " +
                       inputs.str() + " :work " + work.str();
    try {
        double start = now();
        std::function<Result()> fn = setup();
        double setupSeconds = now() - start;

        start = now();
        Result first = fn();
        double single = now() - start;
        // Warm-up: three calls in all, the last one timed for the batch size.
        if (single <= slowCallSeconds) {
            fn();
            single = timeBatch(fn, 1);
        }
        long batch = std::max(1L, long(std::ceil(sampleSeconds / std::max(single, 1e-9))));
        int nSamples = sampleCount(single);
        std::string samples;
        for (int i = 0; i < nSamples; ++i)
            samples += (i ? " " : "") + number(timeBatch(fn, batch) / batch);

        std::printf("%s :value %s :std-error %s :details %s :samples (%s) :batch-size %ld "
                    ":setup-seconds %s)\n",
                    head.c_str(), number(first.value).c_str(), number(first.stdError).c_str(),
                    first.details.str().c_str(), samples.c_str(), batch,
                    number(setupSeconds).c_str());
    } catch (std::exception& e) {
        std::printf("%s :error %s)\n", head.c_str(), quoted(e.what()).c_str());
    }
    std::fflush(stdout);
}

}

#endif
