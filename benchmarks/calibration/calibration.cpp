// calibration.cpp --- checks on the measurement itself (see calibration.lisp)

#include "../common.hpp"

#include <cstdint>

double scalarLoop(long n) {
    double x = 1.0;
    for (long i = 0; i < n; ++i)
        x = x * 1.0000001 + 1e-9;
    return x;
}

// The floor for Monte Carlo engines (see calibration.lisp): the same
// generator and arithmetic as the Lisp side.
std::uint64_t splitmix64(std::uint64_t& state) {
    state += 0x9E3779B97F4A7C15ULL;
    std::uint64_t z = state;
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
}

bench::Result gbmTerminalLoop(long n) {
    const double s0 = 100, k = 100, rate = 0.05, dividend = 0.02, vol = 0.2;
    const double drift = rate - dividend - 0.5 * vol * vol;
    std::uint64_t state = 42;
    double sum = 0, sum2 = 0;
    auto uniform = [&state] { return double((splitmix64(state) >> 11) + 1) * 0x1p-53; };
    auto accumulate = [&](double z) {
        double payoff = std::max(s0 * std::exp(drift + vol * z) - k, 0.0);
        sum += payoff;
        sum2 += payoff * payoff;
    };
    for (long i = 0; i < n / 2; ++i) {
        double u1 = uniform(), u2 = uniform();
        double r = std::sqrt(-2.0 * std::log(u1));
        double angle = 2.0 * M_PI * u2;
        accumulate(r * std::cos(angle));
        accumulate(r * std::sin(angle));
    }
    double df = std::exp(-rate);
    double mean = sum / n;
    double variance = sum2 / n - mean * mean;
    return {df * mean, df * std::sqrt(variance / n)};
}

int main(int argc, char** argv) {
    common::init(argc, argv);

    bench::run("sleep-50ms", "std::this_thread::sleep_for", bench::Plist()("seconds", 0.05),
               bench::Plist(), [] {
                   return [] {
                       std::this_thread::sleep_for(std::chrono::milliseconds(50));
                       return bench::Result{0.05};
                   };
               });

    bench::run("scalar-loop", "loop", bench::Plist()("iterations", 100000000), bench::Plist(),
               [] {
                   // Read through a volatile so the count is not a constant.
                   static volatile long n = 100000000;
                   return [] { return bench::Result{scalarLoop(n)}; };
               });

    bench::run("gbm-terminal-loop", "loop", common::equityInputs()("kind", "call"),
               bench::Plist()("paths", 1000000)("steps", 1), [] {
                   static volatile long n = 1000000;
                   return [] { return gbmTerminalLoop(n); };
               });
}
