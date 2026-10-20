// calibration.cpp --- checks on the measurement itself (see calibration.lisp)

#include "../harness.hpp"

double scalarLoop(long n) {
    double x = 1.0;
    for (long i = 0; i < n; ++i)
        x = x * 1.0000001 + 1e-9;
    return x;
}

int main(int argc, char** argv) {
    bench::init(argc, argv);

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
}
