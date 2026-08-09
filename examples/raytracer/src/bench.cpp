// Throughput harness. Renders the same scene at rising sample counts and
// reports how the cost scales, which is a C++ consumer of the C libraries.
#include "rt_scene.h"
#include "studio_scene.h"

#include <chrono>
#include <cstdio>
#include <iomanip>
#include <iostream>
#include <vector>

namespace {

struct Row {
    int samples;
    double seconds;
    double paths_per_second;
};

Row measure(int width, int height, int samples, int threads) {
    std::vector<unsigned char> pixels(static_cast<std::size_t>(width) * height * 3);

    scene sc{studio_spheres, studio_sphere_count, studio_background};
    camera cam = camera_make(studio_look_from, studio_look_at, v3(0, 1, 0), studio_fov,
                             static_cast<double>(width) / height);

    render_job job{&sc,     &cam,   pixels.data(), width, height,
                   samples, 8,      0,             threads};

    const auto start = std::chrono::steady_clock::now();
    render_run(&job);
    const std::chrono::duration<double> elapsed = std::chrono::steady_clock::now() - start;

    const double paths = static_cast<double>(width) * height * samples;
    return Row{samples, elapsed.count(), paths / elapsed.count()};
}

} // namespace

int main(int argc, char **argv) {
    int width = 240, height = 135, threads = 4;
    if (argc > 1) threads = std::atoi(argv[1]);

    std::cout << "  bench   " << width << "x" << height << ", " << threads << " threads\n\n";
    std::cout << "  " << std::left << std::setw(12) << "samples" << std::setw(14) << "seconds"
              << "M paths/s\n";

    std::vector<Row> rows;
    for (int s : {1, 4, 16, 64}) rows.push_back(measure(width, height, s, threads));

    std::cout << std::fixed;
    for (const Row &r : rows) {
        std::cout << "  " << std::left << std::setw(12) << r.samples << std::setw(14)
                  << std::setprecision(3) << r.seconds << std::setprecision(2)
                  << r.paths_per_second / 1e6 << "\n";
    }

    // Scaling should be close to linear in sample count; report the drift.
    const double first = rows.front().paths_per_second;
    const double last = rows.back().paths_per_second;
    std::cout << "\n  throughput drift " << std::setprecision(1) << (last / first - 1.0) * 100.0
              << " %\n";
    return 0;
}
