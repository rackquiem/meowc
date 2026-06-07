#include "mathx.h"
#include <math.h>

double stats_mean(const double *a, size_t n) {
    if (n == 0) return 0.0;
    double sum = 0.0;
    for (size_t i = 0; i < n; i++) sum += a[i];
    return sum / (double)n;
}

double stats_stddev(const double *a, size_t n) {
    if (n < 2) return 0.0;
    double m = stats_mean(a, n);
    double acc = 0.0;
    for (size_t i = 0; i < n; i++) acc += (a[i] - m) * (a[i] - m);
    return sqrt(acc / (double)(n - 1));
}
