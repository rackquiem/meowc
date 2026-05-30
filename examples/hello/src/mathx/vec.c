#include "mathx.h"
#include <math.h>

double vec_dot(const double *a, const double *b, size_t n) {
    double sum = 0.0;
    for (size_t i = 0; i < n; i++) sum += a[i] * b[i];
    return sum;
}

double vec_norm(const double *a, size_t n) { return sqrt(vec_dot(a, a, n)); }

double vec_sum(const double *a, size_t n) {
    double s = 0.0;
    for (size_t i = 0; i < n; i++) s += a[i];
    return s;
}
