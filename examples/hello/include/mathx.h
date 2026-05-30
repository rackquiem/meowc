#ifndef MATHX_H
#define MATHX_H

#include <stddef.h>

double vec_dot(const double *a, const double *b, size_t n);
double vec_norm(const double *a, size_t n);
double vec_sum(const double *a, size_t n);
double stats_mean(const double *a, size_t n);
double stats_stddev(const double *a, size_t n);

#endif
