#include "mathx.h"
#include <stdio.h>

#ifndef GREETING
#define GREETING "hello"
#endif

int main(void) {
    double xs[] = {1.0, 2.0, 3.0, 4.0, 5.0};
    size_t n = sizeof xs / sizeof xs[0];

    printf("%s\n", GREETING);
    printf("  dot    %.3f\n", vec_dot(xs, xs, n));
    printf("  norm   %.3f\n", vec_norm(xs, n));
    printf("  mean   %.3f\n", stats_mean(xs, n));
    printf("  stddev %.3f\n", stats_stddev(xs, n));
    return 0;
}
