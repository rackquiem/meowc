#include "mathx.h"
#include <math.h>
#include <stdio.h>

static int failures = 0;

static void check(const char *what, double got, double want) {
    if (fabs(got - want) > 1e-9) {
        printf("  %s: got %.12f, want %.12f\n", what, got, want);
        failures++;
    }
}

int main(void) {
    const double a[] = {1.0, 2.0, 3.0, 4.0};
    const double b[] = {4.0, 3.0, 2.0, 1.0};
    const double unit[] = {3.0, 4.0};

    check("vec_dot", vec_dot(a, b, 4), 20.0);
    check("vec_sum", vec_sum(a, 4), 10.0);
    check("vec_norm", vec_norm(unit, 2), 5.0);
    check("stats_mean", stats_mean(a, 4), 2.5);
    check("stats_stddev", stats_stddev(a, 4), sqrt(5.0 / 3.0));
    check("stats_mean empty", stats_mean(a, 0), 0.0);
    check("stats_stddev single", stats_stddev(a, 1), 0.0);

    return failures == 0 ? 0 : 1;
}
