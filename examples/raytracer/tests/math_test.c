#include "rt.h"
#include <math.h>
#include <stdio.h>

static int failures = 0;

static void near(const char *what, double got, double want, double tol) {
    if (fabs(got - want) > tol) {
        fprintf(stderr, "  %s: got %.9f, want %.9f\n", what, got, want);
        failures++;
    }
}

static void check(const char *what, int cond) {
    if (!cond) {
        fprintf(stderr, "  %s: failed\n", what);
        failures++;
    }
}

int main(void) {
    vec3 a = v3(1.0, 2.0, 3.0), b = v3(-4.0, 5.0, -6.0);

    near("dot", v3dot(a, b), -4.0 + 10.0 - 18.0, 1e-12);
    near("len", v3len(v3(3.0, 4.0, 0.0)), 5.0, 1e-12);
    near("norm is unit", v3len(v3norm(b)), 1.0, 1e-12);

    vec3 c = v3cross(a, b);
    near("cross is perpendicular to a", v3dot(c, a), 0.0, 1e-12);
    near("cross is perpendicular to b", v3dot(c, b), 0.0, 1e-12);

    /* A ray striking a surface head on comes straight back. */
    vec3 r = v3reflect(v3(0.0, -1.0, 0.0), v3(0.0, 1.0, 0.0));
    near("reflect x", r.x, 0.0, 1e-12);
    near("reflect y", r.y, 1.0, 1e-12);

    vec3 mid = v3lerp(v3(0.0, 0.0, 0.0), v3(2.0, 4.0, 6.0), 0.5);
    near("lerp midpoint", mid.y, 2.0, 1e-12);

    near("zero length normalises to itself", v3len(v3norm(v3(0, 0, 0))), 0.0, 1e-12);

    /* The generator must be reproducible from a seed, and stay in range. */
    rng x, y;
    rng_seed(&x, 12345);
    rng_seed(&y, 12345);
    double sum = 0.0, lo = 1.0, hi = 0.0;
    for (int i = 0; i < 200000; i++) {
        double u = rng_unit(&x);
        check("same seed gives the same stream", u == rng_unit(&y));
        if (u < lo) lo = u;
        if (u > hi) hi = u;
        sum += u;
    }
    check("stays inside [0,1)", lo >= 0.0 && hi < 1.0);
    near("mean is near one half", sum / 200000.0, 0.5, 0.005);

    for (int i = 0; i < 5000; i++) check("unit vector is unit", fabs(v3len(rng_unit_vector(&x)) - 1.0) < 1e-9);
    for (int i = 0; i < 5000; i++) check("sample sits inside the sphere", v3len2(rng_in_sphere(&x)) < 1.0);

    if (failures == 0) printf("  math: every assertion held\n");
    return failures == 0 ? 0 : 1;
}
