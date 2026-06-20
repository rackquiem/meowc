#include "rt.h"
#include <math.h>

static unsigned long long splitmix(unsigned long long *x) {
    unsigned long long z = (*x += 0x9E3779B97F4A7C15ULL);
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
}

static unsigned long long rotl(unsigned long long x, int k) {
    return (x << k) | (x >> (64 - k));
}

void rng_seed(rng *r, unsigned long long seed) {
    unsigned long long s = seed ? seed : 0x2545F4914F6CDD1DULL;
    r->s0 = splitmix(&s);
    r->s1 = splitmix(&s);
}

static unsigned long long rng_next(rng *r) {
    unsigned long long s0 = r->s0, s1 = r->s1;
    unsigned long long result = s0 + s1;
    s1 ^= s0;
    r->s0 = rotl(s0, 55) ^ s1 ^ (s1 << 14);
    r->s1 = rotl(s1, 36);
    return result;
}

double rng_unit(rng *r) {
    /* 53 significant bits, the most a double holds exactly. */
    return (double)(rng_next(r) >> 11) * (1.0 / 9007199254740992.0);
}

double rng_range(rng *r, double lo, double hi) { return lo + (hi - lo) * rng_unit(r); }

vec3 rng_in_sphere(rng *r) {
    for (int i = 0; i < 64; i++) {
        vec3 p = v3(rng_range(r, -1.0, 1.0), rng_range(r, -1.0, 1.0), rng_range(r, -1.0, 1.0));
        if (v3len2(p) < 1.0) return p;
    }
    return v3(0.0, 0.0, 0.0);
}

vec3 rng_unit_vector(rng *r) {
    double z = rng_range(r, -1.0, 1.0);
    double a = rng_range(r, 0.0, 6.283185307179586);
    double s = sqrt(1.0 - z * z);
    return v3(s * cos(a), s * sin(a), z);
}
