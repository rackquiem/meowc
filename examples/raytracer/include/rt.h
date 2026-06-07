#ifndef RT_H
#define RT_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    double x, y, z;
} vec3;

vec3 v3(double x, double y, double z);
vec3 v3add(vec3 a, vec3 b);
vec3 v3sub(vec3 a, vec3 b);
vec3 v3mul(vec3 a, vec3 b);
vec3 v3scale(vec3 a, double t);
vec3 v3neg(vec3 a);
double v3dot(vec3 a, vec3 b);
vec3 v3cross(vec3 a, vec3 b);
double v3len2(vec3 a);
double v3len(vec3 a);
vec3 v3norm(vec3 a);
vec3 v3reflect(vec3 v, vec3 n);
vec3 v3lerp(vec3 a, vec3 b, double t);

/* Deterministic xorshift128+, so a render is reproducible. */
typedef struct {
    unsigned long long s0, s1;
} rng;

void rng_seed(rng *r, unsigned long long seed);
double rng_unit(rng *r);
double rng_range(rng *r, double lo, double hi);
vec3 rng_in_sphere(rng *r);
vec3 rng_unit_vector(rng *r);

#ifdef __cplusplus
}
#endif

#endif
