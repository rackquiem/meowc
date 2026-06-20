#include "rt.h"
#include <math.h>

vec3 v3(double x, double y, double z) {
    vec3 v = {x, y, z};
    return v;
}

vec3 v3add(vec3 a, vec3 b) { return v3(a.x + b.x, a.y + b.y, a.z + b.z); }
vec3 v3sub(vec3 a, vec3 b) { return v3(a.x - b.x, a.y - b.y, a.z - b.z); }
vec3 v3mul(vec3 a, vec3 b) { return v3(a.x * b.x, a.y * b.y, a.z * b.z); }
vec3 v3scale(vec3 a, double t) { return v3(a.x * t, a.y * t, a.z * t); }
vec3 v3neg(vec3 a) { return v3(-a.x, -a.y, -a.z); }

double v3dot(vec3 a, vec3 b) { return a.x * b.x + a.y * b.y + a.z * b.z; }

vec3 v3cross(vec3 a, vec3 b) {
    return v3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x);
}

double v3len2(vec3 a) { return v3dot(a, a); }
double v3len(vec3 a) { return sqrt(v3len2(a)); }

vec3 v3norm(vec3 a) {
    double n = v3len(a);
    return n > 0.0 ? v3scale(a, 1.0 / n) : a;
}

vec3 v3reflect(vec3 v, vec3 n) { return v3sub(v, v3scale(n, 2.0 * v3dot(v, n))); }

vec3 v3lerp(vec3 a, vec3 b, double t) {
    return v3add(v3scale(a, 1.0 - t), v3scale(b, t));
}
