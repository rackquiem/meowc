#include "rt_scene.h"
#include <math.h>

static int sphere_hit(const sphere *s, ray r, double tmin, double tmax, hit *out) {
    vec3 oc = v3sub(r.origin, s->center);
    double a = v3len2(r.dir);
    double half_b = v3dot(oc, r.dir);
    double c = v3len2(oc) - s->radius * s->radius;
    double disc = half_b * half_b - a * c;
    if (disc < 0.0) return 0;

    double root = sqrt(disc);
    double t = (-half_b - root) / a;
    if (t < tmin || t > tmax) {
        t = (-half_b + root) / a;
        if (t < tmin || t > tmax) return 0;
    }

    out->t = t;
    out->point = v3add(r.origin, v3scale(r.dir, t));
    vec3 outward = v3scale(v3sub(out->point, s->center), 1.0 / s->radius);
    out->front = v3dot(r.dir, outward) < 0.0;
    out->normal = out->front ? outward : v3neg(outward);
    out->mat = s->mat;
    return 1;
}

int scene_hit(const scene *s, ray r, double tmin, double tmax, hit *out) {
    hit best;
    int found = 0;
    double closest = tmax;

    for (int i = 0; i < s->count; i++) {
        hit h;
        if (sphere_hit(&s->spheres[i], r, tmin, closest, &h)) {
            found = 1;
            closest = h.t;
            best = h;
        }
    }
    if (found) *out = best;
    return found;
}
