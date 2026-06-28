#include "rt_scene.h"
#include <math.h>

camera camera_make(vec3 look_from, vec3 look_at, vec3 up, double fov_deg, double aspect) {
    double theta = fov_deg * 3.14159265358979323846 / 180.0;
    double half_height = tan(theta / 2.0);
    double half_width = aspect * half_height;

    vec3 w = v3norm(v3sub(look_from, look_at));
    vec3 u = v3norm(v3cross(up, w));
    vec3 v = v3cross(w, u);

    camera c;
    c.origin = look_from;
    c.horizontal = v3scale(u, 2.0 * half_width);
    c.vertical = v3scale(v, 2.0 * half_height);
    c.lower_left = v3sub(v3sub(v3sub(look_from, v3scale(u, half_width)), v3scale(v, half_height)), w);
    return c;
}

ray camera_ray(const camera *c, double u, double v) {
    vec3 target = v3add(v3add(c->lower_left, v3scale(c->horizontal, u)), v3scale(c->vertical, v));
    ray r;
    r.origin = c->origin;
    r.dir = v3norm(v3sub(target, c->origin));
    return r;
}
