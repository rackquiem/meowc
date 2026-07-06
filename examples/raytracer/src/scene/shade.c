#include "rt_scene.h"
#include <math.h>

static int scatter(const hit *h, ray in, rng *rs, ray *out, vec3 *attenuation) {
    switch (h->mat.kind) {
    case MAT_LAMBERT: {
        vec3 dir = v3add(h->normal, rng_unit_vector(rs));
        if (v3len2(dir) < 1e-12) dir = h->normal;
        out->origin = h->point;
        out->dir = v3norm(dir);
        *attenuation = h->mat.albedo;
        return 1;
    }
    case MAT_METAL: {
        vec3 r = v3reflect(v3norm(in.dir), h->normal);
        vec3 dir = v3add(r, v3scale(rng_in_sphere(rs), h->mat.fuzz));
        out->origin = h->point;
        out->dir = v3norm(dir);
        *attenuation = h->mat.albedo;
        return v3dot(out->dir, h->normal) > 0.0;
    }
    case MAT_LIGHT:
    default:
        return 0;
    }
}

vec3 ray_colour(const scene *s, ray r, int depth, rng *rs) {
    vec3 accumulated = v3(0.0, 0.0, 0.0);
    vec3 throughput = v3(1.0, 1.0, 1.0);

    for (int bounce = 0; bounce < depth; bounce++) {
        hit h;
        if (!scene_hit(s, r, 1e-4, 1e30, &h)) {
            accumulated = v3add(accumulated, v3mul(throughput, s->background));
            break;
        }

        if (h.mat.kind == MAT_LIGHT) {
            accumulated = v3add(accumulated, v3mul(throughput, h.mat.albedo));
            break;
        }

        ray next;
        vec3 attenuation;
        if (!scatter(&h, r, rs, &next, &attenuation)) break;

        throughput = v3mul(throughput, attenuation);
        r = next;

        /* Russian roulette once the path stops carrying much energy. */
        if (bounce > 3) {
            double p = fmax(throughput.x, fmax(throughput.y, throughput.z));
            if (p < 1.0) {
                if (rng_unit(rs) > p) break;
                throughput = v3scale(throughput, 1.0 / p);
            }
        }
    }
    return accumulated;
}
