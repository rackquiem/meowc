#include "rt_scene.h"
#include "studio_scene.h"
#include <math.h>
#include <stdio.h>

static int failures = 0;

static void check(const char *what, int cond) {
    if (!cond) {
        fprintf(stderr, "  %s: failed\n", what);
        failures++;
    }
}

int main(void) {
    check("the generated scene has spheres", studio_sphere_count > 0);

    /* One unit sphere at the origin, hit straight down the -z axis. */
    static const sphere one[] = {{{0, 0, 0}, 1.0, {MAT_LAMBERT, {0.5, 0.5, 0.5}, 0.0}}};
    scene sc = {one, 1, {0, 0, 0}};

    ray r = {{0, 0, 5}, {0, 0, -1}};
    hit h;
    check("a ray aimed at the sphere hits it", scene_hit(&sc, r, 1e-4, 1e30, &h));
    check("it hits the near face", fabs(h.t - 4.0) < 1e-9);
    check("the normal faces the ray", fabs(h.normal.z - 1.0) < 1e-9);
    check("the hit is on the outside", h.front == 1);

    ray miss = {{0, 5, 5}, {0, 0, -1}};
    check("a ray that passes above misses", !scene_hit(&sc, miss, 1e-4, 1e30, &h));

    /* Starting inside, the first crossing is the far wall. */
    ray inside = {{0, 0, 0}, {0, 0, -1}};
    check("a ray from the centre still hits", scene_hit(&sc, inside, 1e-4, 1e30, &h));
    check("and reports the inside face", h.front == 0);

    /* The interval bounds are respected. */
    check("a hit beyond tmax is rejected", !scene_hit(&sc, r, 1e-4, 1.0, &h));

    /* Nearest sphere wins when two overlap the ray. */
    static const sphere two[] = {
        {{0, 0, 0}, 1.0, {MAT_LAMBERT, {1, 0, 0}, 0.0}},
        {{0, 0, 2.5}, 0.5, {MAT_METAL, {0, 1, 0}, 0.0}},
    };
    scene both = {two, 2, {0, 0, 0}};
    check("the closer sphere is chosen", scene_hit(&both, r, 1e-4, 1e30, &h) && h.mat.kind == MAT_METAL);

    if (failures == 0) printf("  scene: every assertion held\n");
    return failures == 0 ? 0 : 1;
}
