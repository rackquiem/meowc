#ifndef RT_SCENE_H
#define RT_SCENE_H

#include "rt.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    vec3 origin, dir;
} ray;

typedef enum { MAT_LAMBERT = 0, MAT_METAL = 1, MAT_LIGHT = 2 } mat_kind;

typedef struct {
    mat_kind kind;
    vec3 albedo;
    double fuzz;
} material;

typedef struct {
    vec3 center;
    double radius;
    material mat;
} sphere;

typedef struct {
    const sphere *spheres;
    int count;
    vec3 background;
} scene;

typedef struct {
    vec3 origin, lower_left, horizontal, vertical;
} camera;

typedef struct {
    double t;
    vec3 point, normal;
    material mat;
    int front;
} hit;

camera camera_make(vec3 look_from, vec3 look_at, vec3 up, double fov_deg, double aspect);
ray camera_ray(const camera *c, double u, double v);

int scene_hit(const scene *s, ray r, double tmin, double tmax, hit *out);
vec3 ray_colour(const scene *s, ray r, int depth, rng *rng_state);

typedef struct {
    const scene *sc;
    const camera *cam;
    unsigned char *rgb;
    int width, height, samples, max_depth;
    int row_next;
    int threads;
} render_job;

/* Returns the number of worker threads actually used. */
int render_run(render_job *job);

#ifdef __cplusplus
}
#endif

#endif
