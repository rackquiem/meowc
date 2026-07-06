#include "rt_config.h"
#include "rt_scene.h"
#include <math.h>
#include <stdlib.h>

#if defined(HAVE_PTHREAD_CREATE) && defined(HAVE_PTHREAD_H)
#include <pthread.h>
#define RT_THREADED 1
#endif

static unsigned char to_srgb(double linear) {
    double c = linear <= 0.0031308 ? linear * 12.92 : 1.055 * pow(linear, 1.0 / 2.4) - 0.055;
    if (c < 0.0) c = 0.0;
    if (c > 1.0) c = 1.0;
    return (unsigned char)(c * 255.0 + 0.5);
}

static void render_row(render_job *job, int y) {
    rng rs;
    rng_seed(&rs, (unsigned long long)y * 0x9E3779B97F4A7C15ULL + 1u);

    for (int x = 0; x < job->width; x++) {
        vec3 sum = v3(0.0, 0.0, 0.0);
        for (int s = 0; s < job->samples; s++) {
            double u = ((double)x + rng_unit(&rs)) / (double)job->width;
            double v = 1.0 - ((double)y + rng_unit(&rs)) / (double)job->height;
            ray r = camera_ray(job->cam, u, v);
            sum = v3add(sum, ray_colour(job->sc, r, job->max_depth, &rs));
        }
        vec3 c = v3scale(sum, 1.0 / (double)job->samples);
        unsigned char *px = job->rgb + ((size_t)y * (size_t)job->width + (size_t)x) * 3u;
        px[0] = to_srgb(c.x);
        px[1] = to_srgb(c.y);
        px[2] = to_srgb(c.z);
    }
}

#ifdef RT_THREADED
static pthread_mutex_t row_lock = PTHREAD_MUTEX_INITIALIZER;

static void *worker(void *arg) {
    render_job *job = (render_job *)arg;
    for (;;) {
        pthread_mutex_lock(&row_lock);
        int y = job->row_next++;
        pthread_mutex_unlock(&row_lock);
        if (y >= job->height) break;
        render_row(job, y);
    }
    return NULL;
}
#endif

int render_run(render_job *job) {
    job->row_next = 0;

#ifdef RT_THREADED
    int n = job->threads;
    if (n < 1) n = 1;
    if (n > 64) n = 64;
    if (n > 1) {
        pthread_t *ts = (pthread_t *)calloc((size_t)n, sizeof *ts);
        if (ts) {
            int started = 0;
            for (int i = 0; i < n; i++)
                if (pthread_create(&ts[i], NULL, worker, job) == 0) started++;
            for (int i = 0; i < started; i++) pthread_join(ts[i], NULL);
            free(ts);
            if (started > 0) return started;
        }
    }
#endif

    for (int y = 0; y < job->height; y++) render_row(job, y);
    return 1;
}
