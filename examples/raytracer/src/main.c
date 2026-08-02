#include "rt_config.h"
#include "rt_image.h"
#include "rt_scene.h"

#include "rings_scene.h"
#include "studio_scene.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>

typedef struct {
    const char *name;
    const sphere *spheres;
    int count;
    vec3 background, look_from, look_at;
    double fov;
} entry;

static double now_seconds(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec * 1e-9;
}

/* Create every directory on the way to PATH, ignoring ones already there. */
static void ensure_parent(const char *path) {
    char buf[512];
    snprintf(buf, sizeof buf, "%s", path);
    for (char *p = buf + 1; *p; p++) {
        if (*p != '/') continue;
        *p = '\0';
        mkdir(buf, 0755);
        *p = '/';
    }
}

static long file_size(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) return -1;
    fseek(f, 0, SEEK_END);
    long n = ftell(f);
    fclose(f);
    return n;
}

static void usage(const char *argv0) {
    printf("usage: %s [options]\n\n", argv0);
    printf("  --scene NAME     studio or rings (default studio)\n");
    printf("  --width N        image width in pixels (default 640)\n");
    printf("  --height N       image height in pixels (default 360)\n");
    printf("  --samples N      paths per pixel (default %d)\n", RT_DEFAULT_SAMPLES);
    printf("  --depth N        maximum bounces (default 12)\n");
    printf("  --threads N      worker threads (default 4)\n");
    printf("  --out PATH       output file (default out/<scene>.%s)\n", image_default_extension());
    printf("  --list           print the built in scenes\n");
}

static int arg_int(int argc, char **argv, int *i, const char *what) {
    if (*i + 1 >= argc) {
        fprintf(stderr, "%s needs a value\n", what);
        exit(2);
    }
    return atoi(argv[++(*i)]);
}

int main(int argc, char **argv) {
    const entry table[] = {
        {"studio", studio_spheres, studio_sphere_count, studio_background, studio_look_from,
         studio_look_at, studio_fov},
        {"rings", rings_spheres, rings_sphere_count, rings_background, rings_look_from,
         rings_look_at, rings_fov},
    };
    const int table_count = (int)(sizeof table / sizeof table[0]);

    const char *want = "studio";
    const char *out_path = NULL;
    int width = 640, height = 360, samples = RT_DEFAULT_SAMPLES, depth = 12, threads = 4;

    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--scene") && i + 1 < argc) want = argv[++i];
        else if (!strcmp(argv[i], "--width")) width = arg_int(argc, argv, &i, "--width");
        else if (!strcmp(argv[i], "--height")) height = arg_int(argc, argv, &i, "--height");
        else if (!strcmp(argv[i], "--samples")) samples = arg_int(argc, argv, &i, "--samples");
        else if (!strcmp(argv[i], "--depth")) depth = arg_int(argc, argv, &i, "--depth");
        else if (!strcmp(argv[i], "--threads")) threads = arg_int(argc, argv, &i, "--threads");
        else if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i];
        else if (!strcmp(argv[i], "--list")) {
            for (int k = 0; k < table_count; k++)
                printf("  %-8s %d spheres\n", table[k].name, table[k].count);
            return 0;
        } else if (!strcmp(argv[i], "--help") || !strcmp(argv[i], "-h")) {
            usage(argv[0]);
            return 0;
        } else {
            fprintf(stderr, "unknown option %s (try --help)\n", argv[i]);
            return 2;
        }
    }

    const entry *chosen = NULL;
    for (int k = 0; k < table_count; k++)
        if (!strcmp(table[k].name, want)) chosen = &table[k];
    if (!chosen) {
        fprintf(stderr, "no scene named %s (try --list)\n", want);
        return 2;
    }
    if (width < 1 || height < 1 || samples < 1 || depth < 1) {
        fprintf(stderr, "width, height, samples and depth must all be positive\n");
        return 2;
    }

    char fallback[256];
    if (!out_path) {
        snprintf(fallback, sizeof fallback, "out/%s.%s", chosen->name, image_default_extension());
        out_path = fallback;
    }

    image *im = image_new(width, height);
    if (!im) {
        fprintf(stderr, "cannot allocate a %dx%d image\n", width, height);
        return 1;
    }

    scene sc = {chosen->spheres, chosen->count, chosen->background};
    camera cam = camera_make(chosen->look_from, chosen->look_at, v3(0, 1, 0), chosen->fov,
                             (double)width / (double)height);

    render_job job = {&sc, &cam, im->rgb, width, height, samples, depth, 0, threads};

    printf("  %-8s%-40s%dx%d\n", "render", chosen->name, width, height);
    printf("  %-8s%s\n", "",
           RAYTRACER_NAME ", built with " RT_COMPILER);
    fflush(stdout);

    double t0 = now_seconds();
    int used = render_run(&job);
    double elapsed = now_seconds() - t0;

    double rays = (double)width * (double)height * (double)samples;
    printf("  %-8s%d spheres, %d samples, depth %d\n", "", chosen->count, samples, depth);
    printf("  %-8s%-40s%.2f s\n", "",
           used > 1 ? "threads" : "single threaded", elapsed);
    printf("  %-8s%-40s%.2f M paths/s\n", "", "", rays / elapsed / 1e6);

    ensure_parent(out_path);

    int rc;
    const char *kind;
#ifdef HAVE_ZLIB
    rc = image_write_png(im, out_path);
    kind = "png";
    if (rc != 0) {
        rc = image_write_ppm(im, out_path);
        kind = "ppm";
    }
#else
    rc = image_write_ppm(im, out_path);
    kind = "ppm";
#endif
    image_free(im);

    if (rc != 0) {
        fprintf(stderr, "cannot write %s\n", out_path);
        return 1;
    }

    long bytes = file_size(out_path);
    printf("  %-8s%-40s%ld KB (%s)\n", "wrote", out_path, bytes / 1024, kind);
    (void)used;
    return 0;
}
