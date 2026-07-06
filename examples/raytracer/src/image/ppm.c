#include "rt_image.h"
#include <stdio.h>

int image_write_ppm(const image *im, const char *path) {
    if (!im || !path) return -1;
    FILE *f = fopen(path, "wb");
    if (!f) return -1;

    fprintf(f, "P6\n%d %d\n255\n", im->width, im->height);
    size_t n = (size_t)im->width * (size_t)im->height * 3u;
    int ok = fwrite(im->rgb, 1u, n, f) == n;
    return (fclose(f) == 0 && ok) ? 0 : -1;
}
