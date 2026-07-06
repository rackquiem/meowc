#include "rt_config.h"
#include "rt_image.h"
#include <stdlib.h>

image *image_new(int width, int height) {
    if (width <= 0 || height <= 0) return NULL;
    image *im = (image *)malloc(sizeof *im);
    if (!im) return NULL;
    im->width = width;
    im->height = height;
    im->rgb = (unsigned char *)calloc((size_t)width * (size_t)height * 3u, 1u);
    if (!im->rgb) {
        free(im);
        return NULL;
    }
    return im;
}

void image_free(image *im) {
    if (!im) return;
    free(im->rgb);
    free(im);
}

const char *image_default_extension(void) {
#ifdef HAVE_ZLIB
    return "png";
#else
    return "ppm";
#endif
}
