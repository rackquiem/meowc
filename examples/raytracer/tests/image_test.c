#include "rt_config.h"
#include "rt_image.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int failures = 0;

static void check(const char *what, int cond) {
    if (!cond) {
        fprintf(stderr, "  %s: failed\n", what);
        failures++;
    }
}

static long read_all(const char *path, unsigned char **out) {
    FILE *f = fopen(path, "rb");
    if (!f) return -1;
    fseek(f, 0, SEEK_END);
    long n = ftell(f);
    fseek(f, 0, SEEK_SET);
    unsigned char *buf = malloc((size_t)n);
    if (!buf || fread(buf, 1u, (size_t)n, f) != (size_t)n) {
        free(buf);
        fclose(f);
        return -1;
    }
    fclose(f);
    *out = buf;
    return n;
}

static unsigned long be32(const unsigned char *p) {
    return ((unsigned long)p[0] << 24) | ((unsigned long)p[1] << 16) | ((unsigned long)p[2] << 8) | p[3];
}

int main(void) {
    image *im = image_new(17, 11);
    check("the image allocates", im != NULL);
    if (!im) return 1;

    check("width is kept", im->width == 17);
    check("the buffer starts black", im->rgb[0] == 0 && im->rgb[17 * 11 * 3 - 1] == 0);

    for (int y = 0; y < im->height; y++)
        for (int x = 0; x < im->width; x++) {
            unsigned char *px = im->rgb + ((size_t)y * 17 + (size_t)x) * 3;
            px[0] = (unsigned char)(x * 15);
            px[1] = (unsigned char)(y * 23);
            px[2] = 128;
        }

    check("a zero sized image is refused", image_new(0, 4) == NULL);

    const char *ppm = "test-out.ppm";
    check("ppm writes", image_write_ppm(im, ppm) == 0);
    unsigned char *buf = NULL;
    long n = read_all(ppm, &buf);
    check("ppm has a header and every pixel", n == (long)(strlen("P6\n17 11\n255\n") + 17 * 11 * 3));
    check("ppm starts with the magic", n > 2 && buf[0] == 'P' && buf[1] == '6');
    free(buf);
    remove(ppm);

#ifdef HAVE_ZLIB
    const char *png = "test-out.png";
    check("png writes", image_write_png(im, png) == 0);
    n = read_all(png, &buf);
    check("png is not empty", n > 8);
    if (n > 8) {
        static const unsigned char sig[8] = {137, 80, 78, 71, 13, 10, 26, 10};
        check("png signature", memcmp(buf, sig, 8) == 0);
        check("first chunk is IHDR", memcmp(buf + 12, "IHDR", 4) == 0);
        check("IHDR carries the width", be32(buf + 16) == 17);
        check("IHDR carries the height", be32(buf + 20) == 11);
        check("eight bits per channel", buf[24] == 8);
        check("truecolour", buf[25] == 2);
        check("last chunk is IEND", memcmp(buf + n - 8, "IEND", 4) == 0);
    }
    free(buf);
    remove(png);
    check("the default extension follows zlib", strcmp(image_default_extension(), "png") == 0);
#else
    check("without zlib the default is ppm", strcmp(image_default_extension(), "ppm") == 0);
#endif

    image_free(im);
    image_free(NULL); /* must tolerate a null pointer */

    if (failures == 0) printf("  image: every assertion held\n");
    return failures == 0 ? 0 : 1;
}
