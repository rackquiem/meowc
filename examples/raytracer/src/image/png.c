#include "rt_config.h"
#include "rt_image.h"

#ifndef HAVE_ZLIB

int image_write_png(const image *im, const char *path) {
    (void)im;
    (void)path;
    return -1; /* built without zlib; callers fall back to PPM */
}

#else

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

static void put_be32(unsigned char *p, unsigned long v) {
    p[0] = (unsigned char)((v >> 24) & 0xFF);
    p[1] = (unsigned char)((v >> 16) & 0xFF);
    p[2] = (unsigned char)((v >> 8) & 0xFF);
    p[3] = (unsigned char)(v & 0xFF);
}

static int put_chunk(FILE *f, const char *type, const unsigned char *data, unsigned long len) {
    unsigned char header[8];
    put_be32(header, len);
    memcpy(header + 4, type, 4);
    if (fwrite(header, 1u, 8u, f) != 8u) return -1;
    if (len && fwrite(data, 1u, len, f) != len) return -1;

    unsigned long crc = crc32(0uL, (const Bytef *)type, 4u);
    if (len) crc = crc32(crc, (const Bytef *)data, (uInt)len);

    unsigned char tail[4];
    put_be32(tail, crc);
    return fwrite(tail, 1u, 4u, f) == 4u ? 0 : -1;
}

int image_write_png(const image *im, const char *path) {
    if (!im || !path) return -1;

    /* One filter byte (type 0, None) in front of every scanline. */
    size_t stride = (size_t)im->width * 3u;
    size_t raw_len = ((size_t)im->height) * (stride + 1u);
    unsigned char *raw = (unsigned char *)malloc(raw_len);
    if (!raw) return -1;

    for (int y = 0; y < im->height; y++) {
        unsigned char *dst = raw + (size_t)y * (stride + 1u);
        dst[0] = 0;
        memcpy(dst + 1, im->rgb + (size_t)y * stride, stride);
    }

    uLongf comp_len = compressBound((uLong)raw_len);
    unsigned char *comp = (unsigned char *)malloc(comp_len);
    if (!comp) {
        free(raw);
        return -1;
    }
    int zr = compress2(comp, &comp_len, raw, (uLong)raw_len, Z_BEST_COMPRESSION);
    free(raw);
    if (zr != Z_OK) {
        free(comp);
        return -1;
    }

    FILE *f = fopen(path, "wb");
    if (!f) {
        free(comp);
        return -1;
    }

    static const unsigned char sig[8] = {137, 80, 78, 71, 13, 10, 26, 10};
    int rc = fwrite(sig, 1u, 8u, f) == 8u ? 0 : -1;

    unsigned char ihdr[13];
    put_be32(ihdr, (unsigned long)im->width);
    put_be32(ihdr + 4, (unsigned long)im->height);
    ihdr[8] = 8;  /* bit depth */
    ihdr[9] = 2;  /* colour type: truecolour */
    ihdr[10] = 0; /* deflate */
    ihdr[11] = 0; /* adaptive filtering */
    ihdr[12] = 0; /* no interlace */

    if (rc == 0) rc = put_chunk(f, "IHDR", ihdr, sizeof ihdr);
    if (rc == 0) rc = put_chunk(f, "IDAT", comp, (unsigned long)comp_len);
    if (rc == 0) rc = put_chunk(f, "IEND", NULL, 0uL);

    free(comp);
    return (fclose(f) == 0 && rc == 0) ? 0 : -1;
}

#endif
