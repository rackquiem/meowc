#ifndef RT_IMAGE_H
#define RT_IMAGE_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    int width, height;
    unsigned char *rgb;
} image;

image *image_new(int width, int height);
void image_free(image *im);

/* Both return 0 on success. */
int image_write_ppm(const image *im, const char *path);
int image_write_png(const image *im, const char *path);

const char *image_default_extension(void);

#ifdef __cplusplus
}
#endif

#endif
