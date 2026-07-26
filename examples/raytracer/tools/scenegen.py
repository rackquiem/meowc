#!/usr/bin/env python3
"""Turn a .scene description into a C translation unit and its header.

usage: scenegen.py <input.scene> <output.c> <output.h>
"""
import os
import sys

KINDS = {"lambert": "MAT_LAMBERT", "metal": "MAT_METAL", "light": "MAT_LIGHT"}


def die(path, lineno, msg):
    sys.stderr.write("%s:%d: %s\n" % (path, lineno, msg))
    raise SystemExit(1)


def parse(path):
    scene = {
        "background": (0.0, 0.0, 0.0),
        "look_from": (0.0, 0.0, 6.0),
        "look_at": (0.0, 0.0, 0.0),
        "fov": 40.0,
        "spheres": [],
    }
    with open(path) as fh:
        for lineno, raw in enumerate(fh, 1):
            line = raw.split("#", 1)[0].strip()
            if not line:
                continue
            word = line.split()
            head, rest = word[0], word[1:]
            try:
                if head == "background":
                    scene["background"] = tuple(float(v) for v in rest[:3])
                elif head == "camera":
                    scene["look_from"] = tuple(float(v) for v in rest[0:3])
                    scene["look_at"] = tuple(float(v) for v in rest[3:6])
                    scene["fov"] = float(rest[6])
                elif head == "sphere":
                    centre = tuple(float(v) for v in rest[0:3])
                    radius = float(rest[3])
                    kind = rest[4]
                    if kind not in KINDS:
                        die(path, lineno, "unknown material %r" % kind)
                    albedo = tuple(float(v) for v in rest[5:8])
                    fuzz = float(rest[8]) if len(rest) > 8 else 0.0
                    scene["spheres"].append((centre, radius, kind, albedo, fuzz))
                else:
                    die(path, lineno, "unknown directive %r" % head)
            except (IndexError, ValueError):
                die(path, lineno, "malformed %s line" % head)
    if not scene["spheres"]:
        die(path, 0, "scene declares no spheres")
    return scene


def v3(t):
    return "{%.6f, %.6f, %.6f}" % t


def main():
    if len(sys.argv) != 4:
        sys.stderr.write(__doc__)
        raise SystemExit(2)

    src, out_c, out_h = sys.argv[1], sys.argv[2], sys.argv[3]
    name = os.path.splitext(os.path.basename(src))[0]
    scene = parse(src)
    guard = ("%s_SCENE_H" % name).upper()

    for path in (out_c, out_h):
        d = os.path.dirname(path)
        if d:
            os.makedirs(d, exist_ok=True)

    with open(out_h, "w") as fh:
        fh.write("/* Generated from %s by scenegen.py. */\n" % src)
        fh.write("#ifndef %s\n#define %s\n\n#include \"rt_scene.h\"\n\n" % (guard, guard))
        fh.write("extern const sphere %s_spheres[];\n" % name)
        fh.write("extern const int %s_sphere_count;\n" % name)
        fh.write("extern const vec3 %s_background;\n" % name)
        fh.write("extern const vec3 %s_look_from;\n" % name)
        fh.write("extern const vec3 %s_look_at;\n" % name)
        fh.write("extern const double %s_fov;\n\n#endif\n" % name)

    with open(out_c, "w") as fh:
        fh.write("/* Generated from %s by scenegen.py. */\n" % src)
        fh.write("#include \"%s_scene.h\"\n\n" % name)
        fh.write("const sphere %s_spheres[] = {\n" % name)
        for centre, radius, kind, albedo, fuzz in scene["spheres"]:
            fh.write("    {%s, %.6f, {%s, %s, %.6f}},\n"
                     % (v3(centre), radius, KINDS[kind], v3(albedo), fuzz))
        fh.write("};\n\n")
        fh.write("const int %s_sphere_count = %d;\n" % (name, len(scene["spheres"])))
        fh.write("const vec3 %s_background = %s;\n" % (name, v3(scene["background"])))
        fh.write("const vec3 %s_look_from = %s;\n" % (name, v3(scene["look_from"])))
        fh.write("const vec3 %s_look_at = %s;\n" % (name, v3(scene["look_at"])))
        fh.write("const double %s_fov = %.6f;\n" % (name, scene["fov"]))


if __name__ == "__main__":
    main()
