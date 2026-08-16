#!/usr/bin/env bash

source "$(dirname "${BASH_SOURCE[0]}")/record-lib.sh"

GEOMETRY=${GEOMETRY:-1120x680}
EXAMPLE="$ROOT/examples/raytracer"
OUT="$ROOT/assets/kitty.gif"
WORK=$(mktemp -d)

git -C "$ROOT" stash list >/dev/null 2>&1 || true
cp -a "$EXAMPLE" "$WORK/raytracer"
rm -rf "$WORK/raytracer/build"
mkdir -p "$WORK/bin"
ln -sf "$ROOT/meowc" "$WORK/bin/meowc"

start_xvfb "$GEOMETRY"

kitty \
    --config "$HOME/.config/kitty/kitty.conf" \
    -o remember_window_size=no \
    -o "initial_window_width=${GEOMETRY%x*}" \
    -o "initial_window_height=${GEOMETRY#*x}" \
    -o confirm_os_window_close=0 \
    --directory "$WORK/raytracer" \
    -- bash --noprofile --norc >/dev/null 2>&1 &
KITTY_PID=$!
sleep 3

xdotool search --sync --onlyvisible --class kitty windowactivate >/dev/null 2>&1 || true
xdotool type --delay 10 -- 'export PS1="raytracer \$ " PATH='"$WORK"'/bin:$PATH; clear'
xdotool key Return
sleep 1

start_capture "$GEOMETRY" "$WORK/kitty.mkv"

send_line 'meowc build' 7
send_line '# Nothing changed' 0.6
send_line 'meowc build' 2.5
send_line '# Every mtime moves, not one byte does' 0.6
send_line 'touch src/*/*.c include/*.h' 0.8
send_line 'meowc build' 2.5
send_line '# Recolour a sphere in the scene description' 0.6
send_line "sed -i 's/0.22 0.42/0.20 0.70/' scenes/studio.scene" 0.8
send_line '# Codegen reruns, only what reads it rebuilds' 0.6
send_line 'meowc build' 3.5
send_line '# A comment only edit: recompiled, cascade stops' 0.6
send_line "sed -i '1i // early cutoff' src/math/vec3.c" 0.8
send_line 'meowc build' 3
send_line 'meowc test' 5

sleep 2
stop_capture
kill "$KITTY_PID" 2>/dev/null || true

to_gif "$WORK/kitty.mkv" "$OUT" 11
rm -rf "$WORK"
echo "wrote $OUT"
