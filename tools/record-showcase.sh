#!/usr/bin/env bash
source "$(dirname "${BASH_SOURCE[0]}")/record-lib.sh"

GEOMETRY=${GEOMETRY:-1280x720}
EXAMPLE="$ROOT/examples/raytracer"
OUT="$ROOT/assets/showcase.mp4"
WORK=$(mktemp -d)
PROMPT_FILE="$WORK/prompt"

cp -a "$EXAMPLE" "$WORK/raytracer"
rm -rf "$WORK/raytracer/build" "$WORK/raytracer/gen" "$WORK/raytracer/out"
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
sleep 3

for _ in $(seq 30); do
    xdotool search --onlyvisible --class kitty >/dev/null 2>&1 && break
    sleep 1
done
xdotool search --onlyvisible --class kitty windowactivate >/dev/null 2>&1 || true
xdotool type --delay 10 -- 'export PS1="raytracer $ " PROMPT_COMMAND="printf . >> '"$PROMPT_FILE"'" PATH='"$WORK"'/bin:$PATH; clear'
xdotool key Return
sleep 1

start_capture "$GEOMETRY" "$WORK/showcase.mkv"

run_line 'meowc targets' 2.4
run_line 'meowc build' 1.6
run_line 'meowc test' 2.2
run_line 'clear' 0.4
run_line 'touch src/*/*.c include/*.h' 0.5
run_line 'meowc build' 2.0
run_line 'echo "void rt_scene_extra(void);" >> include/rt_scene.h' 0.5
run_line 'meowc build' 2.6

stop_capture

LIMIT=${LIMIT:-45}
raw=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$WORK/showcase.mkv")
speed=$(python3 -c "print(max(1.0, $raw / $LIMIT))")
ffmpeg -v error -y -i "$WORK/showcase.mkv" -an -vf "setpts=PTS/$speed" \
    -c:v libx264 -crf 20 -preset slow -pix_fmt yuv420p -movflags +faststart "$OUT"
echo "raw ${raw}s, sped up ${speed}x"
rm -rf "$WORK"
echo "wrote $OUT"
