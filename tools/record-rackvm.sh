#!/usr/bin/env bash
source "$(dirname "${BASH_SOURCE[0]}")/record-lib.sh"

GEOMETRY=${GEOMETRY:-1280x720}
SRC=${SRC:-$HOME/projects/xmm0club/rackvm}
OUT="$ROOT/assets/rackvm.mp4"
WORK=$(mktemp -d)
PROMPT_FILE="$WORK/prompt"

cp -a "$SRC" "$WORK/rackvm"
rm -rf "$WORK/rackvm/build" "$WORK/rackvm/.git"
mkdir -p "$WORK/bin"
ln -sf "$ROOT/meowc" "$WORK/bin/meowc"

start_xvfb "$GEOMETRY"

kitty \
    --config "$HOME/.config/kitty/kitty.conf" \
    -o remember_window_size=no \
    -o "initial_window_width=${GEOMETRY%x*}" \
    -o "initial_window_height=${GEOMETRY#*x}" \
    -o confirm_os_window_close=0 \
    --directory "$WORK/rackvm" \
    -- bash --noprofile --norc >/dev/null 2>&1 &
sleep 3

for _ in $(seq 30); do
    xdotool search --onlyvisible --class kitty >/dev/null 2>&1 && break
    sleep 1
done
xdotool search --onlyvisible --class kitty windowactivate >/dev/null 2>&1 || true
xdotool type --delay 10 -- 'export PS1="rackvm $ " PROMPT_COMMAND="printf . >> '"$PROMPT_FILE"'" PATH='"$WORK"'/bin:$PATH; clear'
xdotool key Return
sleep 1

start_capture "$GEOMETRY" "$WORK/rackvm.mkv"

run_line 'meowc targets' 2.2
run_line 'meowc build' 1.8
run_line 'meowc test' 1.8
run_line 'meowc run check' 2.2
run_line 'meowc --prefix /tmp/stage install' 2.2
run_line 'clear' 0.4
run_line 'touch src/*.c include/*.h tests/*.c' 0.5
run_line 'meowc build' 2.0
run_line 'echo "void rackvm_unused_decl(void);" >> include/rackvm.h' 0.5
run_line 'meowc build' 2.6

stop_capture

LIMIT=${LIMIT:-45}
raw=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$WORK/rackvm.mkv")
speed=$(python3 -c "print(max(1.0, $raw / $LIMIT))")
ffmpeg -v error -y -i "$WORK/rackvm.mkv" -an -vf "setpts=PTS/$speed" \
    -c:v libx264 -crf 20 -preset slow -pix_fmt yuv420p -movflags +faststart "$OUT"
echo "raw ${raw}s, sped up ${speed}x"
rm -rf "$WORK"
echo "wrote $OUT"
