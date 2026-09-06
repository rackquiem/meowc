#!/usr/bin/env bash
source "$(dirname "${BASH_SOURCE[0]}")/record-lib.sh"

GEOMETRY=${GEOMETRY:-1280x720}
SRC=${SRC:-$HOME/projects/abnormal}
OUT="$ROOT/assets/compiler.mp4"
WORK=$(mktemp -d)

cp -a "$SRC" "$WORK/abnormal"
rm -rf "$WORK/abnormal/build" "$WORK/abnormal/gen"
mkdir -p "$WORK/bin"
ln -sf "$ROOT/meowc" "$WORK/bin/meowc"

start_xvfb "$GEOMETRY"

kitty \
    --config "$HOME/.config/kitty/kitty.conf" \
    -o remember_window_size=no \
    -o "initial_window_width=${GEOMETRY%x*}" \
    -o "initial_window_height=${GEOMETRY#*x}" \
    -o confirm_os_window_close=0 \
    --directory "$WORK/abnormal" \
    -- bash --noprofile --norc >/dev/null 2>&1 &
sleep 3

for _ in $(seq 30); do
    xdotool search --onlyvisible --class kitty >/dev/null 2>&1 && break
    sleep 1
done
xdotool search --onlyvisible --class kitty windowactivate >/dev/null 2>&1 || true
xdotool type --delay 10 -- 'export PS1="expr $ " PATH='"$WORK"'/bin:$PATH; clear'
xdotool key Return
sleep 1

start_capture "$GEOMETRY" "$WORK/compiler.mkv"

send_line 'cat programs/poly.expr programs/folded.expr' 2.2
send_line 'meowc build' 4
send_line 'meowc test' 4
send_line 'clear' 0.6
send_line './build/bin/vm --list' 3.5
send_line './build/bin/vm poly 5' 3.5
send_line 'objdump -d -M intel --no-show-raw-insn build/bin/vm | sed -n "/prog_folded/,/ret/p"' 3
send_line 'objdump -d -M intel --no-show-raw-insn build/bin/vm | sed -n "/prog_poly/,/ret/p"' 4.5

stop_capture

LIMIT=${LIMIT:-29.5}
raw=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$WORK/compiler.mkv")
speed=$(python3 -c "print(max(1.0, $raw / $LIMIT))")
ffmpeg -v error -y -i "$WORK/compiler.mkv" -an -vf "setpts=PTS/$speed" \
    -c:v libx264 -crf 20 -preset slow -pix_fmt yuv420p -movflags +faststart "$OUT"
echo "raw ${raw}s, sped up ${speed}x"
rm -rf "$WORK"
echo "wrote $OUT"
