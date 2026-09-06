#!/usr/bin/env bash
source "$(dirname "${BASH_SOURCE[0]}")/record-lib.sh"

GEOMETRY=${GEOMETRY:-1280x720}
LUA=${LUA:-$HOME/projects/lua}
OUT="$ROOT/assets/showcase.mp4"
WORK=$(mktemp -d)

cp -a "$LUA" "$WORK/lua"
rm -rf "$WORK/lua/build"
mkdir -p "$WORK/bin"
ln -sf "$ROOT/meowc" "$WORK/bin/meowc"

start_xvfb "$GEOMETRY"

kitty \
    --config "$HOME/.config/kitty/kitty.conf" \
    -o remember_window_size=no \
    -o "initial_window_width=${GEOMETRY%x*}" \
    -o "initial_window_height=${GEOMETRY#*x}" \
    -o confirm_os_window_close=0 \
    --directory "$WORK/lua" \
    -- bash --noprofile --norc >/dev/null 2>&1 &
sleep 3

for _ in $(seq 30); do
    xdotool search --onlyvisible --class kitty >/dev/null 2>&1 && break
    sleep 1
done
xdotool search --onlyvisible --class kitty windowactivate >/dev/null 2>&1 || true
xdotool type --delay 10 -- 'export PS1="lua-5.4.7 $ " WINEDEBUG=-all LIBGL_ALWAYS_SOFTWARE=1 MESA_DEBUG=silent PATH='"$WORK"'/bin:$PATH; clear'
xdotool key Return
sleep 1

start_capture "$GEOMETRY" "$WORK/showcase.mkv"

send_line 'meowc build' 8.5
send_line './build/bin/lua -v' 1.8
send_line 'meowc --target x86_64-w64-mingw32 build' 13
send_line 'file build/x86_64-w64-mingw32/bin/lua.exe' 2.2
send_line 'wine build/x86_64-w64-mingw32/bin/lua.exe -v' 3.2

stop_capture

# Fit the cut to the target length rather than truncating the ending off it
LIMIT=${LIMIT:-29.5}
raw=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$WORK/showcase.mkv")
speed=$(python3 -c "print(max(1.0, $raw / $LIMIT))")
ffmpeg -v error -y -i "$WORK/showcase.mkv" -an -vf "setpts=PTS/$speed" \
    -c:v libx264 -crf 20 -preset slow -pix_fmt yuv420p -movflags +faststart "$OUT"
echo "raw ${raw}s, sped up ${speed}x"
rm -rf "$WORK"
echo "wrote $OUT"
