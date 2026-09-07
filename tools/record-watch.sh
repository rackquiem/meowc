#!/usr/bin/env bash
source "$(dirname "${BASH_SOURCE[0]}")/record-lib.sh"

GEOMETRY=${GEOMETRY:-1280x720}
SRC=${SRC:-$HOME/projects/xmm0club/rackvm}
OUT="$ROOT/assets/watch.mp4"
WORK=$(mktemp -d)

cp -a "$SRC" "$WORK/rackvm"
rm -rf "$WORK/rackvm/build" "$WORK/rackvm/.git"
mkdir -p "$WORK/bin"
ln -sf "$ROOT/meowc" "$WORK/bin/meowc"
(cd "$WORK/rackvm" && PATH="$WORK/bin:$PATH" meowc -q build >/dev/null 2>&1)
tmux -L meowcrec kill-server 2>/dev/null || true
printf 'set -g status off\nset -g pane-active-border-style fg=colour240\nset -g pane-border-style fg=colour240\n' > "$WORK/tmux.conf"

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
xdotool type --delay 10 -- 'export PS1="rackvm $ " PATH='"$WORK"'/bin:$PATH; clear; tmux -L meowcrec -f '"$WORK"'/tmux.conf new-session -s d'
xdotool key Return
sleep 2
xdotool type --delay 10 -- 'export PS1="rackvm $ " PATH='"$WORK"'/bin:$PATH; clear'
xdotool key Return
sleep 1

start_capture "$GEOMETRY" "$WORK/watch.mkv"

send_line 'meowc watch' 3
xdotool key ctrl+b; sleep 0.4; xdotool key percent; sleep 1.2
xdotool type --delay 10 -- 'export PS1="rackvm $ " PATH='"$WORK"'/bin:$PATH; clear'
xdotool key Return
sleep 1

send_line 'echo "void rackvm_probe(void);" >> include/rackvm.h' 6
send_line 'sed -i /rackvm_probe/d include/rackvm.h' 6
send_line 'touch src/*.c include/*.h' 4

stop_capture

LIMIT=${LIMIT:-45}
raw=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$WORK/watch.mkv")
speed=$(python3 -c "print(max(1.0, $raw / $LIMIT))")
ffmpeg -v error -y -i "$WORK/watch.mkv" -an -vf "setpts=PTS/$speed" \
    -c:v libx264 -crf 20 -preset slow -pix_fmt yuv420p -movflags +faststart "$OUT"
echo "raw ${raw}s, sped up ${speed}x"
rm -rf "$WORK"
echo "wrote $OUT"
