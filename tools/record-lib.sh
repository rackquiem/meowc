#!/usr/bin/env bash

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
FONT=${FONT:-JetBrainsMono Nerd Font Mono}
FPS=${FPS:-12}

start_xvfb() {
    local geometry=$1
    for n in $(seq 90 120); do
        [ -e "/tmp/.X11-unix/X$n" ] && continue
        DISPLAY=":$n"
        Xvfb "$DISPLAY" -screen 0 "${geometry}x24" -nolisten tcp >/dev/null 2>&1 &
        XVFB_PID=$!
        export DISPLAY XVFB_PID
        for _ in $(seq 50); do
            xdotool getdisplaygeometry >/dev/null 2>&1 && return 0
            sleep 0.1
        done
        kill "$XVFB_PID" 2>/dev/null || true
    done
    echo "no free X display" >&2
    exit 1
}

start_capture() {
    local geometry=$1 out=$2
    ffmpeg -v error -y -f x11grab -draw_mouse 0 -framerate "$FPS" \
        -video_size "$geometry" -i "$DISPLAY" -codec:v libx264 -preset ultrafast \
        -qp 0 "$out" >/dev/null 2>&1 &
    FFMPEG_PID=$!
    export FFMPEG_PID
    sleep 1
}

stop_capture() {
    kill -INT "$FFMPEG_PID" 2>/dev/null || true
    wait "$FFMPEG_PID" 2>/dev/null || true
}

send_line() {
    local text=$1 pause=${2:-0.9}
    xdotool type --delay 42 -- "$text"
    sleep 0.35
    xdotool key Return
    sleep "$pause"
}

to_gif() {
    local src=$1 out=$2 fps=${3:-11}
    local palette=${src%.mkv}.png
    ffmpeg -v error -y -i "$src" -vf "fps=$fps,palettegen=stats_mode=diff" "$palette"
    ffmpeg -v error -y -i "$src" -i "$palette" \
        -lavfi "fps=$fps[x];[x][1:v]paletteuse=dither=none:diff_mode=rectangle" \
        -loop 0 "$out"
    rm -f "$palette"
}

cleanup() {
    kill "${FFMPEG_PID:-}" 2>/dev/null || true
    kill "${XVFB_PID:-}" 2>/dev/null || true
}
trap cleanup EXIT
