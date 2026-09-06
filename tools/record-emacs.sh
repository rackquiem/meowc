#!/usr/bin/env bash
source "$(dirname "${BASH_SOURCE[0]}")/record-lib.sh"

GEOMETRY=${GEOMETRY:-1800x1000}
OUT="$ROOT/assets/emacs.gif"
WORK=$(mktemp -d)

EXAMPLE="$ROOT/examples/raytracer"
HEADER="$EXAMPLE/include/rt_scene.h"
cp "$HEADER" "$WORK/rt_scene.h.orig"
restore_example() { cp "$WORK/rt_scene.h.orig" "$HEADER" 2>/dev/null || true; }
trap 'cleanup; restore_example' EXIT

mkdir -p "$WORK/bin"
ln -sf "$ROOT/meowc" "$WORK/bin/meowc"
(cd "$EXAMPLE" && "$ROOT/meowc" build >/dev/null 2>&1)

cat > "$WORK/demo.el" <<ELISP
;;; -*- lexical-binding: t; -*-
(require 'term)
(setq meowc-demo-dir "$EXAMPLE/")
(setenv "PATH" (concat "$WORK/bin:" (getenv "PATH")))

(defvar meowc-demo-build nil)
(defvar meowc-demo-header nil)
(defvar meowc-demo-term nil)

(global-set-key (kbd "<f9>")  (lambda () (interactive) (select-window meowc-demo-build)))
(global-set-key (kbd "<f10>") (lambda () (interactive) (select-window meowc-demo-header)))
(global-set-key (kbd "<f11>") (lambda () (interactive) (select-window meowc-demo-term)))

(defun meowc-demo-layout ()
  (set-frame-size (selected-frame) ${GEOMETRY%x*} ${GEOMETRY#*x} t)
  (set-frame-position (selected-frame) 0 0)
  (find-file (concat meowc-demo-dir "build.meow"))
  (redisplay t)
  (let* ((top (selected-window))
         (bottom (split-window top (round (* 0.60 (window-height top))) 'below))
         (right (split-window top (round (* 0.5 (window-width top))) 'right)))
    (setq meowc-demo-build top meowc-demo-header right meowc-demo-term bottom)
    (with-selected-window right
      (find-file (concat meowc-demo-dir "include/rt_scene.h"))
      (goto-char (point-min))
      (search-forward "vec3 ray_colour")
      (beginning-of-line)
      (recenter 8))
    (with-selected-window bottom
      (let ((default-directory meowc-demo-dir))
        (ansi-term "/bin/bash --noprofile --norc -i" "meowc"))
      (term-send-string (get-buffer-process "*meowc*")
                        "export PS1=\"raytracer \\\$ \"; clear\n"))
    (select-window top)
    (goto-char (point-min))))

(run-with-idle-timer 3 nil #'meowc-demo-layout)
ELISP

start_xvfb "$GEOMETRY"
emacs --load "$WORK/demo.el" >/dev/null 2>&1 &
EMACS_PID=$!
sleep 14
for _ in $(seq 30); do
    xdotool search --onlyvisible --class emacs >/dev/null 2>&1 && break
    sleep 1
done
xdotool search --onlyvisible --class emacs windowactivate >/dev/null 2>&1 || true
sleep 1

start_capture "$GEOMETRY" "$WORK/emacs.mkv"

for _ in $(seq 22); do xdotool key ctrl+n; sleep 0.1; done
sleep 1.5

xdotool key F10; sleep 1.2
xdotool key End; sleep 0.4
xdotool key Return; sleep 0.4
xdotool type --delay 55 -- 'int scene_sphere_count(const scene *s);'
sleep 1.2
xdotool key ctrl+x ctrl+s; sleep 1.8

xdotool key F11; sleep 1.2
send_line 'meowc build' 6
send_line 'meowc test' 5

sleep 2
stop_capture
kill "$EMACS_PID" 2>/dev/null || true

to_gif "$WORK/emacs.mkv" "$OUT" 11
restore_example
rm -rf "$WORK"
echo "wrote $OUT"
