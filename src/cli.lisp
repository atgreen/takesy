;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;; cli.lisp
;;;;
;;;; The `takesy` command-line entry point. Recording is the default action:
;;;; `takesy [options]` captures the screen, auto-zooms, and writes an mp4
;;;; (via takesy/record); `help` prints usage.

(defpackage #:takesy/cli
  (:use #:cl)
  (:local-nicknames (#:rec #:takesy/record))
  (:export #:main #:run))

(in-package #:takesy/cli)

;;; ------------------------------------------------------------------
;;; Tiny argument parser: `--key value` pairs into an alist. Dependency-free,
;;; matching the project's hand-rolled ethos.

(defun parse-kv (args)
  "Parse ARGS as `--key value` pairs into an alist of (downcased-key . string).
Signal a usage error on a malformed option."
  (loop for (k v) on args by #'cddr
        do (unless (and (stringp k) (>= (length k) 3) (string= (subseq k 0 2) "--"))
             (error "bad option ~S (expected --key value)" k))
           (unless v (error "option ~A needs a value" k))
        collect (cons (string-downcase (subseq k 2)) v)))

(defun opt (alist key default)
  (let ((cell (assoc key alist :test #'string=)))
    (if cell (cdr cell) default)))

(defun opt-int (alist key default)
  (let ((v (opt alist key nil))) (if v (parse-integer v) default)))

(defun opt-num (alist key default)
  "Parse a real number option safely (no read-eval)."
  (let ((v (opt alist key nil)))
    (if v
        (let ((*read-eval* nil))
          (let ((n (read-from-string v)))
            (unless (realp n) (error "~A expects a number, got ~S" key v))
            (float n 1.0)))
        default)))

(defparameter +color-names+
  '(("black" 0.0 0.0 0.0) ("white" 1.0 1.0 1.0)
    ("gray" 0.5 0.5 0.5) ("grey" 0.5 0.5 0.5)
    ("dark" 0.11 0.12 0.15) ("charcoal" 0.13 0.13 0.14)
    ("navy" 0.08 0.11 0.20) ("slate" 0.16 0.19 0.24)))

(defun parse-color (str)
  "Parse a background colour: `#RRGGBB` / `RRGGBB` hex, or a name (black, white,
gray, dark, navy, slate, ...). Return (r g b) in 0..1."
  (let ((named (assoc (string-downcase (string-trim " " str)) +color-names+
                      :test #'string=)))
    (cond
      (named (rest named))
      (t (let ((s (string-left-trim "#" (string-trim " " str))))
           (unless (and (= (length s) 6) (every (lambda (c) (digit-char-p c 16)) s))
             (error "bad --bg colour ~S (use #RRGGBB or a name like dark/navy/black)" str))
           (flet ((h (a b) (/ (parse-integer s :start a :end b :radix 16) 255.0)))
             (list (h 0 2) (h 2 4) (h 4 6))))))))

(defun parse-xy (str)
  "Parse `X,Y` (two reals) into a (cons x . y). Used for --cursor-hotspot."
  (let ((c (position #\, str)))
    (unless c (error "expected X,Y (comma-separated), got ~S" str))
    (flet ((num (s) (let ((*read-eval* nil))
                      (let ((n (read-from-string s)))
                        (unless (realp n) (error "~S is not a number" s))
                        (float n 1.0)))))
      (cons (num (subseq str 0 c)) (num (subseq str (1+ c)))))))

(defun parse-audio (str)
  "Parse --audio into a source mode: system/desktop/monitor -> :system,
mic/microphone -> :mic, both/on/mix -> :both, off/none or absent -> NIL."
  (if (null str)
      nil
      (let ((s (string-downcase (string-trim " " str))))
        (cond
          ((member s '("off" "none" "no") :test #'string=) nil)
          ((member s '("system" "desktop" "monitor") :test #'string=) :system)
          ((member s '("mic" "microphone" "input") :test #'string=) :mic)
          ((member s '("both" "on" "yes" "all" "mix") :test #'string=) :both)
          (t (error "bad --audio ~S (use system, mic, both, or off)" str))))))

(defun parse-aspect (str)
  "Parse --aspect `W:H` (e.g. 9:16, 1:1, 16:9) into a (cons w . h) of positive
integers, or NIL when absent."
  (if (null str)
      nil
      (let ((c (position #\: str)))
        (unless c (error "bad --aspect ~S (use W:H, e.g. 9:16, 1:1, 16:9)" str))
        (let ((w (parse-integer str :end c :junk-allowed nil))
              (h (parse-integer str :start (1+ c) :junk-allowed nil)))
          (unless (and (plusp w) (plusp h))
            (error "bad --aspect ~S (W and H must be positive)" str))
          (cons w h)))))

;;; ------------------------------------------------------------------
;;; Subcommands.

(defparameter +usage+
  "takesy -- screen recorder for modern Linux desktops, Wayland & X11

Usage:
  takesy [options]           Capture your screen -> auto-zoom -> mp4.
  takesy capture [options]   Capture only; save to a dir for later rendering.
  takesy render DIR [options]  Render a captured dir -> mp4 (re-run with any tuning).
  takesy help                Show this help.

Capture once, render many: `takesy capture` writes a self-contained recording dir
(frames + manifest); `takesy render DIR` runs the auto-zoom/composite over it, so
you can try different --bg / --zoom / cursor tuning without re-recording.

options:
  --output PATH    output file           (default /tmp/takesy-record.mp4;
                                          a .gif path makes an animated GIF, no audio)
  --dir    PATH    recording dir         (default /tmp/takesy-rec; capture/record)
  --duration S     max seconds (safety)  (default 30; capture/record)
  --fps    N       output frames/sec     (default 24; static stretches hold)
  --height N        max output height, px (default 1200; never upscales)
  --bg     COLOR    background: #RRGGBB, a name (black/white/dark/navy/...),
                    or `blur` (a frosted, blurred copy of the screen)
  --bg-image PATH   background image (png/...), cover-fit behind the inset;
                    overrides --bg
  --aspect W:H      reframe output to an aspect (e.g. 9:16 vertical, 1:1),
                    keeping the active region centered (default: content aspect)
  --corner-radius F rounded-corner radius, fraction of content (default 0.09;
                    0 = square corners)
  --cursor PATH     draw a custom cursor image (png/...) instead of the arrow
  --cursor-hotspot X,Y  click point as a fraction of the image (default 0,0 = top-left)
  --cursor-size F   cursor height as a fraction of output height (default 0.06)
  --audio  MODE     record audio (capture/record): system (desktop), mic, or both
                    (default: off). Muxed into the mp4; survives capture->render.

direction tuning (auto-zoom + cursor feel):
  --zoom   F              punch-in zoom factor for activity     (default 1.8)
  --zoom-merge-gap S      idle gap below which zoom pans instead
                          of zooming out and back in            (default 2.5)
  --cursor-omega-fast R   cursor-spring stiffness when moving
                          fast -- higher = snappier, less lag   (default 30.0)
  --cursor-anticipate S   seconds before a click to start aiming
                          the cursor straight at it             (default 0.4)

  Pops a screen-share dialog; click GNOME's Stop button (top bar) to finish. The
  cursor hides during capture (auto-zoom needs its position), restored on exit.

takesy  Copyright (C) 2026  Anthony Green <green@moxielogic.com>
Distributed under the MIT license; this is free software with NO WARRANTY.
")

(defun %render-args (o)
  "Extract RENDER-RECORDING keyword args (direction tuning + output) from the
option alist O, applying defaults. Shared by `record` and `render`."
  (let* ((bg-str  (opt o "bg" nil))
         (bg-blur (and bg-str (string-equal (string-trim " " bg-str) "blur"))))
    (list :fps               (opt-int o "fps" 24)
          :max-height        (opt-int o "height" 1200)
          :bg-blur           bg-blur
          :bg                (if (and bg-str (not bg-blur))
                                 (parse-color bg-str) '(0.11 0.12 0.15))
          :bg-image          (opt o "bg-image" nil)
          :aspect            (parse-aspect (opt o "aspect" nil))
          :corner            (opt-num o "corner-radius" 0.09)
          :cursor            (opt o "cursor" nil)
          :cursor-hotspot    (let ((s (opt o "cursor-hotspot" nil)))
                               (if s (parse-xy s) '(0.0 . 0.0)))
          :cursor-size       (when (opt o "cursor-size" nil)
                               (opt-num o "cursor-size" 0.06))
          :zoom              (opt-num o "zoom" 1.8)
          :zoom-merge-gap    (opt-num o "zoom-merge-gap" 2.5)
          :cursor-omega-fast (opt-num o "cursor-omega-fast" 30.0)
          :cursor-anticipate (opt-num o "cursor-anticipate" 0.4)
          :out               (opt o "output" "/tmp/takesy-record.mp4"))))

(defun cmd-record (args)
  "Full pipeline: capture then render in one shot (the default action)."
  (let* ((o   (parse-kv args))
         (dur (opt-num o "duration" 30.0))
         (fps (opt-int o "fps" 24))
         (dir (opt o "dir" "/tmp/takesy-rec"))
         (audio (parse-audio (opt o "audio" nil)))
         (ra  (%render-args o)))
    (format t "takesy: recording up to ~,0Fs @ ~Dfps, up to ~Dp tall~@[ +audio(~(~A~))~] -> ~A~%"
            dur fps (getf ra :max-height) audio (getf ra :out))
    (format t "  a screen-share dialog will appear -- pick a source.~%~
                 click GNOME's Stop button (top bar) to finish; the cursor hides~%~
                 during capture (auto-zoom needs it) and is restored on exit.~%")
    (let ((rec (rec:capture-recording :duration dur :fps fps :dir dir :audio audio)))
      (multiple-value-bind (path n) (apply #'rec:render-recording rec ra)
        (format t "done: wrote ~A (~D frames)~%" path n)
        (format t "  re-render this capture with: takesy render ~A [--bg ... --zoom ...]~%" dir)
        path))))

(defun cmd-capture (args)
  "Capture stage only: record to DIR (frames + intermediate + manifest.sexp) so it
can be rendered -- and re-rendered with different config -- later."
  (let* ((o   (parse-kv args))
         (dur (opt-num o "duration" 30.0))
         (fps (opt-int o "fps" 24))
         (dir (opt o "dir" "/tmp/takesy-rec"))
         (audio (parse-audio (opt o "audio" nil))))
    (format t "takesy: capturing up to ~,0Fs @ ~Dfps~@[ +audio(~(~A~))~] -> ~A~%"
            dur fps audio dir)
    (format t "  a screen-share dialog will appear -- pick a source.~%~
                 click GNOME's Stop button (top bar) to finish.~%")
    (let ((rec (rec:capture-recording :duration dur :fps fps :dir dir :audio audio)))
      (format t "done: captured ~D frames to ~A~%" (length (getf rec :frames)) dir)
      (format t "  render it with: takesy render ~A [--output out.mp4 --bg ... --zoom ...]~%" dir)
      dir)))

(defun cmd-render (args)
  "Direct + composite stages only: render a previously-captured DIR to an mp4.
Re-runnable with different tuning without re-capturing."
  (let ((dir (first args)))
    (when (or (null dir)
              (and (>= (length dir) 2) (string= (subseq dir 0 2) "--")))
      (error "render needs a recording DIR: takesy render DIR [options]"))
    (let* ((ra (%render-args (parse-kv (rest args)))))
      (format t "takesy: rendering ~A -> ~A~%" dir (getf ra :out))
      (multiple-value-bind (path n) (apply #'rec:render-recording-dir dir ra)
        (format t "done: wrote ~A (~D frames)~%" path n)
        path))))

(defun run (args)
  "Dispatch ARGS (the command-line minus argv0). Recording is the default action,
so `takesy [options]` records; `capture` and `render` split the pipeline so one
capture can be rendered repeatedly with different config. `help` prints usage.
Return normally on success; signal on failure. Separate from MAIN so it is
REPL-callable."
  (let ((cmd (first args)))
    (cond
      ((member cmd '("help" "--help" "-h") :test #'equal) (write-string +usage+) nil)
      ((equal cmd "capture") (cmd-capture (rest args)))
      ((equal cmd "render")  (cmd-render (rest args)))
      ((equal cmd "record")  (cmd-record (rest args)))  ; still accepted, but optional
      (t (cmd-record args)))))                          ; default: just record

;;; ------------------------------------------------------------------
;;; Executable entry point (the :entry-point of the "takesy" system; `make`
;;; dumps the image via asdf:make).

(defun main ()
  (handler-case
      (progn (run (rest sb-ext:*posix-argv*))
             (finish-output)
             (sb-ext:exit :code 0))
    (error (e)
      (format *error-output* "takesy: ~A~%" e)
      (finish-output *error-output*)
      (sb-ext:exit :code 1))))
