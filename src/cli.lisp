;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <anthony@atgreen.org>
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

;;; ------------------------------------------------------------------
;;; Subcommands.

(defparameter +usage+
  "takesy -- screen recorder for modern Linux desktops, Wayland & X11 (Common Lisp)

Usage:
  takesy [options]           Capture your screen -> auto-zoom -> mp4.
  takesy help                Show this help.

options:
  --output PATH    output mp4            (default /tmp/takesy-record.mp4)
  --duration S     max seconds (safety)  (default 30)
  --fps    N       output frames/sec     (default 24; static stretches hold)
  --height N        max output height, px (default 1200; never upscales)
  --bg     COLOR    background: #RRGGBB or a name (black/white/dark/navy/...)
  Pops a screen-share dialog; click GNOME's Stop button (top bar) to finish. The
  cursor hides during capture (auto-zoom needs its position), restored on exit.
")

(defun cmd-record (args)
  (let* ((o     (parse-kv args))
         (out   (opt o "output" "/tmp/takesy-record.mp4"))
         (dur   (opt-num o "duration" 30.0))
         (fps   (opt-int o "fps" 24))
         (height (opt-int o "height" 1200))
         (bg-str (opt o "bg" nil))
         (bg    (if bg-str (parse-color bg-str) '(0.11 0.12 0.15))))
    (format t "takesy: recording up to ~,0Fs @ ~Dfps, up to ~Dp tall -> ~A~%" dur fps height out)
    (format t "  a screen-share dialog will appear -- pick a source.~%~
                 click GNOME's Stop button (top bar) to finish; the cursor hides~%~
                 during capture (auto-zoom needs it) and is restored on exit.~%")
    (multiple-value-bind (path n)
        (rec:record-to-mp4 :duration dur :fps fps :max-height height :bg bg :out out)
      (format t "done: wrote ~A (~D frames)~%" path n)
      path)))

(defun run (args)
  "Dispatch ARGS (the command-line minus argv0). Recording is the default action,
so `takesy [options]` records; `help` is the only named subcommand (`record` is
still accepted). Return normally on success; signal on failure. Separate from
MAIN so it is REPL-callable."
  (let ((cmd (first args)))
    (cond
      ((member cmd '("help" "--help" "-h") :test #'equal) (write-string +usage+) nil)
      ((equal cmd "record") (cmd-record (rest args)))  ; still accepted, but optional
      (t (cmd-record args)))))                          ; default: just record

;;; ------------------------------------------------------------------
;;; Executable entry point (set as :toplevel by build.lisp).

(defun main ()
  (handler-case
      (progn (run (rest sb-ext:*posix-argv*))
             (finish-output)
             (sb-ext:exit :code 0))
    (error (e)
      (format *error-output* "takesy: ~A~%" e)
      (finish-output *error-output*)
      (sb-ext:exit :code 1))))
