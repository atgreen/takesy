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
  --scale  K       downsample output 1/K (default 3)
  Pops a screen-share dialog; click GNOME's Stop button (top bar) to finish. The
  cursor hides during capture (auto-zoom needs its position), restored on exit.
")

(defun cmd-record (args)
  (let* ((o     (parse-kv args))
         (out   (opt o "output" "/tmp/takesy-record.mp4"))
         (dur   (opt-num o "duration" 30.0))
         (fps   (opt-int o "fps" 24))
         (scale (opt-int o "scale" 3)))
    (format t "takesy: recording up to ~,0Fs @ ~Dfps, scale 1/~D -> ~A~%" dur fps scale out)
    (format t "  a screen-share dialog will appear -- pick a source.~%~
                 click GNOME's Stop button (top bar) to finish; the cursor hides~%~
                 during capture (auto-zoom needs it) and is restored on exit.~%")
    (multiple-value-bind (path n)
        (rec:record-to-mp4 :duration dur :fps fps :scale scale :out out)
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
