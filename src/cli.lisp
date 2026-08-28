;;;; cli.lisp
;;;;
;;;; Bead green-screen-am4.5: the `takesy` command-line entry point. For now it
;;;; exposes `takesy demo`, which renders the synthetic auto-zoom pipeline
;;;; (Director -> compositor) to an mp4 -- a runnable binary that de-risks the
;;;; save-lisp-and-die packaging before real capture (`takesy record`) lands.

(defpackage #:takesy/cli
  (:use #:cl)
  (:local-nicknames (#:dir #:takesy/director) (#:demo #:takesy/demo))
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
  "takesy -- Wayland-native screen recorder (Common Lisp)

Usage:
  takesy demo [options]      Render the synthetic auto-zoom demo to an mp4.
  takesy help                Show this help.

demo options:
  --output PATH    output mp4            (default /tmp/takesy-director.mp4)
  --width  W       render width, even    (default 480)
  --height H       render height, even   (default 300)
  --fps    N       frames per second     (default 30)
  --zoom   Z       punch-in zoom factor  (default 2.0)

Real capture ('takesy record') is coming -- see bead green-screen-am4.
")

(defun cmd-demo (args)
  (let* ((o    (parse-kv args))
         (out  (opt o "output" "/tmp/takesy-director.mp4"))
         (w    (opt-int o "width" 480))
         (h    (opt-int o "height" 300))
         (fps  (opt-int o "fps" 30))
         (zoom (opt-num o "zoom" 2.0)))
    (format t "takesy demo: ~Dx~D @ ~Dfps, zoom ~,1F -> ~A~%" w h fps zoom out)
    (let ((dir:*zoom-level* zoom))
      (multiple-value-bind (path n)
          (demo:director-demo :width w :height h :fps fps :path out)
        (format t "done: wrote ~A (~D frames)~%" path n)
        path))))

(defun run (args)
  "Dispatch ARGS (the command-line minus argv0). Return normally on success;
signal an error on failure. Kept separate from MAIN so it is REPL-callable."
  (let ((cmd (first args)))
    (cond
      ((or (null cmd) (member cmd '("help" "--help" "-h") :test #'string=))
       (write-string +usage+) nil)
      ((string= cmd "demo") (cmd-demo (rest args)))
      (t (error "unknown command ~S (try `takesy help`)" cmd)))))

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
