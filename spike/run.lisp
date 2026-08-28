;;;; run.lisp -- load deps and run the portal ScreenCast spike.
;;;;
;;;;   sbcl --script spike/run.lisp
;;;;
;;;; `--script' skips ~/.sbclrc, so we bootstrap ocicl's source registry
;;;; ourselves to keep the runner self-contained.

(let ((rt #p"/home/green/.local/share/ocicl/ocicl-runtime.lisp"))
  (when (probe-file rt) (load rt)))

;; Resolve systems from the current working directory (where ocicl.csv lives).
(asdf:initialize-source-registry
 (list :source-registry (list :directory (uiop:getcwd)) :inherit-configuration))

(handler-case
    (asdf:load-system "takesy")
  (error (e)
    (format *error-output* "~&Failed to load takesy: ~A~%~
                            Ensure deps are installed (ocicl install dbus cffi flexi-streams).~%" e)
    (uiop:quit 1)))

(load (merge-pathnames "spike/portal-screencast.lisp" (uiop:getcwd)))

;; cursor_mode defaults to EMBEDDED (safe). Opt into METADATA -- which hides the
;; hardware cursor and exercises the cursor path -- with GS_CURSOR_MODE=4.
;; GS_RECORD=<seconds> records a clip (frame sequence + cursor track); it forces
;; METADATA so we get the cursor track, and the Session.Close teardown is armed.
;; Teardown runs either way; see AGENTS.md hazard #1.
(handler-case
    (let* ((rec-s (uiop:getenv "GS_RECORD"))
           (mode  (let ((s (uiop:getenv "GS_CURSOR_MODE")))
                    (cond (rec-s takesy/spike::+cursor-metadata+)
                          (s (parse-integer s))
                          (t takesy/spike::+cursor-embedded+)))))
      (if rec-s
          (takesy/spike:run
           :cursor-mode mode
           :record (list :duration (float (read-from-string rec-s) 1.0)
                         :fps (let ((f (uiop:getenv "GS_FPS")))
                                (if f (parse-integer f) 30))
                         :dir (or (uiop:getenv "GS_REC_DIR") "/tmp/takesy-rec")))
          (takesy/spike:run :cursor-mode mode)))
  (error (e)
    (format *error-output* "~&Spike error: ~A~%" e)
    (uiop:quit 1)))

(uiop:quit 0)
