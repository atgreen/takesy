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
    (asdf:load-system "green-screen")
  (error (e)
    (format *error-output* "~&Failed to load green-screen: ~A~%~
                            Ensure deps are installed (ocicl install dbus cffi flexi-streams).~%" e)
    (uiop:quit 1)))

(load (merge-pathnames "spike/portal-screencast.lisp" (uiop:getcwd)))

(handler-case
    (green-screen/spike:run)
  (error (e)
    (format *error-output* "~&Spike error: ~A~%" e)
    (uiop:quit 1)))

(uiop:quit 0)
