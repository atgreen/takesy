;;;; demo.lisp
;;;;
;;;; Bead green-screen-wtd.5: end-to-end demo that ties the two halves together --
;;;; the DIRECTOR (takesy/director) produces an auto-zoom keyframe timeline from a
;;;; session, and the COMPOSITOR (takesy/compositor) renders it to an mp4. Lives
;;;; in its own tiny system so neither half has to depend on the other; the
;;;; keyframe struct is the only shared contract.
;;;;
;;;; This proves the whole post-processing pipeline on a SYNTHETIC session + a
;;;; still source -- no portal capture, no live desktop. Swapping the synthetic
;;;; session for a real capture track and the still for a real frame sequence is
;;;; bead green-screen-7k8.7.

(defpackage #:takesy/demo
  (:use #:cl)
  (:local-nicknames (#:dir #:takesy/director) (#:comp #:takesy/compositor))
  (:export #:director-demo))

(in-package #:takesy/demo)

(defun director-demo (&key (width 480) (height 300) (fps 30)
                           (path "/tmp/takesy-director.mp4"))
  "Synthetic session -> Director auto-zoom timeline -> compositor render -> mp4.
Return (values path n-frames timeline). The Director's centers are normalized, so
the render resolution is independent of the session's screen size."
  (let* ((session  (dir:make-synthetic-session))
         (timeline (dir:plan-timeline session))
         (source   (comp:make-test-pattern width height))
         (duration (dir:session-duration session)))
    (multiple-value-bind (out n)
        (comp:render-timeline timeline source width height
                              :fps fps :duration duration :path path)
      (values out n timeline))))
