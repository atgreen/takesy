;;;; keyframe.lisp
;;;;
;;;; Bead green-screen-7k8.3: the Director keyframe -- the contract between the
;;;; DIRECTOR (green-screen-wtd, decides motion from the cursor/input track) and
;;;; the COMPOSITOR (green-screen-7k8, renders it). Deliberately dependency-free
;;;; so both systems can share it.
;;;;
;;;; Coordinates: CENTER-X/CENTER-Y are the zoom focal point in source texture
;;;; UV space, 0..1, with (0,0) at the first pixel row/col. ZOOM is >= 1 (1.0 =
;;;; whole frame, 2.0 = punch in to half width/height). The later fields drive
;;;; M4/M5 (background inset, rounded corners, drop shadow).

(defpackage #:green-screen/keyframe
  (:use #:cl)
  (:nicknames #:gs-kf)
  (:export #:keyframe #:make-keyframe #:copy-keyframe #:keyframe-p
           #:keyframe-time #:keyframe-zoom
           #:keyframe-center-x #:keyframe-center-y
           #:keyframe-corner-radius #:keyframe-shadow-blur #:keyframe-shadow-alpha
           #:keyframe-bg-color
           #:clamp-center #:effective-center))

(in-package #:green-screen/keyframe)

(defstruct keyframe
  (time 0.0)                     ; seconds
  (zoom 1.0)                     ; >= 1.0
  (center-x 0.5) (center-y 0.5)  ; focal point, source UV space 0..1
  (corner-radius 0.0)            ; M4: rounded-rect radius (fraction of min dim)
  (shadow-blur 0.0)              ; M5: shadow softness
  (shadow-alpha 0.0)             ; M5: shadow opacity 0..1
  (bg-color '(0.0 0.0 0.0)))     ; M4: background (r g b), 0..1

(defun clamp-center (c zoom)
  "Clamp a normalized center coord so the sampled window [c-0.5/zoom, c+0.5/zoom]
stays fully inside [0,1]. At zoom<=1 the window is the whole frame -> 0.5."
  (if (<= zoom 1.0)
      0.5
      (let ((half (/ 0.5 zoom)))
        (max half (min (- 1.0 half) (float c 1.0))))))

(defun effective-center (kf)
  "The clamped (center-x . center-y) the compositor should actually sample with,
so a keyframe never pans the zoom window off the edge of the source."
  (cons (clamp-center (keyframe-center-x kf) (keyframe-zoom kf))
        (clamp-center (keyframe-center-y kf) (keyframe-zoom kf))))
