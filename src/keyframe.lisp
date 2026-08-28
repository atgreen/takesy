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
           #:keyframe-padding
           #:keyframe-corner-radius #:keyframe-shadow-blur #:keyframe-shadow-alpha
           #:keyframe-bg-color
           #:clamp-center #:effective-center
           #:lerp #:ease-smoothstep #:lerp-keyframe #:sample-timeline))

(in-package #:green-screen/keyframe)

(defstruct keyframe
  (time 0.0)                     ; seconds
  (zoom 1.0)                     ; >= 1.0
  (center-x 0.5) (center-y 0.5)  ; focal point, source UV space 0..1
  (padding 0.0)                  ; M4: inset margin, fraction of min canvas dim
  (corner-radius 0.0)            ; M4: rounded-rect radius (fraction of min content dim)
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

;;; ------------------------------------------------------------------
;;; Timeline interpolation (shared with the Director, green-screen-wtd).

(defun lerp (a b e) (+ a (* (- b a) e)))

(defun ease-smoothstep (u)
  "Smoothstep ease on U in [0,1]: zero slope at both ends (no motion snap)."
  (let ((u (max 0.0 (min 1.0 (float u 1.0)))))
    (* u u (- 3.0 (* 2.0 u)))))

(defun lerp-keyframe (a b e)
  "Interpolate every field of keyframes A and B by eased fraction E in [0,1]."
  (flet ((k (fa fb) (lerp fa fb e)))
    (make-keyframe
     :time         (k (keyframe-time a) (keyframe-time b))
     :zoom         (k (keyframe-zoom a) (keyframe-zoom b))
     :center-x     (k (keyframe-center-x a) (keyframe-center-x b))
     :center-y     (k (keyframe-center-y a) (keyframe-center-y b))
     :padding      (k (keyframe-padding a) (keyframe-padding b))
     :corner-radius (k (keyframe-corner-radius a) (keyframe-corner-radius b))
     :shadow-blur  (k (keyframe-shadow-blur a) (keyframe-shadow-blur b))
     :shadow-alpha (k (keyframe-shadow-alpha a) (keyframe-shadow-alpha b))
     :bg-color     (mapcar (lambda (ca cb) (lerp ca cb e))
                           (keyframe-bg-color a) (keyframe-bg-color b)))))

(defun sample-timeline (keyframes time)
  "The interpolated keyframe at TIME. KEYFRAMES need not be pre-sorted. Clamps to
the endpoints outside the timeline; between two keyframes, eases with smoothstep."
  (let ((ks (sort (copy-list keyframes) #'< :key #'keyframe-time)))
    (cond
      ((null ks) (error "sample-timeline: empty timeline"))
      ((<= time (keyframe-time (first ks))) (first ks))
      ((>= time (keyframe-time (car (last ks)))) (car (last ks)))
      (t (loop for (a b) on ks
               when (and b (<= (keyframe-time a) time) (< time (keyframe-time b)))
                 do (let* ((span (- (keyframe-time b) (keyframe-time a)))
                           (u    (if (zerop span) 0.0
                                     (/ (- time (keyframe-time a)) span))))
                      (return (lerp-keyframe a b (ease-smoothstep u)))))))))
