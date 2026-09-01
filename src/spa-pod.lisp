;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;; spa-pod.lisp
;;;;
;;;; Hand-assembled SPA PODs (bead green-screen-jme). SPA's spa_pod_builder_*
;;;; helpers are static-inline and not FFI-callable, so we build the binary
;;;; POD ourselves. Layout (all little-endian, bodies 8-byte aligned):
;;;;
;;;;   spa_pod            = { u32 size(body); u32 type }
;;;;   object body        = { u32 object_type; u32 object_id; prop* }
;;;;   prop               = { u32 key; u32 flags; spa_pod value }
;;;;   choice pod         = { u32 size; u32 type=Choice }
;;;;   choice body        = { u32 choice_type; u32 flags; spa_pod child; value* }
;;;;
;;;; A POD's `size' counts its body only (not the 8-byte header) but DOES count
;;;; inter-element padding. We pad every value to 8 bytes so the next element
;;;; starts aligned, and validate the whole thing against spa_debug_pod in C
;;;; (see src/val-pod.c) before trusting it.

(in-package #:takesy/pipewire)

;;; ------------------------------------------------------------------
;;; Byte buffer primitives.

(defun make-pod-buffer ()
  (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun put-u32 (buf x)
  (vector-push-extend (ldb (byte 8 0) x) buf)
  (vector-push-extend (ldb (byte 8 8) x) buf)
  (vector-push-extend (ldb (byte 8 16) x) buf)
  (vector-push-extend (ldb (byte 8 24) x) buf))

(defun set-u32 (buf pos x)
  (setf (aref buf pos)       (ldb (byte 8 0) x)
        (aref buf (+ pos 1)) (ldb (byte 8 8) x)
        (aref buf (+ pos 2)) (ldb (byte 8 16) x)
        (aref buf (+ pos 3)) (ldb (byte 8 24) x)))

(defun pad8 (buf)
  (loop until (zerop (mod (fill-pointer buf) 8))
        do (vector-push-extend 0 buf)))

;;; ------------------------------------------------------------------
;;; Value PODs (each writes a complete, 8-padded value).

(defun pod-id (buf value)
  (put-u32 buf 4) (put-u32 buf +spa-type-id+)
  (put-u32 buf value)
  (pad8 buf))

(defun pod-choice-enum-id (buf default alternatives)
  "Choice(Enum) over Id: DEFAULT then ALTERNATIVES."
  (let* ((nvals (1+ (length alternatives)))
         (size (+ 8 8 (* 4 nvals))))    ; choice-body(8) + child-hdr(8) + values
    (put-u32 buf size) (put-u32 buf +spa-type-choice+)
    (put-u32 buf +spa-choice-enum+) (put-u32 buf 0)   ; choice_type, flags
    (put-u32 buf 4) (put-u32 buf +spa-type-id+)        ; child: size=4, type=Id
    (put-u32 buf default)
    (dolist (a alternatives) (put-u32 buf a))
    (pad8 buf)))

(defun pod-choice-range-rect (buf dw dh minw minh maxw maxh)
  "Choice(Range) over Rectangle: default, min, max."
  (let ((size (+ 8 8 (* 8 3))))
    (put-u32 buf size) (put-u32 buf +spa-type-choice+)
    (put-u32 buf +spa-choice-range+) (put-u32 buf 0)
    (put-u32 buf 8) (put-u32 buf +spa-type-rectangle+)  ; child: size=8, Rectangle
    (put-u32 buf dw) (put-u32 buf dh)
    (put-u32 buf minw) (put-u32 buf minh)
    (put-u32 buf maxw) (put-u32 buf maxh)
    (pad8 buf)))

(defun pod-choice-range-fraction (buf dn dd minn mind maxn maxd)
  "Choice(Range) over Fraction: default, min, max."
  (let ((size (+ 8 8 (* 8 3))))
    (put-u32 buf size) (put-u32 buf +spa-type-choice+)
    (put-u32 buf +spa-choice-range+) (put-u32 buf 0)
    (put-u32 buf 8) (put-u32 buf +spa-type-fraction+)   ; child: size=8, Fraction
    (put-u32 buf dn) (put-u32 buf dd)
    (put-u32 buf minn) (put-u32 buf mind)
    (put-u32 buf maxn) (put-u32 buf maxd)
    (pad8 buf)))

(defun pod-int (buf value)
  (put-u32 buf 4) (put-u32 buf +spa-type-int+)
  (put-u32 buf value)
  (pad8 buf))

(defun pod-rectangle (buf w h)
  (put-u32 buf 8) (put-u32 buf +spa-type-rectangle+)
  (put-u32 buf w) (put-u32 buf h)
  (pad8 buf))

(defun pod-fraction (buf num den)
  (put-u32 buf 8) (put-u32 buf +spa-type-fraction+)
  (put-u32 buf num) (put-u32 buf den)
  (pad8 buf))

(defun pod-choice-range-int (buf default min max)
  (let ((size (+ 8 8 (* 4 3))))
    (put-u32 buf size) (put-u32 buf +spa-type-choice+)
    (put-u32 buf +spa-choice-range+) (put-u32 buf 0)
    (put-u32 buf 4) (put-u32 buf +spa-type-int+)   ; child: size=4, Int
    (put-u32 buf default) (put-u32 buf min) (put-u32 buf max)
    (pad8 buf)))

(defun pod-prop (buf key writer)
  "A property: key, flags=0, then the value POD written by WRITER."
  (put-u32 buf key) (put-u32 buf 0)
  (funcall writer))

(defun build-object-pod (object-type object-id prop-thunk)
  "Generic object POD: PROP-THUNK writes the props into the buffer."
  (let ((buf (make-pod-buffer)))
    (let ((size-pos (fill-pointer buf)))
      (put-u32 buf 0)
      (put-u32 buf +spa-type-object+)
      (let ((body-start (fill-pointer buf)))
        (put-u32 buf object-type)
        (put-u32 buf object-id)
        (funcall prop-thunk buf)
        (set-u32 buf size-pos (- (fill-pointer buf) body-start))))
    buf))

;;; ------------------------------------------------------------------
;;; The EnumFormat object we offer to pw_stream_connect.

(defun build-enum-format-pod ()
  "Return an octet vector: an EnumFormat object POD for raw video, offering the
common 32-bit packed formats over a wide size/framerate range."
  (let ((buf (make-pod-buffer)))
    (let ((size-pos (fill-pointer buf)))
      (put-u32 buf 0)                      ; body size (backpatched)
      (put-u32 buf +spa-type-object+)
      (let ((body-start (fill-pointer buf)))
        (put-u32 buf +spa-type-object-format+)   ; object type
        (put-u32 buf +spa-param-enum-format+)    ; object id
        (pod-prop buf +spa-format-media-type+
                  (lambda () (pod-id buf +spa-media-type-video+)))
        (pod-prop buf +spa-format-media-subtype+
                  (lambda () (pod-id buf +spa-media-subtype-raw+)))
        (pod-prop buf +spa-format-video-format+
                  (lambda ()
                    (pod-choice-enum-id
                     buf +spa-video-format-bgrx+
                     (list +spa-video-format-bgrx+ +spa-video-format-rgbx+
                           +spa-video-format-bgra+ +spa-video-format-rgba+
                           +spa-video-format-xrgb+ +spa-video-format-argb+))))
        (pod-prop buf +spa-format-video-size+
                  (lambda ()
                    (pod-choice-range-rect buf 1920 1080 1 1 8192 8192)))
        (pod-prop buf +spa-format-video-framerate+
                  (lambda ()
                    ;; Ask for a STEADY framerate (positive floor) rather than the
                    ;; old min=0, which let the compositor drop to 0fps when the
                    ;; screen was static -- starving the cursor metadata (that rides
                    ;; on frames) and leaving gaps where the pointer can't be tracked
                    ;; (green-screen: pointer "way off" during static stretches).
                    (pod-choice-range-fraction buf 30 1 25 1 60 1)))
        (set-u32 buf size-pos (- (fill-pointer buf) body-start))))
    buf))

;;; ------------------------------------------------------------------
;;; ParamBuffers + ParamMeta, built in param_changed to finish negotiation.

(defun build-buffers-pod (stride size)
  "ParamBuffers requesting mappable (MemPtr/MemFd) buffers of STRIDE/SIZE."
  (let ((datatype (logior (ash 1 +spa-data-memptr+) (ash 1 +spa-data-memfd+))))
    (build-object-pod
     +spa-type-object-param-buffers+ +spa-param-buffers+
     (lambda (buf)
       (pod-prop buf +spa-param-buffers-buffers+
                 (lambda () (pod-choice-range-int buf 8 2 16)))
       (pod-prop buf +spa-param-buffers-blocks+  (lambda () (pod-int buf 1)))
       (pod-prop buf +spa-param-buffers-size+    (lambda () (pod-int buf size)))
       (pod-prop buf +spa-param-buffers-stride+  (lambda () (pod-int buf stride)))
       (pod-prop buf +spa-param-buffers-datatype+ (lambda () (pod-int buf datatype)))))))

(defun build-cursor-meta-pod ()
  "ParamMeta requesting SPA_META_Cursor (position, and bitmap when it changes)."
  (build-object-pod
   +spa-type-object-param-meta+ +spa-param-meta+
   (lambda (buf)
     (pod-prop buf +spa-param-meta-type+
               (lambda () (pod-id buf +spa-meta-cursor+)))
     (pod-prop buf +spa-param-meta-size+
               (lambda ()
                 ;; position + a cursor bitmap; keep the range generous so the
                 ;; compositor's chosen size always fits (HiDPI cursors are big).
                 (pod-choice-range-int buf
                                       (+ 28 20 (* 128 128 4))
                                       28
                                       (* 4 1024 1024)))))))

;;; ------------------------------------------------------------------
;;; Foreign-memory handoff.

(defun octets->foreign (vec)
  "Copy octet vector VEC into freshly malloc'd foreign memory; return the
pointer. Caller frees with cffi:foreign-free."
  (let* ((n (length vec))
         (p (cffi:foreign-alloc :uint8 :count n)))
    (dotimes (i n) (setf (cffi:mem-aref p :uint8 i) (aref vec i)))
    p))

(defun dump-enum-format-pod (path)
  "Write the EnumFormat POD bytes to PATH (for offline validation)."
  (let ((vec (build-enum-format-pod)))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence vec s))
    (length vec)))
