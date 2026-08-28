;;;; compositor.lisp
;;;;
;;;; Bead green-screen-7k8: GL compositor. Renders raw screen frames + Director
;;;; keyframes into polished output (zoom/pan, padded background, rounded
;;;; corners, drop shadow) -> ffmpeg. Runs headless via green-screen/egl.
;;;;
;;;; Milestone M1 (7k8.1): stand up the offscreen context, clear an FBO to a
;;;; known colour, read it back, and save a PNG -- proving headless GPU render +
;;;; readback works on this box before any real shading.

(defpackage #:green-screen/compositor
  (:use #:cl)
  (:local-nicknames (#:egl #:green-screen/egl))
  (:export #:bringup-test))

(in-package #:green-screen/compositor)

;;; ------------------------------------------------------------------
;;; Offscreen framebuffer.

(defun make-fbo (w h)
  "Create and bind an RGBA8 offscreen framebuffer of W x H. Return (values fbo rbo)."
  (let ((fbo (gl:gen-framebuffer))
        (rbo (gl:gen-renderbuffer)))
    (gl:bind-framebuffer :framebuffer fbo)
    (gl:bind-renderbuffer :renderbuffer rbo)
    (gl:renderbuffer-storage :renderbuffer :rgba8 w h)
    (gl:framebuffer-renderbuffer :framebuffer :color-attachment0 :renderbuffer rbo)
    ;; cl-opengl maps GL_FRAMEBUFFER_COMPLETE (0x8CD5) to the OES-tokened
    ;; keyword; accept every spelling of "complete".
    (let ((status (gl:check-framebuffer-status :framebuffer)))
      (unless (member status '(:framebuffer-complete
                               :framebuffer-complete-oes :framebuffer-complete-ext))
        (error "framebuffer incomplete: ~A" status)))
    (gl:viewport 0 0 w h)
    (values fbo rbo)))

;;; ------------------------------------------------------------------
;;; Readback + PNG. GL's framebuffer origin is bottom-left, so the raw bytes are
;;; upside-down relative to image convention; ffmpeg's vflip fixes it on encode.
;;; (Reusing the ffmpeg rawvideo shell-out pattern from src/pipewire.lisp.)

(defun read-rgba (w h)
  "Read the bound framebuffer as a flat (unsigned-byte 8) RGBA vector (len w*h*4)."
  (let ((px (gl:read-pixels 0 0 w h :rgba :unsigned-byte)))
    ;; cl-opengl returns a specialized Lisp vector for :unsigned-byte reads.
    (unless (typep px '(simple-array (unsigned-byte 8) (*)))
      (error "unexpected read-pixels return type: ~A" (type-of px)))
    px))

(defun save-rgba-png (rgba w h path)
  (let ((raw (format nil "~A.raw" path)))
    (with-open-file (s raw :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
      (write-sequence rgba s))
    (uiop:run-program
     (list "ffmpeg" "-y" "-loglevel" "error"
           "-f" "rawvideo" "-pix_fmt" "rgba"
           "-s" (format nil "~Dx~D" w h) "-i" raw
           "-vf" "vflip" "-frames:v" "1" path)
     :output t :error-output t)
    path))

;;; ------------------------------------------------------------------
;;; M1 bring-up.

(defun bringup-test (&key (width 640) (height 360) (path "/tmp/gs-egl-bringup.png"))
  "Headless EGL -> FBO -> clear to a known colour -> read back -> PNG.
Return the PNG path. Prints the centre pixel so success is checkable in a script."
  (egl:with-headless-gl (ctx width height)
    (declare (ignore ctx))
    (format t "  [gl] renderer=~A version=~A~%"
            (gl:get-string :renderer) (gl:get-string :version))
    (multiple-value-bind (fbo rbo) (make-fbo width height)
      (gl:clear-color 0.10 0.60 0.30 1.0)   ; a green-screen green
      (gl:clear :color-buffer-bit)
      (gl:finish)
      (let* ((rgba (read-rgba width height))
             (i (* 4 (+ (* (floor height 2) width) (floor width 2)))))
        (format t "  [gl] centre pixel RGBA=(~D ~D ~D ~D)~%"
                (aref rgba i) (aref rgba (+ i 1)) (aref rgba (+ i 2)) (aref rgba (+ i 3)))
        (save-rgba-png rgba width height path)
        (gl:delete-framebuffers (list fbo))
        (gl:delete-renderbuffers (list rbo))
        (format t "  [gl] wrote ~A~%" path)
        path))))
