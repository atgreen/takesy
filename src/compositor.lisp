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
  (:export #:bringup-test #:texture-1to1-test
           #:compile-shader #:make-program #:make-fullscreen-quad
           #:make-texture-rgba #:draw-textured-quad #:make-test-pattern))

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
  "Encode an RGBA byte buffer to PATH via ffmpeg. No vflip: real content is drawn
from a source texture, and the texture-upload inversion (data row 0 -> GL v=0)
cancels glReadPixels' bottom-origin, so RGBA rows already arrive top-first. (M1's
solid clear has no source and no meaningful orientation.)"
  (let ((raw (format nil "~A.raw" path)))
    (with-open-file (s raw :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
      (write-sequence rgba s))
    (uiop:run-program
     (list "ffmpeg" "-y" "-loglevel" "error"
           "-f" "rawvideo" "-pix_fmt" "rgba"
           "-s" (format nil "~Dx~D" w h) "-i" raw
           "-frames:v" "1" path)
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

;;; ------------------------------------------------------------------
;;; M2 (7k8.2): real GL pipeline -- shader program, fullscreen quad (VAO/VBO),
;;; source texture, draw 1:1. This plumbing underlies every later milestone;
;;; the only thing that changes downstream is the fragment shader.

(defparameter +vs-passthrough+
  "#version 330 core
layout(location=0) in vec2 pos;
layout(location=1) in vec2 uv;
out vec2 v_uv;
void main() { v_uv = uv; gl_Position = vec4(pos, 0.0, 1.0); }")

(defparameter +fs-texture+
  "#version 330 core
in vec2 v_uv;
out vec4 frag;
uniform sampler2D tex;
void main() { frag = texture(tex, v_uv); }")

(defun shader-ok-p (value)
  "cl-opengl may report a GL status boolean as T, 1, NIL, 0, or :false --
normalise to a real generalized boolean (0/NIL/:false all mean failure)."
  (not (or (null value) (eql value 0) (eq value :false))))

(defun compile-shader (type source)
  (let ((s (gl:create-shader type)))
    (gl:shader-source s source)
    (gl:compile-shader s)
    (unless (shader-ok-p (gl:get-shader s :compile-status))
      (error "~A compile failed:~%~A" type (gl:get-shader-info-log s)))
    s))

(defun make-program (vs-src fs-src)
  "Compile + link a program from vertex/fragment GLSL. Shaders are flagged for
deletion once linked (GL frees them when the program is destroyed)."
  (let ((vs (compile-shader :vertex-shader vs-src))
        (fs (compile-shader :fragment-shader fs-src))
        (p  (gl:create-program)))
    (gl:attach-shader p vs)
    (gl:attach-shader p fs)
    (gl:link-program p)
    (unless (shader-ok-p (gl:get-program p :link-status))
      (error "program link failed:~%~A" (gl:get-program-info-log p)))
    (gl:delete-shader vs)
    (gl:delete-shader fs)
    p))

(defun make-fullscreen-quad ()
  "A VAO for a 4-vertex triangle strip covering NDC, interleaved pos(vec2) +
uv(vec2). uv(0,0) sits at NDC bottom-left so texture row 0 (GL's v=0) lands on
the framebuffer's bottom row -- keeping readback aligned with source rows.
Return (values vao vbo)."
  (let ((verts #(-1.0 -1.0  0.0 0.0
                  1.0 -1.0  1.0 0.0
                 -1.0  1.0  0.0 1.0
                  1.0  1.0  1.0 1.0))
        (vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (let ((arr (gl:alloc-gl-array :float (length verts))))
      (dotimes (i (length verts)) (setf (gl:glaref arr i) (aref verts i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))
    (gl:enable-vertex-attrib-array 0)                 ; pos: 2 floats @ offset 0
    (gl:vertex-attrib-pointer 0 2 :float nil 16 0)
    (gl:enable-vertex-attrib-array 1)                 ; uv:  2 floats @ offset 8
    (gl:vertex-attrib-pointer 1 2 :float nil 16 8)
    (values vao vbo)))

(defun make-texture-rgba (bytes w h)
  "Upload BYTES (a (unsigned-byte 8) RGBA vector, len w*h*4) as a 2D texture.
NEAREST filtering + clamp so a 1:1 draw reproduces texels exactly."
  (let ((tex (gl:gen-texture)))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (cffi:with-pointer-to-vector-data (ptr bytes)
      (gl:tex-image-2d :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte ptr))
    tex))

(defun draw-textured-quad (program vao tex)
  (gl:use-program program)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d tex)
  (let ((loc (gl:get-uniform-location program "tex")))
    (when (>= loc 0) (gl:uniformi loc 0)))
  (gl:bind-vertex-array vao)
  (gl:draw-arrays :triangle-strip 0 4))

(defun make-test-pattern (w h)
  "Deterministic RGBA test pattern: per-axis colour ramps in R/G plus a bright
top-left quadrant in B. Distinct per pixel so a 1:1 compare is meaningful."
  (let ((v (make-array (* w h 4) :element-type '(unsigned-byte 8))))
    (dotimes (y h)
      (dotimes (x w)
        (let ((i (* 4 (+ (* y w) x))))
          (setf (aref v (+ i 0)) (logand (* x 3) #xFF)
                (aref v (+ i 1)) (logand (* y 3) #xFF)
                (aref v (+ i 2)) (if (and (< x (floor w 2)) (< y (floor h 2))) 200 40)
                (aref v (+ i 3)) 255))))
    v))

(defun texture-1to1-test (&key (width 256) (height 144) (path "/tmp/gs-m2-1to1.png"))
  "M2: upload a test pattern, draw it 1:1 through the shader pipeline, read back,
and assert an exact byte-for-byte match. Also writes PATH for eyeballing."
  (let ((src (make-test-pattern width height)))
    (egl:with-headless-gl (ctx width height)
      (declare (ignore ctx))
      (make-fbo width height)
      (let ((program (make-program +vs-passthrough+ +fs-texture+))
            (vao     (make-fullscreen-quad))
            (tex     (make-texture-rgba src width height)))
        (gl:clear-color 0.0 0.0 0.0 1.0)
        (gl:clear :color-buffer-bit)
        (draw-textured-quad program vao tex)
        (gl:finish)
        (let* ((out (read-rgba width height))
               (mism (loop for i below (length src)
                           count (/= (aref src i) (aref out i)))))
          (save-rgba-png out width height path)
          (format t "  [m2] ~Dx~D mismatched bytes = ~D / ~D~%"
                  width height mism (length src))
          (unless (zerop mism)
            (error "1:1 round-trip mismatch: ~D of ~D bytes differ"
                   mism (length src)))
          (format t "  [m2] EXACT 1:1 round-trip OK -> ~A~%" path)
          path)))))
