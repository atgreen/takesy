;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;; compositor.lisp
;;;;
;;;; Bead green-screen-7k8: GL compositor. Renders raw screen frames + Director
;;;; keyframes into polished output (zoom/pan, padded background, rounded
;;;; corners, drop shadow) -> ffmpeg. Runs headless via takesy/egl.
;;;;
;;;; Milestone M1 (7k8.1): stand up the offscreen context, clear an FBO to a
;;;; known colour, read it back, and save a PNG -- proving headless GPU render +
;;;; readback works on this box before any real shading.

(defpackage #:takesy/compositor
  (:use #:cl)
  (:local-nicknames (#:egl #:takesy/egl)
                    (#:kf  #:takesy/keyframe))
  (:export #:bringup-test #:texture-1to1-test #:zoom-crop-test
           #:compose-test #:compose-reduction-check #:compose-shadow-test
           #:render-timeline #:render-frame-sequence #:render-demo
           #:update-texture-rgba #:rgba->bgrx #:bgrx-bridge-test
           #:compile-shader #:make-program #:make-fullscreen-quad
           #:make-texture-rgba #:draw-textured-quad #:draw-zoom #:draw-compose
           #:make-test-pattern #:make-gradient-pattern))

(in-package #:takesy/compositor)

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

(defun bringup-test (&key (width 640) (height 360) (path "/tmp/tk-egl-bringup.png"))
  "Headless EGL -> FBO -> clear to a known colour -> read back -> PNG.
Return the PNG path. Prints the centre pixel so success is checkable in a script."
  (egl:with-headless-gl (ctx width height)
    (declare (ignore ctx))
    (format t "  [gl] renderer=~A version=~A~%"
            (gl:get-string :renderer) (gl:get-string :version))
    (multiple-value-bind (fbo rbo) (make-fbo width height)
      (gl:clear-color 0.10 0.60 0.30 1.0)   ; a chroma-key green
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

(defun make-texture-rgba (bytes w h &key (source-format :rgba) (filter :nearest)
                                         (mipmap nil))
  "Upload BYTES (a (unsigned-byte 8) vector, len w*h*4) as an RGBA8 texture.
SOURCE-FORMAT is how GL should read the bytes: :rgba, or :bgra for captured
frames (SPA BGRx, fmt=8) so R/B land correctly with no shader change. FILTER is
:nearest (exact 1:1) or :linear (smooth when scaled). MIPMAP t builds a mip chain
and selects trilinear minification -- essential quality when the source is
downscaled a lot (e.g. a 4K capture into a small canvas), so fine detail like text
filters cleanly instead of aliasing."
  (let ((tex (gl:gen-texture)))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-parameter :texture-2d :texture-min-filter
                      (if mipmap :linear-mipmap-linear filter))
    (gl:tex-parameter :texture-2d :texture-mag-filter (if mipmap :linear filter))
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (cffi:with-pointer-to-vector-data (ptr bytes)
      (gl:tex-image-2d :texture-2d 0 :rgba w h 0 source-format :unsigned-byte ptr))
    (when mipmap (gl:generate-mipmap :texture-2d))
    tex))

(defun update-texture-rgba (tex bytes w h &key (source-format :rgba) (mipmap nil))
  "Replace TEX's pixels in place (glTexSubImage2D) -- for a per-frame video source
where re-uploading a whole new texture each frame would churn allocations. MIPMAP
t rebuilds the mip chain after the upload (needed each frame for trilinear
minification to stay correct)."
  (gl:bind-texture :texture-2d tex)
  (cffi:with-pointer-to-vector-data (ptr bytes)
    (gl:tex-sub-image-2d :texture-2d 0 0 0 w h source-format :unsigned-byte ptr))
  (when mipmap (gl:generate-mipmap :texture-2d))
  tex)

(defun rgba->bgrx (rgba)
  "Swap R and B channels of an RGBA byte vector, yielding BGRx (the SPA capture
layout, fmt=8). Reference/test helper -- real capture already delivers BGRx."
  (let ((out (make-array (length rgba) :element-type '(unsigned-byte 8))))
    (loop for i below (length rgba) by 4 do
      (setf (aref out i)       (aref rgba (+ i 2))
            (aref out (+ i 1)) (aref rgba (+ i 1))
            (aref out (+ i 2)) (aref rgba i)
            (aref out (+ i 3)) (aref rgba (+ i 3))))
    out))

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

;;; ------------------------------------------------------------------
;;; M3 (7k8.3): zoom/pan. The fragment shader samples a sub-window of the source
;;; -- centred on the keyframe focal point, sized 1/zoom -- and maps it to the
;;; full output. This is the auto-zoom "punch in on the action" transform.

(defparameter +fs-zoom+
  "#version 330 core
in vec2 v_uv;
out vec4 frag;
uniform sampler2D tex;
uniform float u_zoom;      // >= 1
uniform vec2  u_center;    // focal point in source UV, pre-clamped
void main() {
  vec2 uv = u_center + (v_uv - vec2(0.5)) / u_zoom;
  frag = texture(tex, uv);
}")

(defun draw-zoom (program vao tex zoom center-x center-y)
  "Draw the source texture zoomed by ZOOM about (CENTER-X,CENTER-Y) in source UV."
  (gl:use-program program)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d tex)
  (flet ((uni (name) (gl:get-uniform-location program name)))
    (let ((l (uni "tex")))      (when (>= l 0) (gl:uniformi l 0)))
    (let ((l (uni "u_zoom")))   (when (>= l 0) (gl:uniformf l (float zoom 1.0))))
    (let ((l (uni "u_center"))) (when (>= l 0)
                                  (gl:uniformf l (float center-x 1.0) (float center-y 1.0)))))
  (gl:bind-vertex-array vao)
  (gl:draw-arrays :triangle-strip 0 4))

(defun make-gradient-pattern (w h)
  "Smooth RGBA gradient: R ramps with x, G ramps with y, B constant. Adjacent
pixels differ by ~1, so a nearest-sample check tolerates GPU/CPU float rounding
at texel boundaries without the discontinuities of a wrapping pattern."
  (let ((v (make-array (* w h 4) :element-type '(unsigned-byte 8))))
    (dotimes (y h)
      (dotimes (x w)
        (let ((i (* 4 (+ (* y w) x))))
          (setf (aref v (+ i 0)) (floor (* x 255) (max 1 (1- w)))
                (aref v (+ i 1)) (floor (* y 255) (max 1 (1- h)))
                (aref v (+ i 2)) 128
                (aref v (+ i 3)) 255))))
    v))

(defun sample-nearest (src w h u v)
  "Nearest-sample SRC (RGBA, data row 0 = texture v=0) at UV with clamp-to-edge.
Return the R,G,B bytes."
  (let* ((sx (min (1- w) (max 0 (floor (* u w)))))
         (sy (min (1- h) (max 0 (floor (* v h)))))
         (i  (* 4 (+ (* sy w) sx))))
    (values (aref src i) (aref src (+ i 1)) (aref src (+ i 2)))))

(defun verify-zoom (src out w h zoom cx cy &key (tol 3) (border 0))
  "Compare every output pixel against the predicted nearest-sample of SRC under
the zoom/pan transform. Readback row r <-> output uv.v=(r+0.5)/h (bottom-origin,
matching glReadPixels); col c <-> uv.u=(c+0.5)/w. BORDER skips that many edge
pixels (used when an antialiased rounded edge blends the outermost ring).
Return (values bad-count worst)."
  (let ((bad 0) (worst 0))
    (dotimes (r h)
      (dotimes (c w)
        (unless (or (< r border) (>= r (- h border))
                    (< c border) (>= c (- w border)))
          (let* ((ovx (/ (+ c 0.5) w))
                 (ovy (/ (+ r 0.5) h))
                 (u   (+ cx (/ (- ovx 0.5) zoom)))
                 (v   (+ cy (/ (- ovy 0.5) zoom)))
                 (oi  (* 4 (+ (* r w) c))))
            (multiple-value-bind (er eg eb) (sample-nearest src w h u v)
              (let ((d (max (abs (- er (aref out oi)))
                            (abs (- eg (aref out (+ oi 1))))
                            (abs (- eb (aref out (+ oi 2)))))))
                (setf worst (max worst d))
                (when (> d tol) (incf bad))))))))
    (values bad worst)))

(defun zoom-crop-test (&key (width 256) (height 144)
                            (zoom 2.0) (center-x 0.6) (center-y 0.35)
                            (path "/tmp/gs-m3-zoom.png"))
  "M3: render a keyframe-driven punch-in over a gradient and verify the full
frame matches the predicted zoom/pan sampling of the source."
  (let* ((src (make-gradient-pattern width height))
         (frame (kf:make-keyframe :zoom zoom :center-x center-x :center-y center-y))
         (ec  (kf:effective-center frame))
         (cx  (car ec)) (cy (cdr ec)))
    (egl:with-headless-gl (ctx width height)
      (declare (ignore ctx))
      (make-fbo width height)
      (let ((program (make-program +vs-passthrough+ +fs-zoom+))
            (vao     (make-fullscreen-quad))
            (tex     (make-texture-rgba src width height)))
        (gl:clear-color 0.0 0.0 0.0 1.0)
        (gl:clear :color-buffer-bit)
        (draw-zoom program vao tex (kf:keyframe-zoom frame) cx cy)
        (gl:finish)
        (let ((out (read-rgba width height)))
          (save-rgba-png out width height path)
          (multiple-value-bind (bad worst) (verify-zoom src out width height zoom cx cy)
            (format t "  [m3] zoom=~,2F center=(~,3F,~,3F) bad=~D/~D worst-delta=~D -> ~A~%"
                    zoom cx cy bad (* width height) worst path)
            (when (> bad (floor (* width height) 200))   ; allow <0.5% boundary pixels
              (error "zoom transform mismatch: ~D pixels exceed tolerance (worst ~D)"
                     bad worst))
            (format t "  [m3] zoom/pan transform verified~%")
            path))))))

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

;;; ------------------------------------------------------------------
;;; M4 (7k8.4): the polished inset look. Draw the (zoomed) screen inset on a
;;; padded background, masked to a rounded rectangle. All in one fragment shader:
;;; a signed-distance rounded-box gives an antialiased edge; inside the box we
;;; sample the zoomed source, outside we show the background.

(defparameter +fs-compose+
  "#version 330 core
in vec2 v_uv;
out vec4 frag;
uniform sampler2D tex;
uniform float u_zoom;
uniform vec2  u_center;    // pre-clamped focal point, source UV
uniform vec2  u_canvas;       // output size in px (W,H)
uniform float u_padding;      // inset margin, fraction of min(W,H)
uniform float u_corner;       // rounded-rect radius, fraction of min(content dim)
uniform vec3  u_bg;           // background colour (used when u_has_bg == 0)
uniform sampler2D u_bgtex;    // background image (used when u_has_bg == 1)
uniform int   u_has_bg;       // 1 = sample u_bgtex, 0 = solid u_bg
uniform vec2  u_bg_size;      // background image size in px (for cover-fit aspect)
uniform float u_shadow_blur;  // shadow softness, fraction of min(W,H); 0 = none
uniform float u_shadow_alpha; // shadow peak opacity 0..1
uniform vec4  u_crop;         // (x0,y0,x1,y1) source UV region to show; 0011 = all

// Signed distance to a rounded box centred at origin, half-size b, radius r.
float sd_round_box(vec2 p, vec2 b, float r) {
  vec2 q = abs(p) - b + vec2(r);
  return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r;
}

void main() {
  vec2  P   = v_uv * u_canvas;                     // pixel coordinate
  vec2  pad = u_padding * u_canvas;                // per-axis padding: the inset
  vec2  lo  = pad;                                 // keeps the source aspect (no
  vec2  hi  = u_canvas - pad;                      // stretch), since the canvas
  vec2  sz  = hi - lo;                             // already matches it
  vec2  ctr = 0.5 * (lo + hi);
  vec2  b   = 0.5 * sz;
  float r   = u_corner * min(sz.x, sz.y);
  float d   = sd_round_box(P - ctr, b, r);
  float aa  = fwidth(d) + 1e-6;                    // ~1px antialiased edge
  float ins = 1.0 - smoothstep(-aa, aa, d);
  vec2  cuv = (P - lo) / sz;                       // UV within the inset rect
  vec2  suv = u_center + (cuv - vec2(0.5)) / u_zoom;
  suv = u_crop.xy + suv * (u_crop.zw - u_crop.xy);   // crop to the content region
  vec3  screen = texture(tex, suv).rgb;

  // Soft drop shadow: an offset, blurred copy of the same rounded rect, behind
  // the content. +P.y is image-down (see draw-compose), so the shadow drops
  // downward. Layer order: background -> shadow -> content.
  float blurPx = u_shadow_blur * min(u_canvas.x, u_canvas.y);
  float sh = 0.0;
  if (blurPx > 0.0 && u_shadow_alpha > 0.0) {
    vec2  soff = vec2(0.0, 0.35 * blurPx);
    float dsh  = sd_round_box(P - ctr - soff, b, r);
    sh = 1.0 - smoothstep(0.0, blurPx, dsh);
  }
  // Background: a solid colour, or a cover-fit image (scaled to fill the canvas
  // preserving aspect, centre-cropped) when one is supplied.
  vec3 bgcol = u_bg;
  if (u_has_bg == 1) {
    float ca = u_canvas.x / u_canvas.y;
    float ia = u_bg_size.x / u_bg_size.y;
    vec2  s  = (ia > ca) ? vec2(ca / ia, 1.0) : vec2(1.0, ia / ca);
    vec2  buv = (v_uv - vec2(0.5)) * s + vec2(0.5);
    bgcol = texture(u_bgtex, buv).rgb;
  }
  vec3 col = mix(bgcol, vec3(0.0), u_shadow_alpha * sh);
  col      = mix(col, screen, ins);
  frag = vec4(col, 1.0);
}")

(defun draw-compose (program vao tex frame canvas-w canvas-h
                     &optional (crop '(0.0 0.0 1.0 1.0))
                     &key bg-tex bg-size)
  "Draw FRAME's zoomed screen inset on its background, rounded corners, into the
bound FBO. CANVAS-W/H are the output size in pixels. CROP (x0 y0 x1 y1 in source
UV) selects the region of the source to show -- used to trim empty desktop
borders so the output frames the actual content. BG-TEX, when given, is a
background-image texture (BG-SIZE = (cons w . h) px) drawn cover-fit instead of
the solid FRAME background colour."
  (let ((ec (kf:effective-center frame)))
    (gl:use-program program)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d tex)
    (when bg-tex
      (gl:active-texture :texture1)
      (gl:bind-texture :texture-2d bg-tex)
      (gl:active-texture :texture0))
    (flet ((uni (n) (gl:get-uniform-location program n)))
      (let ((l (uni "tex")))       (when (>= l 0) (gl:uniformi l 0)))
      (let ((l (uni "u_bgtex")))   (when (>= l 0) (gl:uniformi l 1)))
      (let ((l (uni "u_has_bg")))  (when (>= l 0) (gl:uniformi l (if bg-tex 1 0))))
      (let ((l (uni "u_bg_size")))
        (when (and (>= l 0) bg-size)
          (gl:uniformf l (float (car bg-size) 1.0) (float (cdr bg-size) 1.0))))
      (let ((l (uni "u_zoom")))    (when (>= l 0) (gl:uniformf l (float (kf:keyframe-zoom frame) 1.0))))
      (let ((l (uni "u_center")))  (when (>= l 0) (gl:uniformf l (float (car ec) 1.0) (float (cdr ec) 1.0))))
      (let ((l (uni "u_canvas")))  (when (>= l 0) (gl:uniformf l (float canvas-w 1.0) (float canvas-h 1.0))))
      (let ((l (uni "u_padding"))) (when (>= l 0) (gl:uniformf l (float (kf:keyframe-padding frame) 1.0))))
      (let ((l (uni "u_corner")))  (when (>= l 0) (gl:uniformf l (float (kf:keyframe-corner-radius frame) 1.0))))
      (let ((l (uni "u_shadow_blur")))  (when (>= l 0) (gl:uniformf l (float (kf:keyframe-shadow-blur frame) 1.0))))
      (let ((l (uni "u_shadow_alpha"))) (when (>= l 0) (gl:uniformf l (float (kf:keyframe-shadow-alpha frame) 1.0))))
      (let ((l (uni "u_bg")))
        (when (>= l 0)
          (destructuring-bind (r g b) (kf:keyframe-bg-color frame)
            (gl:uniformf l (float r 1.0) (float g 1.0) (float b 1.0)))))
      (let ((l (uni "u_crop")))
        (when (>= l 0)
          (destructuring-bind (x0 y0 x1 y1) crop
            (gl:uniformf l (float x0 1.0) (float y0 1.0) (float x1 1.0) (float y1 1.0))))))
    (gl:bind-vertex-array vao)
    (gl:draw-arrays :triangle-strip 0 4)))

(defun compose-reduction-check (&key (width 256) (height 144)
                                     (zoom 2.0) (center-x 0.6) (center-y 0.35))
  "With padding=0 and corner=0 the compose shader must collapse to the verified
M3 zoom. Check the interior (skip the 2px antialiased edge ring)."
  (let* ((src   (make-gradient-pattern width height))
         (frame (kf:make-keyframe :zoom zoom :center-x center-x :center-y center-y
                                  :padding 0.0 :corner-radius 0.0 :bg-color '(0.0 0.0 0.0)))
         (ec    (kf:effective-center frame)) (cx (car ec)) (cy (cdr ec)))
    (egl:with-headless-gl (ctx width height)
      (declare (ignore ctx))
      (make-fbo width height)
      (let ((program (make-program +vs-passthrough+ +fs-compose+))
            (vao     (make-fullscreen-quad))
            (tex     (make-texture-rgba src width height)))
        (gl:clear-color 0.0 0.0 0.0 1.0)
        (gl:clear :color-buffer-bit)
        (draw-compose program vao tex frame width height)
        (gl:finish)
        (let ((out (read-rgba width height)))
          (multiple-value-bind (bad worst)
              (verify-zoom src out width height zoom cx cy :border 2)
            (format t "  [m4] reduction (pad0/corner0) bad=~D worst-delta=~D~%" bad worst)
            (when (> bad (floor (* width height) 200))
              (error "M4 does not reduce to M3 zoom: ~D interior pixels differ (worst ~D)"
                     bad worst))
            (format t "  [m4] reduces to M3 zoom on interior~%")
            t))))))

(defun compose-test (&key (width 320) (height 200)
                          (zoom 1.4) (center-x 0.5) (center-y 0.5)
                          (padding 0.07) (corner 0.12) (bg '(0.11 0.12 0.15))
                          (path "/tmp/gs-m4-compose.png"))
  "M4: render the full inset/background/rounded-corner composite over a gradient.
Structural check: all four canvas corners (in the padding) must be background."
  (let* ((src   (make-gradient-pattern width height))
         (frame (kf:make-keyframe :zoom zoom :center-x center-x :center-y center-y
                                  :padding padding :corner-radius corner :bg-color bg)))
    (egl:with-headless-gl (ctx width height)
      (declare (ignore ctx))
      (make-fbo width height)
      (let ((program (make-program +vs-passthrough+ +fs-compose+))
            (vao     (make-fullscreen-quad))
            (tex     (make-texture-rgba src width height)))
        (gl:clear-color 0.0 0.0 0.0 1.0)
        (gl:clear :color-buffer-bit)
        (draw-compose program vao tex frame width height)
        (gl:finish)
        (let ((out (read-rgba width height))
              (bg8 (mapcar (lambda (c) (round (* 255 c))) bg)))
          (save-rgba-png out width height path)
          (flet ((px (r c) (let ((i (* 4 (+ (* r width) c))))
                             (list (aref out i) (aref out (+ i 1)) (aref out (+ i 2)))))
                 (near (a b) (every (lambda (x y) (<= (abs (- x y)) 2)) a b)))
            (let* ((corners (list (px 0 0) (px 0 (1- width))
                                  (px (1- height) 0) (px (1- height) (1- width))))
                   (ok (count-if (lambda (p) (near p bg8)) corners)))
              (format t "  [m4] bg=~A corners-are-bg=~D/4 -> ~A~%" bg8 ok path)
              (when (< ok 4)
                (error "M4: only ~D/4 canvas corners are background" ok))))
          (format t "  [m4] composite OK~%")
          path)))))

;;; ------------------------------------------------------------------
;;; M5 (7k8.5): soft drop shadow. Handled by the same +fs-compose+ pass (a second
;;; offset/blurred rounded-box SDF behind the content). Orientation note: readback
;;; is bottom-origin and we save without flipping, so image-down = increasing P.y
;;; in the shader; the shadow offset is +P.y (drops downward in the final image).

(defun compose-shadow-test (&key (width 320) (height 200)
                                 (zoom 1.4) (padding 0.10) (corner 0.12)
                                 (bg '(0.55 0.57 0.62))
                                 (shadow-blur 0.06) (shadow-alpha 0.55)
                                 (path "/tmp/gs-m5-shadow.png"))
  "M5: render with a soft drop shadow on a light background so the shadow is
visible, then assert (a) a point in the shadow band just below the inset is
darker than the background, and (b) the top-left canvas corner is still clean bg."
  (let* ((src   (make-gradient-pattern width height))
         (frame (kf:make-keyframe :zoom zoom :padding padding :corner-radius corner
                                  :bg-color bg :shadow-blur shadow-blur
                                  :shadow-alpha shadow-alpha)))
    (egl:with-headless-gl (ctx width height)
      (declare (ignore ctx))
      (make-fbo width height)
      (let ((program (make-program +vs-passthrough+ +fs-compose+))
            (vao     (make-fullscreen-quad))
            (tex     (make-texture-rgba src width height)))
        (gl:clear-color 0.0 0.0 0.0 1.0)
        (gl:clear :color-buffer-bit)
        (draw-compose program vao tex frame width height)
        (gl:finish)
        (let* ((out    (read-rgba width height))
               (bg8    (mapcar (lambda (c) (round (* 255 c))) bg))
               (m      (* padding (min width height)))
               (blurpx (* shadow-blur (min width height))))
          (save-rgba-png out width height path)
          (flet ((px (r c) (let ((i (* 4 (+ (* r width) c))))
                             (list (aref out i) (aref out (+ i 1)) (aref out (+ i 2))))))
            ;; Shadow band: just below the inset's bottom edge (image-down = +P.y),
            ;; horizontally centred. P.y = (H-m) + 0.5*blur -> readback row P.y-0.5.
            (let* ((band-r (min (1- height)
                                (round (- (+ (- height m) (* 0.5 blurpx)) 0.5))))
                   (band-c (floor width 2))
                   (band   (px band-r band-c))
                   (corner (px 0 0))
                   (band-sum (reduce #'+ band))
                   (bg-sum   (reduce #'+ bg8)))
              (format t "  [m5] bg=~A shadow-band@(~D,~D)=~A corner=~A~%"
                      bg8 band-r band-c band corner)
              (unless (< band-sum (- bg-sum 20))
                (error "M5: shadow band ~A is not darker than bg ~A" band bg8))
              (unless (every (lambda (x y) (<= (abs (- x y)) 3)) corner bg8)
                (error "M5: top-left corner ~A is not clean background ~A" corner bg8))
              (format t "  [m5] drop shadow verified (band darker, corner clean) -> ~A~%"
                      path)))
          path)))))

;;; ------------------------------------------------------------------
;;; M6 (7k8.6): the finale. Interpolate a keyframe timeline across N frames,
;;; render each through the compose shader, and encode to mp4. Context/program/
;;; quad/texture are built once and reused across frames (the source is a still
;;; for the MVP; a real capture is a per-frame texture upload later). Frames are
;;; streamed to a raw file, then encoded in one ffmpeg pass.

(defparameter *h264-encoders* '("libx264" "libopenh264")
  "Software H.264 encoders to try, best-first. Distros vary: Fedora's default
ffmpeg ships libopenh264 but not libx264.")

(defun ffmpeg-has-encoder-p (name)
  (let ((out (with-output-to-string (s)
               (ignore-errors
                (uiop:run-program (list "ffmpeg" "-hide_banner" "-encoders")
                                  :output s :error-output nil
                                  :ignore-error-status t)))))
    (and (search name out) t)))

(defun pick-h264-encoder ()
  (or (find-if #'ffmpeg-has-encoder-p *h264-encoders*)
      (error "no usable H.264 encoder in ffmpeg (tried ~{~A~^, ~})" *h264-encoders*)))

(defun %quality-flags (encoder width height fps)
  "Per-encoder flags for a visually-sharp screen recording. x264 gets a low CRF
(near-lossless) and a screen-tuned preset; encoders without CRF get a generous
bitrate scaled to resolution (~0.12 bits/pixel, screen content compresses well)."
  (cond
    ((string= encoder "libx264")
     (list "-crf" "18" "-preset" "slow" "-tune" "stillimage"))
    (t
     (let ((bps (max 4000000 (round (* width height fps 0.12)))))
       (list "-b:v" (format nil "~D" bps) "-maxrate" (format nil "~D" (* 2 bps))
             "-bufsize" (format nil "~D" (* 2 bps)))))))

;;; Streaming encoder: pipe composited RGBA frames straight to ffmpeg's stdin so
;;; long renders don't spill a huge raw dump. A 20-min 1080p@24 output would be
;;; ~240 GB of raw RGBA on disk if buffered; streamed, peak scratch is ~zero.

(defstruct (frame-encoder (:constructor %make-frame-encoder))
  proc stream path encoder mux)

(defun %gif-output-p (path)
  "T if PATH names a .gif output (case-insensitive)."
  (let ((s (string-downcase (namestring path))))
    (and (>= (length s) 4) (string= (subseq s (- (length s) 4)) ".gif"))))

(defun %open-frame-encoder (path width height fps &key audio)
  "Launch ffmpeg reading rawvideo RGBA WxH from stdin, encoding to PATH. Encodes
H.264 (muxing AUDIO if given, trimmed to the shorter stream), or -- when PATH ends
in .gif -- an animated GIF via a single-pass palette filtergraph (no audio; GIFs
have none). Return a FRAME-ENCODER: write each frame's bytes to its STREAM, then
%CLOSE-FRAME-ENCODER to finalize."
  (let* ((gif     (%gif-output-p path))
         (encoder (if gif "gif" (pick-h264-encoder)))
         (mux     (and (not gif) audio (probe-file audio)))
         (input   (list "ffmpeg" "-y" "-loglevel" "error"
                        "-f" "rawvideo" "-pix_fmt" "rgba"
                        "-s" (format nil "~Dx~D" width height)
                        "-r" (format nil "~D" fps) "-i" "pipe:0"))
         (cmd     (if gif
                      ;; one-pass high-quality GIF: build a per-clip palette and
                      ;; apply it in the same graph (works over the stdin stream,
                      ;; which a two-pass palettegen file could not re-read).
                      (append input
                              (list "-vf"
                                    (concatenate 'string
                                     "split[a][b];[a]palettegen=stats_mode=diff[p];"
                                     "[b][p]paletteuse=dither=bayer:diff_mode=rectangle")
                                    path))
                      (append input
                              (when mux (list "-i" (namestring mux)))
                              (list "-c:v" encoder)
                              (%quality-flags encoder width height fps)
                              (when mux (list "-c:a" "aac" "-b:a" "192k" "-shortest"))
                              (list "-pix_fmt" "yuv420p" "-movflags" "+faststart" path))))
         ;; stderr discarded so ffmpeg never blocks on an undrained pipe; our
         ;; writes get natural backpressure from ffmpeg reading stdin.
         (proc (uiop:launch-program cmd :input :stream
                                        :output nil :error-output nil)))
    (when (and gif audio)
      (format t "  [enc] gif output -- audio not included (GIFs have no audio)~%"))
    (%make-frame-encoder :proc proc :stream (uiop:process-info-input proc)
                         :path path :encoder encoder :mux mux)))

(defun %write-frame (enc bytes)
  "Write one RGBA frame BYTES to the encoder's stdin."
  (write-sequence bytes (frame-encoder-stream enc)))

(defun %close-frame-encoder (enc n fps)
  "Flush + close the encoder's stdin (EOF), wait for ffmpeg, and report. Signal if
ffmpeg exits non-zero so a failed encode isn't silently reported as success."
  (ignore-errors (finish-output (frame-encoder-stream enc)))
  (ignore-errors (close (frame-encoder-stream enc)))     ; EOF -> ffmpeg finalizes
  (let ((code (uiop:wait-process (frame-encoder-proc enc))))
    (unless (eql code 0)
      (error "ffmpeg encoder exited ~A writing ~A" code (frame-encoder-path enc))))
  (format t "  [enc] ~D frames @ ~Dfps (~A, streamed)~:[~; +audio~] -> ~A~%"
          n fps (frame-encoder-encoder enc) (frame-encoder-mux enc)
          (frame-encoder-path enc))
  (values (frame-encoder-path enc) n))

(defun render-timeline (keyframes source width height
                        &key (fps 30) (duration 3.0) (path "/tmp/gs-comp.mp4")
                             (source-format :rgba))
  "Animate a still SOURCE (bytes, WxH; SOURCE-FORMAT :rgba or :bgra) through the
compose shader driven by KEYFRAMES over DURATION seconds at FPS, encode to an
H.264 mp4 at PATH. Return (values path n-frames). WIDTH/HEIGHT even (yuv420p)."
  (let* ((n   (max 1 (round (* fps duration)))))
    (egl:with-headless-gl (ctx width height)
      (declare (ignore ctx))
      (make-fbo width height)
      (let ((program (make-program +vs-passthrough+ +fs-compose+))
            (vao     (make-fullscreen-quad))
            (tex     (make-texture-rgba source width height :source-format source-format)))
        (let ((enc (%open-frame-encoder path width height fps)))
          (unwind-protect
               (dotimes (i n)
                 (let* ((tsec  (if (= n 1) 0.0 (* duration (/ i (float (1- n) 1.0)))))
                        (frame (kf:sample-timeline keyframes tsec)))
                   (gl:clear-color 0.0 0.0 0.0 1.0)
                   (gl:clear :color-buffer-bit)
                   (draw-compose program vao tex frame width height)
                   (gl:finish)
                   (%write-frame enc (read-rgba width height))))
            (%close-frame-encoder enc n fps))
          (values path n))))))

;;; ------------------------------------------------------------------
;;; Cursor overlay (am4.9). METADATA capture hides the HW cursor, so we draw one
;;; ourselves at the eased position, transformed through the same zoom/pan as the
;;; content and clipped to the rounded content rect. A white SDF arrow with a
;;; dark border, alpha-blended over the composited frame.

(defparameter +fs-cursor+
  "#version 330 core
in vec2 v_uv;
out vec4 frag;
uniform vec2  u_canvas;
uniform vec2  u_cursor;    // hotspot in framebuffer px
uniform float u_size;      // arrow size in px
uniform float u_padding;
uniform float u_corner;

float sd_tri(vec2 p, vec2 a, vec2 b, vec2 c) {
  vec2 e0=b-a, e1=c-b, e2=a-c, v0=p-a, v1=p-b, v2=p-c;
  vec2 pq0=v0-e0*clamp(dot(v0,e0)/dot(e0,e0),0.0,1.0);
  vec2 pq1=v1-e1*clamp(dot(v1,e1)/dot(e1,e1),0.0,1.0);
  vec2 pq2=v2-e2*clamp(dot(v2,e2)/dot(e2,e2),0.0,1.0);
  float s=sign(e0.x*e2.y-e0.y*e2.x);
  vec2 d=min(min(vec2(dot(pq0,pq0), s*(v0.x*e0.y-v0.y*e0.x)),
                 vec2(dot(pq1,pq1), s*(v1.x*e1.y-v1.y*e1.x))),
                 vec2(dot(pq2,pq2), s*(v2.x*e2.y-v2.y*e2.x)));
  return -sqrt(d.x)*sign(d.y);
}
float sd_round_box(vec2 p, vec2 b, float r) {
  vec2 q=abs(p)-b+vec2(r); return min(max(q.x,q.y),0.0)+length(max(q,0.0))-r;
}
void main() {
  vec2 P = v_uv * u_canvas;
  vec2 pad = u_padding * u_canvas;
  vec2 lo=pad, hi=u_canvas-pad, sz=hi-lo, ctr=0.5*(lo+hi);
  if (sd_round_box(P-ctr, 0.5*sz, u_corner*min(sz.x,sz.y)) > 0.0) discard;
  // arrow: tip at hotspot, body toward +x/+y (image right/down)
  vec2 L = (P - u_cursor) / u_size;
  float d  = sd_tri(L, vec2(0.0,0.0), vec2(0.0,1.0), vec2(0.62,0.62));
  float aa = fwidth(d) + 1e-5;
  float a  = 1.0 - smoothstep(0.0, aa, d);            // alpha: inside the arrow
  vec3  col = mix(vec3(0.05), vec3(1.0), step(0.09, -d)); // dark border, white fill
  frag = vec4(col, a);
}")

(defparameter +fs-cursor-image+
  "#version 330 core
in vec2 v_uv;
out vec4 frag;
uniform sampler2D u_tex;
uniform vec2  u_canvas;
uniform vec2  u_cursor;    // hotspot in framebuffer px
uniform vec2  u_size;      // drawn cursor size in px (w,h)
uniform vec2  u_hotspot;   // hotspot as a fraction of the image (0..1)
uniform float u_padding;
uniform float u_corner;

float sd_round_box(vec2 p, vec2 b, float r) {
  vec2 q=abs(p)-b+vec2(r); return min(max(q.x,q.y),0.0)+length(max(q,0.0))-r;
}
void main() {
  vec2 P = v_uv * u_canvas;
  vec2 pad = u_padding * u_canvas;
  vec2 lo=pad, hi=u_canvas-pad, sz=hi-lo, ctr=0.5*(lo+hi);
  if (sd_round_box(P-ctr, 0.5*sz, u_corner*min(sz.x,sz.y)) > 0.0) discard;  // clip to content
  // image space: hotspot sits at u_cursor; image extends by u_size around it.
  vec2 uv = (P - u_cursor) / u_size + u_hotspot;
  if (uv.x < 0.0 || uv.x > 1.0 || uv.y < 0.0 || uv.y > 1.0) discard;
  vec4 c = texture(u_tex, uv);
  if (c.a <= 0.003) discard;                     // fully-transparent pixels
  frag = c;
}")

(defun cursor-output-px (cursor-uv keyframe out-w out-h)
  "Map a cursor source-UV (cons u . v) through KEYFRAME's zoom/pan/padding to a
framebuffer pixel position. Return (values px py visible-p) -- visible-p is nil
when the cursor falls outside the zoomed content view."
  (let* ((ec (kf:effective-center keyframe))
         (z  (kf:keyframe-zoom keyframe))
         (p  (kf:keyframe-padding keyframe))
         (mx (* p out-w)) (my (* p out-h))       ; per-axis padding, matches shader
         (sx (- out-w (* 2 mx))) (sy (- out-h (* 2 my)))
         (cuvx (+ 0.5 (* (- (car cursor-uv) (car ec)) z)))
         (cuvy (+ 0.5 (* (- (cdr cursor-uv) (cdr ec)) z))))
    (values (+ mx (* cuvx sx)) (+ my (* cuvy sy))
            (and (<= 0.0 cuvx 1.0) (<= 0.0 cuvy 1.0)))))

(defun draw-cursor (program vao px py size keyframe out-w out-h)
  (gl:use-program program)
  (flet ((uni (n) (gl:get-uniform-location program n)))
    (let ((l (uni "u_canvas")))  (when (>= l 0) (gl:uniformf l (float out-w 1.0) (float out-h 1.0))))
    (let ((l (uni "u_cursor")))  (when (>= l 0) (gl:uniformf l (float px 1.0) (float py 1.0))))
    (let ((l (uni "u_size")))    (when (>= l 0) (gl:uniformf l (float size 1.0))))
    (let ((l (uni "u_padding"))) (when (>= l 0) (gl:uniformf l (float (kf:keyframe-padding keyframe) 1.0))))
    (let ((l (uni "u_corner")))  (when (>= l 0) (gl:uniformf l (float (kf:keyframe-corner-radius keyframe) 1.0)))))
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:bind-vertex-array vao)
  (gl:draw-arrays :triangle-strip 0 4)
  (gl:disable :blend))

(defun draw-cursor-image (program vao tex px py w h hotspot keyframe out-w out-h)
  "Draw a user cursor texture TEX with its HOTSPOT (cons hx . hy, fraction of the
image) placed at framebuffer (PX,PY), sized W x H px, clipped to the content rect."
  (gl:use-program program)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d tex)
  (flet ((uni (n) (gl:get-uniform-location program n)))
    (let ((l (uni "u_tex")))     (when (>= l 0) (gl:uniformi l 0)))
    (let ((l (uni "u_canvas")))  (when (>= l 0) (gl:uniformf l (float out-w 1.0) (float out-h 1.0))))
    (let ((l (uni "u_cursor")))  (when (>= l 0) (gl:uniformf l (float px 1.0) (float py 1.0))))
    (let ((l (uni "u_size")))    (when (>= l 0) (gl:uniformf l (float w 1.0) (float h 1.0))))
    (let ((l (uni "u_hotspot"))) (when (>= l 0) (gl:uniformf l (float (car hotspot) 1.0) (float (cdr hotspot) 1.0))))
    (let ((l (uni "u_padding"))) (when (>= l 0) (gl:uniformf l (float (kf:keyframe-padding keyframe) 1.0))))
    (let ((l (uni "u_corner")))  (when (>= l 0) (gl:uniformf l (float (kf:keyframe-corner-radius keyframe) 1.0)))))
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:bind-vertex-array vao)
  (gl:draw-arrays :triangle-strip 0 4)
  (gl:disable :blend))

(defun render-frame-sequence (keyframes frame-fn n-frames out-w out-h
                              &key (fps 30) (source-format :rgba)
                                   (source-width out-w) (source-height out-h)
                                   (time-fn nil) (cursor-fn nil)
                                   (cursor-image nil) (cursor-hotspot '(0.0 . 0.0))
                                   (cursor-size nil)
                                   (bg-image nil)
                                   (audio nil)
                                   (crop '(0.0 0.0 1.0 1.0))
                                   (path "/tmp/takesy-seq.mp4"))
  "Render a real per-frame video into an OUT-W x OUT-H canvas. FRAME-FN is
(i) -> a SOURCE-WIDTH x SOURCE-HEIGHT byte vector in SOURCE-FORMAT (:rgba or
captured :bgra) for output frame I; the source (texture) size is decoupled from
the output size, so a 3840x2400 capture can render to a small canvas. The compose
shader is driven by KEYFRAMES sampled at (TIME-FN i) seconds, or i/FPS if TIME-FN
is nil. CURSOR-FN, if given, is (i) -> (cons u . v) source-UV of the (eased)
cursor for frame I, or nil to draw none; it is transformed through the frame's
zoom and drawn as an overlay. CURSOR-IMAGE, when given, is (list rgba-bytes w h)
for a user cursor drawn instead of the built-in arrow, with CURSOR-HOTSPOT (cons
hx . hy, fraction of the image) as the click point and CURSOR-SIZE its height as a
fraction of OUT-H. BG-IMAGE, when given, is (list rgba-bytes w h) drawn cover-fit
as the padded background instead of the solid colour. The texture is uploaded once
and updated each frame."
  (progn
    (egl:with-headless-gl (ctx out-w out-h)
      (declare (ignore ctx))
      (make-fbo out-w out-h)
      (let* ((program  (make-program +vs-passthrough+ +fs-compose+))
             (curs-prog (when (and cursor-fn (not cursor-image))
                          (make-program +vs-passthrough+ +fs-cursor+)))
             (img-prog  (when (and cursor-fn cursor-image)
                          (make-program +vs-passthrough+ +fs-cursor-image+)))
             (vao      (make-fullscreen-quad))
             (tex      (make-texture-rgba (funcall frame-fn 0) source-width source-height
                                          :source-format source-format :filter :linear
                                          :mipmap t))   ; trilinear: clean downscales
             (cur-tex  (when cursor-image
                         (make-texture-rgba (first cursor-image)
                                            (second cursor-image) (third cursor-image)
                                            :source-format :rgba :filter :linear)))
             (bg-tex   (when bg-image
                         (make-texture-rgba (first bg-image)
                                            (second bg-image) (third bg-image)
                                            :source-format :rgba :filter :linear)))
             (bg-size  (when bg-image (cons (second bg-image) (third bg-image))))
             (cur-size (* out-h 0.030))                    ; built-in arrow scale
             (img-h    (* out-h (or cursor-size 0.06)))    ; user cursor height, px
             (img-w    (when cursor-image
                         (* img-h (/ (float (second cursor-image) 1.0)
                                     (float (third cursor-image) 1.0))))))
        (let ((enc (%open-frame-encoder path out-w out-h fps :audio audio)))
          (unwind-protect
               (dotimes (i n-frames)
                 (when (> i 0)
                   (update-texture-rgba tex (funcall frame-fn i) source-width source-height
                                        :source-format source-format :mipmap t))
                 (let* ((tsec  (if time-fn (funcall time-fn i) (/ i (float fps 1.0))))
                        (frame (kf:sample-timeline keyframes tsec)))
                   (gl:clear-color 0.0 0.0 0.0 1.0)
                   (gl:clear :color-buffer-bit)
                   (draw-compose program vao tex frame out-w out-h crop
                                 :bg-tex bg-tex :bg-size bg-size)
                   (when cursor-fn
                     (let ((cuv (funcall cursor-fn i)))
                       (when cuv
                         (multiple-value-bind (px py vis)
                             (cursor-output-px cuv frame out-w out-h)
                           (when vis
                             (if img-prog
                                 (draw-cursor-image img-prog vao cur-tex px py
                                                    img-w img-h cursor-hotspot
                                                    frame out-w out-h)
                                 (draw-cursor curs-prog vao px py cur-size frame out-w out-h)))))))
                   (gl:finish)
                   ;; stream this frame straight to ffmpeg -- no raw dump on disk
                   (%write-frame enc (read-rgba out-w out-h))))
            ;; on the happy path this finalizes; on a nonlocal exit it still
            ;; closes stdin so ffmpeg won't hang holding the pipe.
            (%close-frame-encoder enc n-frames fps))
          (values path n-frames))))))

(defun bgrx-bridge-test (&key (width 256) (height 144))
  "Feed a BGRx copy of an RGBA pattern with :source-format :bgra and draw it 1:1;
the RGB output must equal the original exactly, proving the capture-format bridge
corrects channel order with no shader change. Return T or error."
  (let* ((rgba (make-gradient-pattern width height))
         (bgrx (rgba->bgrx rgba)))
    (egl:with-headless-gl (ctx width height)
      (declare (ignore ctx))
      (make-fbo width height)
      (let ((program (make-program +vs-passthrough+ +fs-texture+))
            (vao     (make-fullscreen-quad))
            (tex     (make-texture-rgba bgrx width height :source-format :bgra)))
        (gl:clear-color 0.0 0.0 0.0 1.0)
        (gl:clear :color-buffer-bit)
        (draw-textured-quad program vao tex)
        (gl:finish)
        (let* ((out (read-rgba width height))
               (bad (loop for i below (length rgba) by 4
                          count (or (/= (aref rgba i)       (aref out i))
                                    (/= (aref rgba (+ i 1)) (aref out (+ i 1)))
                                    (/= (aref rgba (+ i 2)) (aref out (+ i 2)))))))
          (format t "  [7k8.7] bgrx-bridge mismatched pixels = ~D / ~D~%"
                  bad (* width height))
          (unless (zerop bad) (error "BGRx bridge mismatch: ~D px" bad))
          (format t "  [7k8.7] BGRx->RGBA exact -- channel order corrected~%")
          t)))))

(defun render-demo (&key (width 320) (height 200) (fps 30) (duration 3.0)
                         (path "/tmp/gs-comp.mp4"))
  "M6 demo: an auto-zoom timeline over a still test pattern -> mp4. Punches in on
the top-left, pans to the bottom-right, then eases back out."
  (let* ((src (make-test-pattern width height))
         (pad 0.06) (corner 0.10) (sblur 0.05) (salpha 0.5) (bg '(0.11 0.12 0.15)))
    (flet ((k (time zoom cx cy)
             (kf:make-keyframe :time time :zoom zoom :center-x cx :center-y cy
                               :padding pad :corner-radius corner
                               :shadow-blur sblur :shadow-alpha salpha :bg-color bg)))
      (render-timeline (list (k 0.0 1.0 0.5  0.5)
                             (k 1.2 2.2 0.28 0.30)
                             (k 2.0 2.2 0.72 0.68)
                             (k 3.0 1.0 0.5  0.5))
                       src width height :fps fps :duration duration :path path))))
