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
  (:local-nicknames (#:egl #:green-screen/egl)
                    (#:kf  #:green-screen/keyframe))
  (:export #:bringup-test #:texture-1to1-test #:zoom-crop-test
           #:compose-test #:compose-reduction-check #:compose-shadow-test
           #:compile-shader #:make-program #:make-fullscreen-quad
           #:make-texture-rgba #:draw-textured-quad #:draw-zoom #:draw-compose
           #:make-test-pattern #:make-gradient-pattern))

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

;;; ------------------------------------------------------------------
;;; M3 (7k8.3): zoom/pan. The fragment shader samples a sub-window of the source
;;; -- centred on the keyframe focal point, sized 1/zoom -- and maps it to the
;;; full output. This is the polished "punch in on the action" transform.

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
;;; M4 (7k8.4): the polished look. Draw the (zoomed) screen inset on a
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
uniform vec3  u_bg;           // background colour
uniform float u_shadow_blur;  // shadow softness, fraction of min(W,H); 0 = none
uniform float u_shadow_alpha; // shadow peak opacity 0..1

// Signed distance to a rounded box centred at origin, half-size b, radius r.
float sd_round_box(vec2 p, vec2 b, float r) {
  vec2 q = abs(p) - b + vec2(r);
  return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r;
}

void main() {
  vec2  P   = v_uv * u_canvas;                     // pixel coordinate
  float m   = u_padding * min(u_canvas.x, u_canvas.y);
  vec2  lo  = vec2(m);
  vec2  hi  = u_canvas - vec2(m);
  vec2  sz  = hi - lo;                             // content rect size
  vec2  ctr = 0.5 * (lo + hi);
  vec2  b   = 0.5 * sz;
  float r   = u_corner * min(sz.x, sz.y);
  float d   = sd_round_box(P - ctr, b, r);
  float aa  = fwidth(d) + 1e-6;                    // ~1px antialiased edge
  float ins = 1.0 - smoothstep(-aa, aa, d);
  vec2  cuv = (P - lo) / sz;                       // UV within the inset rect
  vec2  suv = u_center + (cuv - vec2(0.5)) / u_zoom;
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
  vec3 col = mix(u_bg, vec3(0.0), u_shadow_alpha * sh);
  col      = mix(col, screen, ins);
  frag = vec4(col, 1.0);
}")

(defun draw-compose (program vao tex frame canvas-w canvas-h)
  "Draw FRAME's zoomed screen inset on its background, rounded corners, into the
bound FBO. CANVAS-W/H are the output size in pixels (needed for isotropic
padding + circular corners)."
  (let ((ec (kf:effective-center frame)))
    (gl:use-program program)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d tex)
    (flet ((uni (n) (gl:get-uniform-location program n)))
      (let ((l (uni "tex")))       (when (>= l 0) (gl:uniformi l 0)))
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
            (gl:uniformf l (float r 1.0) (float g 1.0) (float b 1.0))))))
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
