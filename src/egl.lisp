;;;; egl.lisp
;;;;
;;;; Bead green-screen-7k8.1: hand-rolled CFFI bindings for the EGL headless
;;;; bootstrap. cl-opengl covers the GL rendering calls but deliberately omits
;;;; context/display creation, so we bind the small, stable slice of EGL needed
;;;; to stand up an offscreen (windowless) OpenGL context and render into an FBO.
;;;;
;;;; Path on this box (Intel/Mesa + NVIDIA): EGL_PLATFORM_SURFACELESS_MESA with
;;;; EGL_DEFAULT_DISPLAY routes via glvnd to libEGL_mesa and needs no window or
;;;; GBM device -- ideal for FBO + glReadPixels. GBM on a DRM render node is the
;;;; fallback for drivers without surfaceless support.

(defpackage #:green-screen/egl
  (:use #:cl)
  (:nicknames #:gs-egl)
  (:export #:with-headless-gl #:make-headless-context #:destroy-context
           #:egl-context #:egl-context-display #:egl-context-ctx
           #:egl-context-width #:egl-context-height))

(in-package #:green-screen/egl)

(cffi:define-foreign-library libEGL
  (:unix (:or "libEGL.so.1" "libEGL.so"))
  (t (:default "libEGL")))

(unless (cffi:foreign-library-loaded-p 'libEGL)
  (cffi:use-foreign-library libEGL))

;;; ------------------------------------------------------------------
;;; EGL constants (from eglplatform.h / egl.h). EGLint attrib arrays are int32;
;;; EGLAttrib arrays (eglGetPlatformDisplay) are intptr_t.

(defconstant +egl-false+ 0)
(defconstant +egl-true+  1)
(defconstant +egl-none+  #x3038)
(defconstant +egl-no-context+ 0)
(defconstant +egl-no-surface+ 0)

(defconstant +egl-platform-surfaceless-mesa+ #x31DD)
(defconstant +egl-platform-gbm+              #x31D7)

(defconstant +egl-opengl-api+     #x30A2)

(defconstant +egl-surface-type+    #x3033)
(defconstant +egl-pbuffer-bit+     #x0001)
(defconstant +egl-renderable-type+ #x3040)
(defconstant +egl-opengl-bit+      #x0008)
(defconstant +egl-red-size+        #x3024)
(defconstant +egl-green-size+      #x3023)
(defconstant +egl-blue-size+       #x3022)
(defconstant +egl-alpha-size+      #x3021)
(defconstant +egl-depth-size+      #x3025)

(defconstant +egl-context-major-version+ #x3098)

;; eglQueryString names.
(defconstant +egl-vendor+     #x3053)
(defconstant +egl-version+    #x3054)
(defconstant +egl-extensions+ #x3055)

;;; ------------------------------------------------------------------
;;; Foreign functions. EGLBoolean/EGLenum are unsigned int; EGLint is int32;
;;; displays/configs/contexts are opaque pointers.

(cffi:defcfun ("eglGetError" egl-get-error) :int32)

(cffi:defcfun ("eglGetPlatformDisplay" egl-get-platform-display) :pointer
  (platform :uint32) (native-display :pointer) (attrib-list :pointer))

(cffi:defcfun ("eglInitialize" egl-initialize) :uint
  (dpy :pointer) (major :pointer) (minor :pointer))

(cffi:defcfun ("eglTerminate" egl-terminate) :uint
  (dpy :pointer))

(cffi:defcfun ("eglBindAPI" egl-bind-api) :uint
  (api :uint32))

(cffi:defcfun ("eglChooseConfig" egl-choose-config) :uint
  (dpy :pointer) (attrib-list :pointer) (configs :pointer)
  (config-size :int32) (num-config :pointer))

(cffi:defcfun ("eglCreateContext" egl-create-context) :pointer
  (dpy :pointer) (config :pointer) (share :pointer) (attrib-list :pointer))

(cffi:defcfun ("eglDestroyContext" egl-destroy-context) :uint
  (dpy :pointer) (ctx :pointer))

(cffi:defcfun ("eglMakeCurrent" egl-make-current) :uint
  (dpy :pointer) (draw :pointer) (read :pointer) (ctx :pointer))

(cffi:defcfun ("eglQueryString" egl-query-string) :string
  (dpy :pointer) (name :int32))

(cffi:defcfun ("eglReleaseThread" egl-release-thread) :uint)

;;; ------------------------------------------------------------------
;;; Helpers.

(defun egl-error-name (code)
  (case code
    (#x3000 "EGL_SUCCESS")     (#x3001 "EGL_NOT_INITIALIZED")
    (#x3002 "EGL_BAD_ACCESS")  (#x3003 "EGL_BAD_ALLOC")
    (#x3004 "EGL_BAD_ATTRIBUTE") (#x3005 "EGL_BAD_CONFIG")
    (#x3006 "EGL_BAD_CONTEXT") (#x3007 "EGL_BAD_CURRENT_SURFACE")
    (#x3008 "EGL_BAD_DISPLAY") (#x3009 "EGL_BAD_MATCH")
    (#x300A "EGL_BAD_NATIVE_PIXMAP") (#x300B "EGL_BAD_NATIVE_WINDOW")
    (#x300C "EGL_BAD_PARAMETER") (#x300D "EGL_BAD_SURFACE")
    (#x300E "EGL_CONTEXT_LOST")
    (t (format nil "0x~X" code))))

(defmacro check-egl (form what)
  "Signal a descriptive error if FORM returns EGL_FALSE."
  (let ((r (gensym)))
    `(let ((,r ,form))
       (when (= ,r +egl-false+)
         (error "~A failed: ~A" ,what (egl-error-name (egl-get-error))))
       ,r)))

(defun int-array (values)
  "Foreign int32 array of VALUES (already NONE-terminated by the caller)."
  (let ((arr (cffi:foreign-alloc :int32 :count (length values))))
    (loop for v in values for i from 0
          do (setf (cffi:mem-aref arr :int32 i) v))
    arr))

;;; ------------------------------------------------------------------
;;; Headless context lifecycle.

(defstruct egl-context display config ctx width height)

(defun make-headless-context (width height)
  "Create a windowless OpenGL context sized WIDTH x HEIGHT and make it current.
Return an EGL-CONTEXT. Tries EGL_PLATFORM_SURFACELESS_MESA first."
  (let ((dpy (egl-get-platform-display +egl-platform-surfaceless-mesa+
                                       (cffi:null-pointer) (cffi:null-pointer))))
    (when (cffi:null-pointer-p dpy)
      (error "eglGetPlatformDisplay(SURFACELESS_MESA) returned EGL_NO_DISPLAY: ~A"
             (egl-error-name (egl-get-error))))
    (cffi:with-foreign-objects ((major :int32) (minor :int32))
      (check-egl (egl-initialize dpy major minor) "eglInitialize")
      (format t "  [egl] display up: EGL ~D.~D vendor=~A~%"
              (cffi:mem-ref major :int32) (cffi:mem-ref minor :int32)
              (egl-query-string dpy +egl-vendor+)))
    (check-egl (egl-bind-api +egl-opengl-api+) "eglBindAPI(OpenGL)")
    (let ((cfg-attrs (int-array (list +egl-surface-type+    +egl-pbuffer-bit+
                                      +egl-renderable-type+ +egl-opengl-bit+
                                      +egl-red-size+   8
                                      +egl-green-size+ 8
                                      +egl-blue-size+  8
                                      +egl-alpha-size+ 8
                                      +egl-depth-size+ 0
                                      +egl-none+)))
          (ctx-attrs (int-array (list +egl-context-major-version+ 3 +egl-none+))))
      (unwind-protect
           (cffi:with-foreign-objects ((configs :pointer 1) (n :int32))
             (check-egl (egl-choose-config dpy cfg-attrs configs 1 n)
                        "eglChooseConfig")
             (when (zerop (cffi:mem-ref n :int32))
               (error "eglChooseConfig matched 0 configs"))
             (let* ((config (cffi:mem-aref configs :pointer 0))
                    (ctx (egl-create-context dpy config (cffi:null-pointer) ctx-attrs)))
               (when (cffi:null-pointer-p ctx)
                 (error "eglCreateContext failed: ~A"
                        (egl-error-name (egl-get-error))))
               ;; Surfaceless: current with no draw/read surface; we render to an FBO.
               (check-egl (egl-make-current dpy (cffi:null-pointer)
                                            (cffi:null-pointer) ctx)
                          "eglMakeCurrent(surfaceless)")
               (make-egl-context :display dpy :config config :ctx ctx
                                 :width width :height height)))
        (cffi:foreign-free cfg-attrs)
        (cffi:foreign-free ctx-attrs)))))

(defun destroy-context (c)
  "Tear down the EGL context/display. Best-effort, safe to call once."
  (when c
    (let ((dpy (egl-context-display c)))
      (ignore-errors
       (egl-make-current dpy (cffi:null-pointer) (cffi:null-pointer)
                         (cffi:null-pointer)))
      (ignore-errors (egl-destroy-context dpy (egl-context-ctx c)))
      (ignore-errors (egl-terminate dpy))
      (ignore-errors (egl-release-thread)))))

(defmacro with-headless-gl ((ctx width height) &body body)
  "Bind CTX to a fresh headless GL context of WIDTH x HEIGHT for BODY, then
tear it down unconditionally (AGENTS.md teardown discipline)."
  `(let ((,ctx (make-headless-context ,width ,height)))
     (unwind-protect (locally ,@body)   ; locally permits a leading (declare ...)
       (destroy-context ,ctx))))
