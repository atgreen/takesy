;;;; takesy.asd

(asdf:defsystem "takesy"
  :description "A Wayland-native screen recorder in Common Lisp."
  :depends-on ("takesy/dbus-fd" "takesy/pipewire"))

(asdf:defsystem "takesy/pipewire"
  :description "Pure-Lisp CFFI bindings to libpipewire-0.3 for frame capture."
  :depends-on ("cffi")
  :serial t
  :components ((:module "src"
                :components ((:file "pipewire-package")
                             (:file "pw-abi")
                             (:file "spa-pod")
                             (:file "pipewire")))))

(asdf:defsystem "takesy/dbus-fd"
  :description "SCM_RIGHTS Unix-fd passing extension for the CL dbus client."
  :depends-on ("dbus" "cffi" "flexi-streams")
  :serial t
  :components ((:module "src"
                :components ((:file "dbus-fd-passing")))))

(asdf:defsystem "takesy/keyframe"
  :description "Director keyframe timeline -- the compositor/director contract."
  :serial t
  :components ((:module "src"
                :components ((:file "keyframe")))))

(asdf:defsystem "takesy/director"
  :description "Post-processing Director: cursor easing + auto-zoom keyframing."
  :depends-on ("takesy/keyframe")
  :serial t
  :components ((:module "src"
                :components ((:file "director")))))

(asdf:defsystem "takesy/compositor"
  :description "Headless GL compositor: zoom/pan, background, rounded corners, shadow."
  :depends-on ("cffi" "cl-opengl" "takesy/keyframe")
  :serial t
  :components ((:module "src"
                :components ((:file "egl")
                             (:file "compositor")))))

(asdf:defsystem "takesy/demo"
  :description "End-to-end demo: Director auto-zoom timeline -> compositor mp4."
  :depends-on ("takesy/director" "takesy/compositor")
  :serial t
  :components ((:module "src"
                :components ((:file "demo")))))

(asdf:defsystem "takesy/cli"
  :description "takesy command-line entry point (build the executable with build.lisp)."
  :depends-on ("takesy/demo")
  :serial t
  :components ((:module "src"
                :components ((:file "cli")))))
