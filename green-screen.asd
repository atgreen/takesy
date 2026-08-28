;;;; green-screen.asd

(asdf:defsystem "green-screen"
  :description "A Wayland-native screen recorder in Common Lisp."
  :depends-on ("green-screen/dbus-fd" "green-screen/pipewire"))

(asdf:defsystem "green-screen/pipewire"
  :description "Pure-Lisp CFFI bindings to libpipewire-0.3 for frame capture."
  :depends-on ("cffi")
  :serial t
  :components ((:module "src"
                :components ((:file "pipewire-package")
                             (:file "pw-abi")
                             (:file "spa-pod")
                             (:file "pipewire")))))

(asdf:defsystem "green-screen/dbus-fd"
  :description "SCM_RIGHTS Unix-fd passing extension for the CL dbus client."
  :depends-on ("dbus" "cffi" "flexi-streams")
  :serial t
  :components ((:module "src"
                :components ((:file "dbus-fd-passing")))))
