;;;; green-screen.asd

(asdf:defsystem "green-screen"
  :description "A Wayland-native screen recorder in Common Lisp."
  :depends-on ("green-screen/dbus-fd"))

(asdf:defsystem "green-screen/dbus-fd"
  :description "SCM_RIGHTS Unix-fd passing extension for the CL dbus client."
  :depends-on ("dbus" "cffi" "flexi-streams")
  :serial t
  :components ((:module "src"
                :components ((:file "dbus-fd-passing")))))
