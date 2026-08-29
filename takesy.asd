;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;; takesy.asd

(asdf:defsystem "takesy"
  :description "A screen recorder for modern Linux desktops (Wayland & X11), via xdg-desktop-portal + PipeWire."
  :depends-on ("takesy/cli")
  :build-operation "program-op"
  :build-pathname "takesy"
  :entry-point "takesy/cli:main")

(asdf:defsystem "takesy/pipewire"
  :description "Pure-Lisp CFFI bindings to libpipewire-0.3 for frame capture."
  :depends-on ("cffi" "takesy/audio")
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

(asdf:defsystem "takesy/portal"
  :description "xdg-desktop-portal ScreenCast handshake -> PipeWire fd + node id."
  :depends-on ("dbus" "takesy/dbus-fd")
  :serial t
  :components ((:module "src"
                :components ((:file "portal")))))

(asdf:defsystem "takesy/evdev"
  :description "Read Linux evdev clicks/keys into Director input-events."
  :depends-on ("takesy/director")
  :serial t
  :components ((:module "src"
                :components ((:file "evdev")))))

(asdf:defsystem "takesy/audio"
  :description "Opt-in audio capture: parallel ffmpeg PulseAudio recorder -> wav."
  :serial t
  :components ((:module "src"
                :components ((:file "audio")))))

(asdf:defsystem "takesy/record"
  :description "Record orchestration: portal capture -> Director -> compositor -> mp4."
  :depends-on ("takesy/portal" "takesy/pipewire" "takesy/director" "takesy/compositor"
               "takesy/audio" "takesy/evdev")
  :serial t
  :components ((:module "src"
                :components ((:file "record")))))

(asdf:defsystem "takesy/cli"
  :description "takesy command-line entry point (build the executable with `make`)."
  :depends-on ("takesy/record")
  :serial t
  :components ((:module "src"
                :components ((:file "cli")))))
