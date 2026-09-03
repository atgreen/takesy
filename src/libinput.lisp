;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;; libinput.lisp
;;;;
;;;; Continuous pointer-motion capture via libinput (green-screen: fix the "pointer
;;;; way off" on machines where the screencast delivers frames only on change).
;;;;
;;;; The portal/PipeWire cursor metadata is ABSOLUTE but gappy -- it rides on video
;;;; frames, which a compositor sends only when the screen changes, so moving the
;;;; mouse over a static window yields no cursor update. libinput reads the kernel
;;;; input devices directly (below Wayland's boundary), so it sees RELATIVE motion
;;;; continuously regardless of what the compositor draws. The render fuses the two:
;;;; PipeWire gives exact anchor positions, libinput gives the path between them.
;;;;
;;;; Best-effort throughout: needs read/write on /dev/input (the `input' group, same
;;;; as click capture); any failure yields an empty track and the render falls back
;;;; to holding the cursor across gaps.

(defpackage #:takesy/libinput
  (:use #:cl)
  (:export #:capture-pointer-motion #:available-p))

(in-package #:takesy/libinput)

(cffi:define-foreign-library libinput
  (:unix (:or "libinput.so.10" "libinput.so"))
  (t (:default "libinput")))

(defun available-p ()
  "T when libinput can be loaded (the shared library is present)."
  (handler-case
      (progn (unless (cffi:foreign-library-loaded-p 'libinput)
               (cffi:use-foreign-library libinput))
             t)
    (error () nil)))

;;; ------------------------------------------------------------------
;;; Foreign bindings (the small slice we need of libinput's path interface).

(cffi:defcfun ("libinput_path_create_context" li-path-create-context) :pointer
  (interface :pointer) (user-data :pointer))
(cffi:defcfun ("libinput_path_add_device" li-path-add-device) :pointer
  (li :pointer) (path :string))
(cffi:defcfun ("libinput_dispatch" li-dispatch) :int (li :pointer))
(cffi:defcfun ("libinput_get_event" li-get-event) :pointer (li :pointer))
(cffi:defcfun ("libinput_event_get_type" li-event-get-type) :int (ev :pointer))
(cffi:defcfun ("libinput_event_get_pointer_event" li-event-get-pointer) :pointer (ev :pointer))
(cffi:defcfun ("libinput_event_pointer_get_dx" li-pointer-dx) :double (pev :pointer))
(cffi:defcfun ("libinput_event_pointer_get_dy" li-pointer-dy) :double (pev :pointer))
(cffi:defcfun ("libinput_event_destroy" li-event-destroy) :void (ev :pointer))
(cffi:defcfun ("libinput_unref" li-unref) :pointer (li :pointer))

;; LIBINPUT_EVENT_POINTER_MOTION (relative motion, post-acceleration -- covers mice
;; and touchpads). See libinput.h; the pointer-event group starts at 400.
(defconstant +ev-pointer-motion+ 400)

;;; ------------------------------------------------------------------
;;; The libinput_interface: two callbacks that open/close device fds. We open
;;; O_RDWR (libinput may ioctl) but never grab, so the compositor and our evdev
;;; click reader keep working alongside.

(cffi:defcallback %open-restricted :int ((path :string) (flags :int) (user :pointer))
  (declare (ignore user))
  (handler-case (sb-posix:open path flags) (error () -1)))

(cffi:defcallback %close-restricted :void ((fd :int) (user :pointer))
  (declare (ignore user))
  (ignore-errors (sb-posix:close fd)))

(defun %make-interface ()
  "Allocate a libinput_interface {open_restricted, close_restricted}."
  (let ((iface (cffi:foreign-alloc :pointer :count 2)))
    (setf (cffi:mem-aref iface :pointer 0) (cffi:get-callback '%open-restricted)
          (cffi:mem-aref iface :pointer 1) (cffi:get-callback '%close-restricted))
    iface))

(defun %device-paths ()
  (sort (mapcar #'namestring (directory #p"/dev/input/event*")) #'string<))

;;; ------------------------------------------------------------------

(defun capture-pointer-motion (stop-fn)
  "Spawn a background thread reading libinput relative pointer motion until
(FUNCALL STOP-FN) is true. Return a thunk that joins the thread and yields the
motion track: a list of (INTERNAL-TIME DX DY), one entry per poll cycle in which
the pointer moved, INTERNAL-TIME from GET-INTERNAL-REAL-TIME (same clock as the
frame timeline). Best-effort -- NIL/empty on any failure."
  (if (not (available-p))
      (lambda () nil)
      (let* ((events (make-array 0 :adjustable t :fill-pointer 0))
             (lock (sb-thread:make-mutex :name "takesy-motion"))
             (thread
               (ignore-errors
                (sb-thread:make-thread
                 (lambda ()
                   (handler-case
                       (let ((iface (%make-interface)) (li nil))
                         (unwind-protect
                              (progn
                                (setf li (li-path-create-context iface (cffi:null-pointer)))
                                (when (and li (not (cffi:null-pointer-p li)))
                                  (dolist (dev (%device-paths))
                                    (ignore-errors (li-path-add-device li dev)))
                                  (loop until (funcall stop-fn) do
                                    (li-dispatch li)
                                    ;; coalesce all motion in this dispatch into one
                                    ;; (time dx dy) sample -- bounds the track size and
                                    ;; keeps a per-poll motion profile.
                                    (let ((sdx 0d0) (sdy 0d0) (moved nil))
                                      (loop for ev = (li-get-event li)
                                            until (or (null ev) (cffi:null-pointer-p ev))
                                            do (when (= (li-event-get-type ev) +ev-pointer-motion+)
                                                 (let ((pev (li-event-get-pointer ev)))
                                                   (incf sdx (li-pointer-dx pev))
                                                   (incf sdy (li-pointer-dy pev))
                                                   (setf moved t)))
                                               (li-event-destroy ev))
                                      (when moved
                                        (sb-thread:with-mutex (lock)
                                          (vector-push-extend
                                           (list (get-internal-real-time) sdx sdy) events))))
                                    (sleep 0.004))))
                           (when (and li (not (cffi:null-pointer-p li))) (ignore-errors (li-unref li)))
                           (ignore-errors (cffi:foreign-free iface))))
                     (error () nil)))
                 :name "takesy-libinput-motion"))))
        (lambda ()
          (when thread (ignore-errors (sb-thread:join-thread thread)))
          (sb-thread:with-mutex (lock) (coerce events 'list))))))
