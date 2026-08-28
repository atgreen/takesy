;;;; evdev.lisp
;;;;
;;;; Bead green-screen-am4.6: read real click/key events from Linux evdev
;;;; (/dev/input/event*) into takesy/director input-events, for click-accurate
;;;; activity instead of the cursor-dwell inference. evdev gives us the TIMING of
;;;; presses (not absolute position); the focus still comes from the captured
;;;; cursor track at that time (detect-activity's cursor-at fallback), so buttons
;;;; carry no x/y here.
;;;;
;;;; Permissions: /dev/input/event* is root:input mode 0660, so reading needs the
;;;; `input' group (or root). read-evdev-events degrades to NIL when a device is
;;;; unreadable, and the orchestrator falls back to dwell -- auto-zoom still
;;;; works without elevated access.

(defpackage #:takesy/evdev
  (:use #:cl)
  (:local-nicknames (#:dir #:takesy/director))
  (:export #:events-from-octets #:read-evdev-events
           #:probe-readable-input-devices #:+input-event-size+))

(in-package #:takesy/evdev)

;;; input_event (64-bit Linux ABI): struct { s64 tv_sec; s64 tv_usec; u16 type;
;;; u16 code; s32 value; } -- 24 bytes, little-endian.
(defconstant +input-event-size+ 24)
(defconstant +ev-key+ 1)          ; type: key/button
(defconstant +btn-left+   #x110)
(defconstant +btn-right+  #x111)
(defconstant +btn-middle+ #x112)

(declaim (inline u16le s32le u64le))
(defun u16le (v o) (logior (aref v o) (ash (aref v (+ o 1)) 8)))
(defun u64le (v o) (loop for i below 8 sum (ash (aref v (+ o i)) (* 8 i))))
(defun s32le (v o)
  (let ((x (logior (aref v o) (ash (aref v (+ o 1)) 8)
                   (ash (aref v (+ o 2)) 16) (ash (aref v (+ o 3)) 24))))
    (if (>= x #x80000000) (- x #x100000000) x)))

(defun %button-kind (code)
  "Classify an EV_KEY code: :click for the main mouse buttons, :key for keyboard
keys (code < 0x100), NIL for anything else (BTN_TOOL_*, side buttons, ...)."
  (cond ((member code (list +btn-left+ +btn-right+ +btn-middle+)) :click)
        ((< code #x100) :key)
        (t nil)))

(defun events-from-octets (octets &key base)
  "Parse OCTETS (a byte vector of packed 24-byte input_event records) into a list
of takesy/director input-events for key/button PRESSES (value=1), in order. Times
are seconds relative to BASE, or to the first press if BASE is nil."
  (let ((n (floor (length octets) +input-event-size+))
        (b base) (out '()))
    (dotimes (k n (nreverse out))
      (let* ((o     (* k +input-event-size+))
             (type  (u16le octets (+ o 16)))
             (code  (u16le octets (+ o 18)))
             (value (s32le octets (+ o 20))))
        (when (and (= type +ev-key+) (= value 1))
          (let ((kind (%button-kind code))
                (tsec (+ (u64le octets o) (/ (u64le octets (+ o 8)) 1000000.0d0))))
            (when kind
              (when (null b) (setf b tsec))
              (push (dir:make-input-event :time (float (- tsec b) 1.0)
                                          :kind kind :x nil :y nil)
                    out))))))))

(defun probe-readable-input-devices ()
  "Return the /dev/input/event* device paths we can actually open for reading
(empty unless we're in the `input' group or root)."
  (remove-if-not
   (lambda (p) (ignore-errors
                (with-open-file (s p :element-type '(unsigned-byte 8)) (declare (ignore s)) t)))
   (directory #p"/dev/input/event*")))

(defun read-evdev-events (path duration)
  "Best-effort: poll device PATH for DURATION seconds and return the press events
(via EVENTS-FROM-OCTETS). Non-blocking: reads only records already available
(listen), so it never hangs on a quiet device. Returns NIL and warns if PATH
can't be opened (permissions) -- callers then fall back to dwell."
  (handler-case
      (with-open-file (stream path :element-type '(unsigned-byte 8) :direction :input)
        (let ((rec (make-array +input-event-size+ :element-type '(unsigned-byte 8)))
              (buf (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0))
              (deadline (+ (get-internal-real-time)
                           (round (* duration internal-time-units-per-second)))))
          (loop while (< (get-internal-real-time) deadline) do
            (if (listen stream)
                (when (= +input-event-size+ (read-sequence rec stream))
                  (loop for byte across rec do (vector-push-extend byte buf)))
                (sleep 0.005)))
          (events-from-octets buf)))
    (error (e)
      (format *error-output* "  [evdev] ~A not readable (~A); using cursor dwell.~%"
              path e)
      nil)))
