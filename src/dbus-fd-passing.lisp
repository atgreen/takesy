;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;; dbus-fd-passing.lisp
;;;;
;;;; Bead green-screen-sz0: teach the CL `dbus' client to actually RECEIVE
;;;; file descriptors passed via SCM_RIGHTS.
;;;;
;;;; The `dbus' library negotiates UNIX_FD passing during auth and parses the
;;;; `h' index in message bodies, but its transport reads the socket with plain
;;;; IOLib stream I/O -- no recvmsg, no control buffer -- so the kernel drops
;;;; the ancillary fds. OpenPipeWireRemote returns exactly such an fd.
;;;;
;;;; Strategy: after the normal authenticated bus is established, CHANGE-CLASS
;;;; its connection to FD-UNIX-CONNECTION, whose RECEIVE-MESSAGE-NO-HANG reads
;;;; bytes via recvmsg(2) with a control buffer, harvests any SCM_RIGHTS fds
;;;; into a FIFO queue, and decodes each message by handing the reused
;;;; DBUS/MESSAGES:DECODE-MESSAGE an in-memory slice. Writes are untouched
;;;; (they still go out through the IOLib socket).
;;;;
;;;; NOTE on fd<->message association: we keep a per-connection FIFO fd queue
;;;; rather than attaching fds to individual messages. Because the kernel
;;;; delivers passed fds in send order, FIFO order is correct; a caller pops
;;;; the fd right after the call it knows carries one (OpenPipeWireRemote).
;;;; Full per-message association (via the UNIX_FDS header) is a documented
;;;; follow-up -- see the bead.

(defpackage #:takesy/dbus-fd
  (:use #:cl)
  (:local-nicknames (#:d #:dbus)
                    (#:msg #:dbus/messages))
  (:export #:enable-fd-passing #:take-fd #:pending-fd-count))

(in-package #:takesy/dbus-fd)

;;; ------------------------------------------------------------------
;;; libc bindings (Linux). Let CFFI compute struct layout/alignment.

(cffi:defcstruct iovec
  (base :pointer)
  (len  :size))

(cffi:defcstruct msghdr
  (name       :pointer)
  (namelen    :uint32)
  (iov        :pointer)
  (iovlen     :size)
  (control    :pointer)
  (controllen :size)
  (flags      :int))

(cffi:defcstruct cmsghdr
  (len   :size)
  (level :int)
  (type  :int))

(cffi:defcstruct pollfd
  (fd      :int)
  (events  :short)
  (revents :short))

(cffi:defcfun ("recvmsg" %recvmsg) :long (fd :int) (msg :pointer) (flags :int))
(cffi:defcfun ("poll" %poll) :int (fds :pointer) (nfds :ulong) (timeout :int))

(defun errno ()
  (cffi:mem-ref (cffi:foreign-funcall "__errno_location" :pointer) :int))

(defconstant +sol-socket+ 1)
(defconstant +scm-rights+ 1)
(defconstant +msg-dontwait+ #x40)
(defconstant +msg-cmsg-cloexec+ #x40000000)
(defconstant +pollin+ 1)
(defconstant +eagain+ 11)
(defconstant +eintr+ 4)

(defun cmsg-align (n)
  "CMSG_ALIGN: round up to a multiple of sizeof(size_t) (8 on x86-64)."
  (logandc2 (+ n 7) 7))

(defparameter +cmsghdr-size+ (cffi:foreign-type-size '(:struct cmsghdr)))
;; CMSG_DATA offset = CMSG_ALIGN(sizeof(struct cmsghdr)).
(defparameter +cmsg-data-off+ (cmsg-align +cmsghdr-size+))
(defparameter +control-bytes+ 256) ; room for ~ (256-16)/4 fds

;;; ------------------------------------------------------------------
;;; The fd-capturing connection.

(defclass fd-unix-connection (dbus/transport-unix:unix-connection)
  ((rx  :accessor conn-rx
        :initform (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0))
   (fds :accessor conn-fds :initform '()))
  (:documentation "A unix D-Bus connection that receives SCM_RIGHTS fds."))

(defun enqueue-fds (conn fds)
  (when fds
    ;; FIFO: keep in arrival order.
    (setf (conn-fds conn) (nconc (conn-fds conn) fds))))

;;; ------------------------------------------------------------------
;;; recvmsg-based reader.

(defun poll-readable (fd)
  "Block until FD is readable (used to wait out EAGAIN)."
  (cffi:with-foreign-object (pfd '(:struct pollfd))
    (setf (cffi:foreign-slot-value pfd '(:struct pollfd) 'fd) fd
          (cffi:foreign-slot-value pfd '(:struct pollfd) 'events) +pollin+
          (cffi:foreign-slot-value pfd '(:struct pollfd) 'revents) 0)
    (loop
      (let ((r (%poll pfd 1 -1)))
        (cond ((>= r 0) (return))
              ((= (errno) +eintr+))            ; interrupted; retry
              (t (error "poll() failed, errno=~D" (errno))))))))

(defun harvest-fds (mh)
  "Walk the control buffer of MH and return a list of received fds."
  (let ((clen (cffi:foreign-slot-value mh '(:struct msghdr) 'controllen))
        (ctrl (cffi:foreign-slot-value mh '(:struct msghdr) 'control))
        (fds '()))
    (let ((off 0))
      (loop
        (when (> (+ off +cmsghdr-size+) clen) (return))
        (let* ((cmsg (cffi:inc-pointer ctrl off))
               (len   (cffi:foreign-slot-value cmsg '(:struct cmsghdr) 'len))
               (level (cffi:foreign-slot-value cmsg '(:struct cmsghdr) 'level))
               (type  (cffi:foreign-slot-value cmsg '(:struct cmsghdr) 'type)))
          (when (< len +cmsghdr-size+) (return))
          (when (and (= level +sol-socket+) (= type +scm-rights+))
            (let ((data (cffi:inc-pointer cmsg +cmsg-data-off+))
                  (nfd  (floor (- len +cmsg-data-off+) 4)))
              (dotimes (i nfd)
                (push (cffi:mem-aref data :int i) fds))))
          (let ((adv (cmsg-align len)))
            (when (zerop adv) (return))
            (incf off adv)))))
    (nreverse fds)))

(defun recv-chunk (conn &optional (want 65536))
  "One recvmsg into CONN's rx buffer; harvest fds. Returns :data, :eagain,
or :eof. All reads use a control buffer so passed fds are never dropped."
  (let ((fd (d:connection-fd conn)))
    (cffi:with-foreign-objects ((iov  '(:struct iovec))
                                (mh   '(:struct msghdr))
                                (buf  :uint8 want)
                                (ctrl :uint8 +control-bytes+))
      (setf (cffi:foreign-slot-value iov '(:struct iovec) 'base) buf
            (cffi:foreign-slot-value iov '(:struct iovec) 'len)  want)
      ;; zero msghdr, then fill.
      (dotimes (i (cffi:foreign-type-size '(:struct msghdr)))
        (setf (cffi:mem-aref mh :uint8 i) 0))
      (setf (cffi:foreign-slot-value mh '(:struct msghdr) 'name) (cffi:null-pointer)
            (cffi:foreign-slot-value mh '(:struct msghdr) 'namelen) 0
            (cffi:foreign-slot-value mh '(:struct msghdr) 'iov) iov
            (cffi:foreign-slot-value mh '(:struct msghdr) 'iovlen) 1
            (cffi:foreign-slot-value mh '(:struct msghdr) 'control) ctrl
            (cffi:foreign-slot-value mh '(:struct msghdr) 'controllen) +control-bytes+
            (cffi:foreign-slot-value mh '(:struct msghdr) 'flags) 0)
      (let ((n (%recvmsg fd mh (logior +msg-dontwait+ +msg-cmsg-cloexec+))))
        (cond
          ((> n 0)
           (enqueue-fds conn (harvest-fds mh))
           (let ((rx (conn-rx conn)))
             (dotimes (i n)
               (vector-push-extend (cffi:mem-aref buf :uint8 i) rx)))
           :data)
          ((= n 0) :eof)
          ((= (errno) +eagain+) :eagain)
          ((= (errno) +eintr+) (recv-chunk conn want))
          (t (error "recvmsg() failed, errno=~D" (errno))))))))

(defun fill-to (conn need &key allow-empty)
  "Block until CONN's rx buffer holds NEED bytes. If ALLOW-EMPTY and the
buffer is empty with nothing immediately available, return NIL (meaning: no
message pending). Otherwise return T once filled."
  (loop
    (when (>= (fill-pointer (conn-rx conn)) need) (return t))
    (ecase (recv-chunk conn)
      (:data)                                   ; progressed; loop
      (:eof (error 'end-of-file :stream conn))
      (:eagain
       (when (and allow-empty (zerop (fill-pointer (conn-rx conn))))
         (return nil))
       (poll-readable (d:connection-fd conn))))
    (setf allow-empty nil)))                    ; committed once we have bytes

;;; ------------------------------------------------------------------
;;; Message framing + decode (reusing the library's decoder).

(defun rd-u32 (vec off little-endian)
  (if little-endian
      (logior (aref vec off)
              (ash (aref vec (+ off 1)) 8)
              (ash (aref vec (+ off 2)) 16)
              (ash (aref vec (+ off 3)) 24))
      (logior (ash (aref vec off) 24)
              (ash (aref vec (+ off 1)) 16)
              (ash (aref vec (+ off 2)) 8)
              (aref vec (+ off 3)))))

(defun message-length (vec)
  "Given at least 16 header bytes, compute the full D-Bus message length.
Layout: [0]=endian [4..7]=body_length [12..15]=header-fields array length,
then pad to 8, then body."
  (let* ((little (= (aref vec 0) (char-code #\l)))
         (body-len   (rd-u32 vec 4 little))
         (fields-len (rd-u32 vec 12 little))
         (header-end (* 8 (ceiling (+ 16 fields-len) 8))))
    (+ header-end body-len)))

(defun decode-slice (vec end)
  "Decode one message occupying VEC[0, END) via the library's decoder."
  (flexi-streams:with-input-from-sequence (raw vec :end end)
    ;; latin-1 makes the stream bivalent (decode-message reads the endianness
    ;; byte as a char, everything else as bytes) with a 1:1 byte<->char map.
    (let ((s (flexi-streams:make-flexi-stream raw :external-format :latin-1)))
      (msg:decode-message s))))

(defmethod d:receive-message-no-hang ((conn fd-unix-connection))
  ;; No-hang contract: return NIL if nothing is pending; otherwise block until
  ;; a whole message is available and return it.
  (unless (fill-to conn 16 :allow-empty t)
    (return-from d:receive-message-no-hang nil))
  (let ((total (message-length (conn-rx conn))))
    (fill-to conn total)
    (let* ((rx (conn-rx conn))
           (message (decode-slice rx total)))
      ;; Drop the consumed bytes, keep the remainder.
      (setf (conn-rx conn)
            (make-array (- (fill-pointer rx) total)
                        :element-type '(unsigned-byte 8)
                        :adjustable t :fill-pointer t
                        :initial-contents (subseq rx total)))
      message)))

;;; ------------------------------------------------------------------
;;; Public API.

(defun enable-fd-passing (bus)
  "Upgrade an already-open, authenticated BUS so its connection can receive
SCM_RIGHTS fds. Returns BUS. Drains anything IOLib already buffered (in order)
before switching the read path to recvmsg, so no bytes are lost or reordered."
  (let* ((conn (d:bus-connection bus))
         ;; connection-socket is an internal reader (not exported by the
         ;; umbrella); we need the IOLib stream object to drain its buffer.
         (socket (dbus/connections::connection-socket conn)))
    (change-class conn 'fd-unix-connection)
    ;; Pull any bytes IOLib has already buffered/available into our rx buffer,
    ;; in order, as raw octets (LISTEN + READ-BYTE avoids utf-8 misdecoding).
    (loop while (listen socket)
          do (vector-push-extend (read-byte socket) (conn-rx conn)))
    bus))

(defun take-fd (bus)
  "Pop and return the oldest received fd (an integer), or NIL if none.
Call right after OpenPipeWireRemote to get the PipeWire fd."
  (pop (conn-fds (d:bus-connection bus))))

(defun pending-fd-count (bus)
  (length (conn-fds (d:bus-connection bus))))
