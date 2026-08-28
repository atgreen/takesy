;;;; portal.lisp
;;;;
;;;; Bead green-screen-am4.3: the xdg-desktop-portal ScreenCast handshake as a
;;;; reusable library (promoted from spike/portal-screencast.lisp). Drives
;;;; CreateSession -> SelectSources -> Start -> OpenPipeWireRemote over the CL
;;;; `dbus' client and yields a PipeWire fd + node id, with the mandatory clean
;;;; teardown (Session.Close + connection scope) around it -- AGENTS.md hazard #1.

(defpackage #:takesy/portal
  (:use #:cl)
  (:local-nicknames (#:d #:dbus) (#:dc #:dbus/connections))
  (:export #:with-screencast #:+cursor-embedded+ #:+cursor-metadata+))

(in-package #:takesy/portal)

(defparameter +portal-dest+ "org.freedesktop.portal.Desktop")
(defparameter +portal-path+ "/org/freedesktop/portal/desktop")
(defparameter +screencast-iface+ "org.freedesktop.portal.ScreenCast")
(defparameter +request-iface+ "org.freedesktop.portal.Request")
(defparameter +session-iface+ "org.freedesktop.portal.Session")

;; cursor_mode: 1=HIDDEN 2=EMBEDDED 4=METADATA. EMBEDDED keeps the HW cursor
;; drawn (safe); METADATA hides it and hands us the position -- pair with clean
;; teardown only.
(defparameter +cursor-embedded+ 2)
(defparameter +cursor-metadata+ 4)

;;; ------------------------------------------------------------------
;;; a{sv} option-dict helpers (this dbus library: a{sv} is a list of
;;; (key (sig value)) entries).

(defun v (sig value) (list sig value))

(defun sv (&rest pairs)
  "PAIRS is (key sig value ...). Return an a{sv} dict."
  (loop for (key sig value) on pairs by #'cdddr
        collect (list key (v sig value))))

(let ((counter 0))
  (defun token () (format nil "tk~D" (incf counter))))

;;; ------------------------------------------------------------------
;;; Request/Response waiting.

(defun sender-token (bus)
  (substitute #\_ #\. (string-left-trim ":" (d:bus-name bus))))

(defun request-path (bus handle-token)
  (format nil "/org/freedesktop/portal/desktop/request/~A/~A"
          (sender-token bus) handle-token))

(defun response-match-p (msg expected-path)
  (and (typep msg 'd:signal-message)
       (equal (d:message-member msg) "Response")
       (equal (d:message-interface msg) +request-iface+)
       (equal (d:message-path msg) expected-path)))

(defun wait-for-response (bus expected-path)
  "Block until the portal Response signal for EXPECTED-PATH arrives; return its
body (response-code results-dict). Pumps the event loop like WAIT-FOR-REPLY."
  (let ((conn (d:bus-connection bus)))
    (loop
      (let ((hit (find-if (lambda (m) (response-match-p m expected-path))
                          (d:connection-pending-messages conn))))
        (when hit
          (setf (d:connection-pending-messages conn)
                (remove hit (d:connection-pending-messages conn)))
          (return (d:message-body hit))))
      (iolib:event-dispatch (dc:connection-event-base conn) :one-shot t))))

(defun call-portal (bus member signature arguments)
  (d:invoke-method (d:bus-connection bus) member
                   :path +portal-path+ :interface +screencast-iface+
                   :destination +portal-dest+
                   :signature signature :arguments arguments))

(defun call-portal/request (bus member signature build-args)
  "Call a Request-pattern portal method and wait for its Response. BUILD-ARGS is
a function of the handle-token returning the argument list. Return the results
dict on success; error otherwise."
  (let* ((tok (token))
         (expected (request-path bus tok))
         (args (funcall build-args tok))
         (returned-path (call-portal bus member signature args))
         (path (or returned-path expected)))
    (destructuring-bind (code results) (wait-for-response bus path)
      (case code
        (0 results)
        (1 (error "~A cancelled by user." member))
        (t (error "~A failed with response code ~A." member code))))))

(defun dict-get (dict key)
  (let ((entry (assoc key dict :test #'equal))) (when entry (second entry))))

(defun close-session (bus session)
  "Best-effort org.freedesktop.portal.Session.Close on SESSION, so Mutter cleanly
restores cursor/input state (never rely on the connection dropping)."
  (when session
    ;; If the user ended the share (GNOME Stop), the session is already gone and
    ;; Close errors with "does not exist" -- that's success (Mutter already
    ;; restored state), so keep it quiet; it's best-effort either way.
    (ignore-errors
     (d:invoke-method (d:bus-connection bus) "Close"
                      :path session :interface +session-iface+
                      :destination +portal-dest+ :signature "" :arguments '()))))

;;; ------------------------------------------------------------------
;;; The public entry: open a screencast, run BODY with (fd node-id) bound, and
;;; always tear the session down.

(defun %open-screencast (bus cursor-mode)
  "Run the handshake on an fd-passing-enabled BUS. Return (values fd node-id
session)."
  (d:add-match bus :type "signal" :interface +request-iface+ :member "Response")
  (let ((session
          (dict-get (call-portal/request
                     bus "CreateSession" "a{sv}"
                     (lambda (tok)
                       (list (sv "handle_token" "s" tok
                                 "session_handle_token" "s" (token)))))
                    "session_handle")))
    (call-portal/request
     bus "SelectSources" "oa{sv}"
     (lambda (tok)
       (list session (sv "handle_token" "s" tok
                         "types" "u" 3 "multiple" "b" nil
                         "cursor_mode" "u" cursor-mode))))
    (let* ((results (call-portal/request
                     bus "Start" "osa{sv}"
                     (lambda (tok) (list session "" (sv "handle_token" "s" tok)))))
           (node-id (first (first (dict-get results "streams")))))
      (call-portal bus "OpenPipeWireRemote" "oa{sv}" (list session (sv)))
      (let ((fd (takesy/dbus-fd:take-fd bus)))
        (unless (and (integerp fd) (>= fd 0))
          (error "OpenPipeWireRemote did not deliver a usable fd."))
        (values fd node-id session)))))

(defmacro with-screencast ((fd-var node-var &key (cursor-mode '+cursor-embedded+))
                           &body body)
  "Open a portal ScreenCast, bind FD-VAR and NODE-VAR, run BODY, and always close
the session afterward. CURSOR-MODE defaults to EMBEDDED; pass +cursor-metadata+
only when you need the cursor track (it hides the HW cursor -- teardown here
restores it). Pops the interactive share dialog."
  (let ((bus (gensym "BUS")) (session (gensym "SESSION")))
    `(d:with-open-bus (,bus (d:session-server-addresses))
       (takesy/dbus-fd:enable-fd-passing ,bus)
       ;; SESSION is set (not rebound) inside the protected form so the cleanup
       ;; always sees the real handle even on error/cancel.
       (let ((,session nil) (,fd-var nil) (,node-var nil))
         (declare (ignorable ,node-var))
         (unwind-protect
              (progn
                (multiple-value-setq (,fd-var ,node-var ,session)
                  (%open-screencast ,bus ,cursor-mode))
                ,@body)
           (close-session ,bus ,session))))))
