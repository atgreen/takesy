;;;; portal-screencast.lisp
;;;;
;;;; Feasibility spike (bead green-screen-csb): drive the xdg-desktop-portal
;;;; ScreenCast interface over the Common Lisp `dbus' library and obtain a live
;;;; PipeWire node id for the selected screen/window.
;;;;
;;;; This proves the "reused half" of the capture path. Running it pops the
;;;; desktop's screen-share dialog; pick a monitor or window to proceed.
;;;;
;;;; The portal uses a Request/Response pattern: each of CreateSession /
;;;; SelectSources / Start returns a Request object path immediately, and the
;;;; real result arrives later as a `Response' signal (u response, a{sv} results)
;;;; on that path. We add a match rule for those signals and wait for each one.

(defpackage #:green-screen/spike
  (:use #:cl)
  (:local-nicknames (#:d #:dbus)
                    (#:dc #:dbus/connections))
  (:export #:run))

(in-package #:green-screen/spike)

(defparameter +portal-dest+ "org.freedesktop.portal.Desktop")
(defparameter +portal-path+ "/org/freedesktop/portal/desktop")
(defparameter +screencast-iface+ "org.freedesktop.portal.ScreenCast")
(defparameter +request-iface+ "org.freedesktop.portal.Request")
(defparameter +session-iface+ "org.freedesktop.portal.Session")

;;; cursor_mode bitmask for SelectSources. Default to EMBEDDED: the compositor
;;; keeps drawing the hardware cursor and composites it into frames, so it can
;;; never be left hidden. METADATA hides the HW cursor (handing it to us as
;;; data) and MUST be paired with the Session.Close teardown below -- see
;;; AGENTS.md hazard #1. Only pass +cursor-metadata+ for the guarded cursor test.
(defparameter +cursor-embedded+ 2)
(defparameter +cursor-metadata+ 4)

;;; ------------------------------------------------------------------
;;; a{sv} option-dict helpers.
;;;
;;; In this library an a{sv} is a list of dict entries, each entry a
;;; (key variant) pair, and a variant is a (signature value) pair.
;;; e.g. ("handle_token" ("s" "gs0")) ("multiple" ("b" nil)).

(defun v (sig value) (list sig value))

(defun sv (&rest pairs)
  "PAIRS is (key sig value key sig value ...). Return an a{sv} dict."
  (loop for (key sig value) on pairs by #'cdddr
        collect (list key (v sig value))))

(let ((counter 0))
  (defun token ()
    "A fresh handle_token, unique within this process run."
    (format nil "gs~D" (incf counter))))

;;; ------------------------------------------------------------------
;;; Request/Response waiting.

(defun sender-token (bus)
  "Our unique bus name, munged for portal request object paths:
strip the leading colon and replace dots with underscores."
  (let ((name (d:bus-name bus)))
    (substitute #\_ #\. (string-left-trim ":" name))))

(defun request-path (bus handle-token)
  "The Request object path the portal will emit the Response signal on."
  (format nil "/org/freedesktop/portal/desktop/request/~A/~A"
          (sender-token bus) handle-token))

(defun response-match-p (msg expected-path)
  (and (typep msg 'd:signal-message)
       (equal (d:message-member msg) "Response")
       (equal (d:message-interface msg) +request-iface+)
       (equal (d:message-path msg) expected-path)))

(defun wait-for-response (bus expected-path)
  "Block until the portal Response signal for EXPECTED-PATH arrives;
return its body (response-code results-dict).

The connection is event-driven: prior invoke-method calls pump the socket
via an io-handler that stashes messages in CONNECTION-PENDING-MESSAGES, so
the signal may already be queued there. Check the queue first, then pump
the event loop one cycle at a time (exactly as WAIT-FOR-REPLY does) until
it shows up."
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
  "Invoke a ScreenCast method synchronously; return its method-return body."
  (d:invoke-method (d:bus-connection bus) member
                   :path +portal-path+
                   :interface +screencast-iface+
                   :destination +portal-dest+
                   :signature signature
                   :arguments arguments))

(defun call-portal/request (bus member signature build-args)
  "Call a Request-pattern portal method and wait for its Response.
BUILD-ARGS is a function of the handle-token returning the argument list
\(so the token can be embedded in the options dict). Returns the results
dict on success, or signals an error on non-zero response."
  (let* ((tok (token))
         (expected (request-path bus tok))
         (args (funcall build-args tok))
         ;; invoke-method returns (values-list body); a Request-pattern call
         ;; returns a single object path, so the primary value IS the path.
         (returned-path (call-portal bus member signature args)))
    ;; Modern portals return the same object path we predicted; prefer theirs.
    (let ((path (or returned-path expected)))
      (destructuring-bind (code results) (wait-for-response bus path)
        (case code
          (0 results)
          (1 (error "~A cancelled by user." member))
          (t (error "~A failed with response code ~A." member code)))))))

(defun close-session (bus session)
  "Best-effort org.freedesktop.portal.Session.Close on the SESSION object path.
Called from the teardown path so Mutter cleanly restores cursor/input state
instead of us relying on the D-Bus connection dropping (AGENTS.md hazard #1)."
  (when session
    (handler-case
        (progn
          (d:invoke-method (d:bus-connection bus) "Close"
                           :path session
                           :interface +session-iface+
                           :destination +portal-dest+
                           :signature ""
                           :arguments '())
          (format t "      Session.Close sent for ~A~%" session))
      (error (e)
        (format t "      WARNING: Session.Close failed: ~A~%" e)))))

(defun dict-get (dict key)
  "Look up KEY in an a{sv} results dict; return the unwrapped value.
On decode a variant unpacks to its bare value, so an entry is (key value)."
  (let ((entry (assoc key dict :test #'equal)))
    (when entry
      (second entry))))

;;; ------------------------------------------------------------------
;;; The handshake.

(defun run (&key (cursor-mode +cursor-embedded+))
  "Drive the portal ScreenCast handshake and capture one frame to PNG.

CURSOR-MODE defaults to +CURSOR-EMBEDDED+ (safe: the compositor keeps drawing
the hardware cursor). Pass +CURSOR-METADATA+ only for the guarded cursor test --
it hides the HW cursor and depends on the Session.Close teardown below to restore
Mutter's cursor/input state (AGENTS.md hazard #1)."
  (d:with-open-bus (bus (d:session-server-addresses))
    (format t "~&Connected to session bus as ~A~%" (d:bus-name bus))
    (when (= cursor-mode +cursor-metadata+)
      (format t "  !! cursor_mode=METADATA: the hardware cursor will hide during ~
                 capture; Session.Close teardown is armed.~%"))

    ;; Upgrade the connection so it can receive SCM_RIGHTS fds (bead sz0).
    ;; Do this before any of our own method calls so the read path is recvmsg
    ;; from here on.
    (green-screen/dbus-fd:enable-fd-passing bus)
    (format t "Connection upgraded for Unix-fd passing.~%")

    ;; Receive Response signals from the portal.
    (d:add-match bus :type "signal" :interface +request-iface+ :member "Response")

    ;; SESSION is hoisted so the cleanup form can always close it, even if the
    ;; handshake errors or the user cancels the picker dialog.
    (let ((session nil))
      (unwind-protect
           (progn
             ;; 1. CreateSession -------------------------------------------
             (format t "~&[1/4] CreateSession...~%")
             (let ((session-tok (token)))
               (setf session
                     (dict-get
                      (call-portal/request
                       bus "CreateSession" "a{sv}"
                       (lambda (tok)
                         (list (sv "handle_token"         "s" tok
                                   "session_handle_token" "s" session-tok))))
                      "session_handle")))
             (format t "      session_handle = ~A~%" session)

             ;; 2. SelectSources -------------------------------------------
             ;; types: 1=MONITOR 2=WINDOW 4=VIRTUAL (bitmask).
             (format t "~&[2/4] SelectSources (monitor|window, cursor_mode=~D)...~%"
                     cursor-mode)
             (call-portal/request
              bus "SelectSources" "oa{sv}"
              (lambda (tok)
                (list session
                      (sv "handle_token" "s" tok
                          "types"        "u" 3       ; MONITOR | WINDOW
                          "multiple"     "b" nil
                          "cursor_mode"  "u" cursor-mode))))

             ;; 3. Start ---------------------------------------------------
             ;; This is what pops the interactive picker dialog.
             (format t "~&[3/4] Start -- a screen-share dialog should appear; pick a source.~%")
             (let* ((results (call-portal/request
                              bus "Start" "osa{sv}"
                              (lambda (tok)
                                (list session "" (sv "handle_token" "s" tok)))))
                    (streams (dict-get results "streams"))
                    (node-id (first (first streams))))
               (format t "~%==> SUCCESS. PipeWire streams from portal:~%")
               (dolist (s streams)
                 ;; each stream is (node_id::u properties::a{sv})
                 (destructuring-bind (nid props) s
                   (format t "      node id = ~A   props = ~S~%" nid props)))

               ;; 4. OpenPipeWireRemote --------------------------------------
               ;; Returns the PipeWire fd via SCM_RIGHTS (bead green-screen-sz0).
               (format t "~&[4/4] OpenPipeWireRemote (retrieving real fd)...~%")
               (let ((fd (progn
                           (call-portal bus "OpenPipeWireRemote" "oa{sv}" (list session (sv)))
                           (green-screen/dbus-fd:take-fd bus))))
                 (format t "      captured fd = ~S (node ~A)~%" fd node-id)
                 (unless (and (integerp fd) (>= fd 0))
                   (error "OpenPipeWireRemote did not deliver a usable fd."))

                 ;; 5. Native PipeWire capture: one frame + cursor (bead jme).
                 (format t "~&[5/5] Capturing one frame via pure-Lisp libpipewire...~%")
                 (let ((info (green-screen/pipewire:capture-frame-to-png
                              fd node-id "/tmp/green-screen-frame.png")))
                   (format t "~%==> FRAME CAPTURED: ~Dx~D fmt=~A stride=~A~%"
                           (getf info :width) (getf info :height)
                           (getf info :format) (getf info :stride))
                   (if (getf info :cursor-x)
                       (format t "    cursor = (~A,~A) after ~D frame~:P~%"
                               (getf info :cursor-x) (getf info :cursor-y)
                               (getf info :frames-waited))
                       (format t "    cursor = none (meta-attached=~A, waited ~D frames)~%"
                               (getf info :cursor-meta-attached) (getf info :frames-waited)))
                   (format t "    PNG    = ~A~%" (getf info :png))))))
        ;; teardown -- ALWAYS close the portal session so Mutter cleanly
        ;; restores cursor/input state; never rely on the connection dropping.
        (close-session bus session))
      (values))))
