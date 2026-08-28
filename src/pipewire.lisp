;;;; pipewire.lisp
;;;;
;;;; Bead green-screen-jme: capture one frame (+ cursor position) from a portal
;;;; PipeWire node, in pure Lisp. Flow:
;;;;
;;;;   pw_init -> main_loop -> context -> connect_fd(portal fd)
;;;;   -> stream_new -> add_listener(events) -> stream_connect(node, EnumFormat)
;;;;   -> run loop:
;;;;        param_changed(Format) -> parse w/h/fmt -> update_params(Buffers+Cursor)
;;;;        process             -> dequeue -> copy pixels + cursor -> quit
;;;;   -> ffmpeg raw -> PNG

(in-package #:takesy/pipewire)

;;; ------------------------------------------------------------------
;;; Foreign read helpers.

(declaim (inline ptr+ u32@ i32@ ptr@))
(defun ptr+ (p n) (cffi:inc-pointer p n))
(defun u32@ (p off) (cffi:mem-ref (ptr+ p off) :uint32))
(defun i32@ (p off) (cffi:mem-ref (ptr+ p off) :int32))
(defun ptr@ (p off) (cffi:mem-ref (ptr+ p off) :pointer))
(defun round-up (n m) (* m (ceiling n m)))

;;; ------------------------------------------------------------------
;;; Capture state, shared with the C callbacks via a special var. One capture
;;; runs at a time on the loop thread, so a dynamic binding is sufficient.

(defstruct capture
  loop stream raw-path
  (width 0) (height 0) (format 0) (stride 0)
  cursor-x cursor-y
  (frames-left 120)                     ; budget to wait for cursor metadata
  (metas-seen 0) (cursor-meta-seen nil) (last-cursor-id nil)
  (update-rc nil) (meta-types nil)
  ;; recording mode (am4.1): capture many frames over a deadline instead of one.
  (record-p nil) (record-dir nil)
  ;; record-start is nil until the first frame actually arrives (the clock starts
  ;; then, so negotiation latency doesn't shorten the clip -- bead am4.7).
  (record-start nil) (record-duration 0) (record-min-dt 0) (record-last nil)
  (n-saved 0) (frames '())
  (done nil) (error nil) (n-empty 0))

(defvar *cap* nil)

;;; ------------------------------------------------------------------
;;; Parse a fixated Format object POD -> width, height, video-format.

(defun scalar-offset (pod off)
  "Offset of a prop's scalar value body. Compositors wrap fixated values in a
Choice(None), so unwrap it: skip choice_type+flags+child-header (16 bytes)."
  (let ((vtype (u32@ pod (+ off 12)))   ; value POD type
        (vbody (+ off 16)))             ; value POD body
    (if (= vtype +spa-type-choice+)
        (+ vbody 16)
        vbody)))

(defun parse-format (pod)
  (let ((end (+ 8 (u32@ pod 0)))   ; 8-byte header + body size
        (off 16)                   ; props begin after obj type+id
        (w 0) (h 0) (fmt 0))
    (loop while (< off end) do
      (let ((key   (u32@ pod off))
            (vsize (u32@ pod (+ off 8))))   ; value POD body size
        (cond ((= key +spa-format-video-format+)
               (setf fmt (u32@ pod (scalar-offset pod off))))
              ((= key +spa-format-video-size+)
               (let ((so (scalar-offset pod off)))
                 (setf w (u32@ pod so) h (u32@ pod (+ so 4))))))
        (incf off (round-up (+ 16 vsize) 8))))
    (values w h fmt)))

;;; ------------------------------------------------------------------
;;; Read SPA_META_Cursor position from a spa_buffer.

(defun read-cursor (spabuf)
  "Return (values x y meta-present-p cursor-id). META-PRESENT-P is true when a
SPA_META_Cursor is attached at all (even with id 0); x/y are non-nil only when
id /= 0 (valid cursor data)."
  (let ((n     (u32@ spabuf +off/spa-buffer/n-metas+))
        (metas (ptr@ spabuf +off/spa-buffer/metas+)))
    (dotimes (i n (values nil nil nil nil))
      (let ((m (ptr+ metas (* i +sz/spa-meta+))))
        (when (= (u32@ m +off/spa-meta/type+) +spa-meta-cursor+)
          (let* ((md (ptr@ m +off/spa-meta/data+))
                 (id (u32@ md +off/spa-meta-cursor/id+)))
            (return-from read-cursor
              (if (/= id 0)
                  (let ((pos (ptr+ md +off/spa-meta-cursor/position+)))
                    (values (i32@ pos +off/spa-point/x+)
                            (i32@ pos +off/spa-point/y+) t id))
                  (values nil nil t id)))))))))

;;; ------------------------------------------------------------------
;;; Callbacks.

(cffi:defcallback cb-state-changed :void
    ((data :pointer) (old :int) (state :int) (error :pointer))
  (declare (ignore data old))
  (when *cap*
    (when (and (= state +pw-stream-state-error+) (not (cffi:null-pointer-p error)))
      (setf (capture-error *cap*) (cffi:foreign-string-to-lisp error)))
    (format t "  [pw] state -> ~A~@[ (~A)~]~%"
            (cond ((= state +pw-stream-state-streaming+) "streaming")
                  ((= state +pw-stream-state-paused+) "paused")
                  ((= state +pw-stream-state-connecting+) "connecting")
                  ((= state +pw-stream-state-unconnected+) "unconnected")
                  ((= state +pw-stream-state-error+) "error")
                  (t state))
            (capture-error *cap*))))

(cffi:defcallback cb-param-changed :void
    ((data :pointer) (id :uint32) (param :pointer))
  (declare (ignore data))
  (when (and *cap* (= id +spa-param-format+) (not (cffi:null-pointer-p param)))
    ;; Dump the real fixated format POD for offline parser diagnosis.
    (let* ((total (+ 8 (u32@ param 0)))
           (v (make-array total :element-type '(unsigned-byte 8))))
      (cffi:with-pointer-to-vector-data (d v)
        (cffi:foreign-funcall "memcpy" :pointer d :pointer param :size total :pointer))
      (ignore-errors
       (with-open-file (s "/tmp/gs-format-param.bin" :direction :output
                                                     :element-type '(unsigned-byte 8)
                                                     :if-exists :supersede)
         (write-sequence v s))))
    (multiple-value-bind (w h fmt) (parse-format param)
      (setf (capture-width *cap*) w (capture-height *cap*) h (capture-format *cap*) fmt)
      (let* ((stride (* w 4))
             (size (* stride h))
             (bufpod (octets->foreign (build-buffers-pod stride size)))
             (curpod (octets->foreign (build-cursor-meta-pod)))
             (arr (cffi:foreign-alloc :pointer :count 2)))
        (setf (capture-stride *cap*) stride)
        (setf (cffi:mem-aref arr :pointer 0) bufpod
              (cffi:mem-aref arr :pointer 1) curpod)
        (setf (capture-update-rc *cap*)
              (pw-stream-update-params (capture-stream *cap*) arr 2))
        (cffi:foreign-free arr)
        (cffi:foreign-free bufpod)
        (cffi:foreign-free curpod)
        (format t "  [pw] negotiated ~Dx~D fmt=~D~%" w h fmt)))))

(defun %write-frame-raw (path dptr coff csize)
  "memcpy CSIZE bytes at DPTR+COFF and write them to PATH (raw BGRx)."
  (let ((vec (make-array csize :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (dst vec)
      (cffi:foreign-funcall "memcpy" :pointer dst
                            :pointer (ptr+ dptr coff) :size csize :pointer))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence vec s))))

(defun %record-frame (cap stream b spabuf dptr csize cstride coff)
  "Recording-mode buffer handler: save throttled frames + per-frame cursor until
DURATION elapses from the first frame, then quit the loop. Always requeues."
  (let ((now (get-internal-real-time)))
    ;; Start the clock on the first real frame so DURATION is wall-clock of
    ;; actual capture, not shortened by portal/PipeWire negotiation (am4.7).
    (unless (capture-record-start cap)
      (setf (capture-record-start cap) now))
    (let ((deadline (+ (capture-record-start cap) (capture-record-duration cap))))
     (cond
      ((>= now deadline)
       (setf (capture-done cap) t)
       (pw-stream-queue-buffer stream b)
       (pw-main-loop-quit (capture-loop cap)))
      ((or (null (capture-record-last cap))
           (>= (- now (capture-record-last cap)) (capture-record-min-dt cap)))
       (when (> cstride 0)                 ; chunk geometry is ground truth
         (setf (capture-stride cap) cstride
               (capture-width cap)  (floor cstride 4)
               (capture-height cap) (floor csize cstride)))
       (multiple-value-bind (cx cy) (read-cursor spabuf)
         (let* ((i    (capture-n-saved cap))
                (path (format nil "~A/frame-~5,'0D.bgrx" (capture-record-dir cap) i)))
           (%write-frame-raw path dptr coff csize)
           (push (list :i i
                       :time (/ (float (- now (capture-record-start cap)) 1.0d0)
                                internal-time-units-per-second)
                       :cursor-x cx :cursor-y cy :path path)
                 (capture-frames cap))
           (when cx (setf (capture-cursor-meta-seen cap) t))
           (incf (capture-n-saved cap))
           (setf (capture-record-last cap) now)))
       (pw-stream-queue-buffer stream b))
      (t (pw-stream-queue-buffer stream b))))))   ; throttled: skip this frame

(cffi:defcallback cb-process :void ((data :pointer))
  (declare (ignore data))
  (let* ((cap *cap*)
         (stream (capture-stream cap))
         (b (pw-stream-dequeue-buffer stream)))
    (unless (cffi:null-pointer-p b)
      (let* ((spabuf (ptr@ b +off/pw-buffer/buffer+))
             (data0  (ptr@ spabuf +off/spa-buffer/datas+))
             (chunk  (ptr@ data0 +off/spa-data/chunk+))
             (dptr   (ptr@ data0 +off/spa-data/data+))
             (csize   (u32@ chunk +off/spa-chunk/size+))
             (cstride (i32@ chunk +off/spa-chunk/stride+))
             (coff    (u32@ chunk +off/spa-chunk/offset+)))
        (cond
          ((and (capture-record-p cap)
                (not (cffi:null-pointer-p dptr)) (> csize 0))
           (%record-frame cap stream b spabuf dptr csize cstride coff))
          ((and (not (cffi:null-pointer-p dptr)) (> csize 0))
           (multiple-value-bind (cx cy present id) (read-cursor spabuf)
             (let ((nm (u32@ spabuf +off/spa-buffer/n-metas+))
                   (mp (ptr@ spabuf +off/spa-buffer/metas+)))
               (setf (capture-metas-seen cap) (max (capture-metas-seen cap) nm)
                     (capture-meta-types cap)
                     (loop for i below nm
                           collect (u32@ (ptr+ mp (* i +sz/spa-meta+))
                                         +off/spa-meta/type+))))
             (when present
               (setf (capture-cursor-meta-seen cap) t (capture-last-cursor-id cap) id))
             (cond
               ;; Finish once we have the cursor, or the budget is spent.
               ((or cx (<= (capture-frames-left cap) 0))
                (when (> cstride 0)   ; chunk geometry is ground truth (32bpp)
                  (setf (capture-stride cap) cstride
                        (capture-width cap)  (floor cstride 4)
                        (capture-height cap) (floor csize cstride)))
                (let ((vec (make-array csize :element-type '(unsigned-byte 8))))
                  (cffi:with-pointer-to-vector-data (dst vec)
                    (cffi:foreign-funcall "memcpy" :pointer dst
                                          :pointer (ptr+ dptr coff) :size csize :pointer))
                  (with-open-file (s (capture-raw-path cap) :direction :output
                                                            :element-type '(unsigned-byte 8)
                                                            :if-exists :supersede)
                    (write-sequence vec s)))
                (setf (capture-cursor-x cap) cx (capture-cursor-y cap) cy
                      (capture-done cap) t)
                (pw-stream-queue-buffer stream b)
                (pw-main-loop-quit (capture-loop cap)))
               ;; Got pixels but no cursor metadata yet -- wait for more frames.
               (t
                (decf (capture-frames-left cap))
                (pw-stream-queue-buffer stream b)))))
          (t
           ;; screencast often emits a few empty buffers first; skip them.
           (incf (capture-n-empty cap))
           (pw-stream-queue-buffer stream b)))))))

;;; ------------------------------------------------------------------
;;; Events struct assembly.

(defun make-stream-events ()
  (let ((ev (cffi:foreign-alloc :uint8 :count +sz/pw-stream-events+)))
    (dotimes (i +sz/pw-stream-events+) (setf (cffi:mem-aref ev :uint8 i) 0))
    (setf (cffi:mem-ref (ptr+ ev +off/pw-stream-events/version+) :uint32)
          +pw-version-stream-events+)
    (setf (cffi:mem-ref (ptr+ ev +off/pw-stream-events/state-changed+) :pointer)
          (cffi:get-callback 'cb-state-changed))
    (setf (cffi:mem-ref (ptr+ ev +off/pw-stream-events/param-changed+) :pointer)
          (cffi:get-callback 'cb-param-changed))
    (setf (cffi:mem-ref (ptr+ ev +off/pw-stream-events/process+) :pointer)
          (cffi:get-callback 'cb-process))
    ev))

;;; ------------------------------------------------------------------
;;; Driver.

(defun %open-stream (fd node-id)
  "pw_init + main loop + context + connect_fd(portal fd) + stream_new.
Return (values mloop ctx stream)."
  (pw-init (cffi:null-pointer) (cffi:null-pointer))
  (let* ((mloop (pw-main-loop-new (cffi:null-pointer)))
         (l     (pw-main-loop-get-loop mloop))
         (ctx   (pw-context-new l (cffi:null-pointer) 0))
         (core  (pw-context-connect-fd ctx fd (cffi:null-pointer) 0)))
    (when (cffi:null-pointer-p core)
      (error "pw_context_connect_fd failed on fd ~D" fd))
    (let* ((props  (make-properties "media.type" "Video"
                                    "media.category" "Capture"
                                    "media.role" "Screen"))
           (stream (pw-stream-new core "takesy-capture" props)))
      (values mloop ctx stream))))

(defun %run-and-teardown (mloop ctx stream node-id cap)
  "Bind *cap*, add the events listener, connect STREAM to NODE-ID with our
EnumFormat, run the loop, then tear everything down under unwind-protect so a
callback error or quit never leaks the stream or leaves PipeWire initialized
(AGENTS.md hazard #1)."
  (let ((hook   (cffi:foreign-alloc :uint8 :count +sz/spa-hook+))
        (events (make-stream-events)))
    (dotimes (i +sz/spa-hook+) (setf (cffi:mem-aref hook :uint8 i) 0))
    (unwind-protect
         (let ((*cap* cap))
           (pw-stream-add-listener stream hook events (cffi:null-pointer))
           (let* ((fmt-pod (octets->foreign (build-enum-format-pod)))
                  (params  (cffi:foreign-alloc :pointer :count 1))
                  (flags   (logior +pw-stream-flag-autoconnect+
                                    +pw-stream-flag-map-buffers+)))
             (setf (cffi:mem-aref params :pointer 0) fmt-pod)
             (pw-stream-connect stream +spa-direction-input+ node-id flags params 1)
             (cffi:foreign-free params)
             (cffi:foreign-free fmt-pod))
           (pw-main-loop-run mloop))
      (ignore-errors (pw-stream-disconnect stream))
      (pw-stream-destroy stream)
      (pw-context-destroy ctx)
      (pw-main-loop-destroy mloop)
      (cffi:foreign-free hook)
      (cffi:foreign-free events)
      (pw-deinit))))

(defun capture-one-frame (fd node-id raw-path)
  "Connect to NODE-ID over the portal FD, capture one frame to RAW-PATH.
Return the CAPTURE struct (width/height/format/stride/cursor)."
  (multiple-value-bind (mloop ctx stream) (%open-stream fd node-id)
    (let ((cap (make-capture :loop mloop :stream stream :raw-path raw-path)))
      (%run-and-teardown mloop ctx stream node-id cap)
      cap)))

(defun record-frames (fd node-id &key (duration 3.0) (max-fps 30)
                                      (dir "/tmp/takesy-rec"))
  "Record DURATION seconds from NODE-ID into DIR: one BGRx file per kept frame
(throttled to MAX-FPS) plus a per-frame cursor track. Return a plist:
  (:width :height :stride :format :fps :dir :frames), where :frames is a list of
  (:i :time :cursor-x :cursor-y :path) in capture order."
  (ensure-directories-exist (concatenate 'string (string-right-trim "/" dir) "/"))
  (multiple-value-bind (mloop ctx stream) (%open-stream fd node-id)
    (let* ((cap (make-capture :loop mloop :stream stream
                              :record-p t
                              :record-dir (string-right-trim "/" dir)
                              ;; clock starts on the first frame (%record-frame)
                              :record-duration
                              (round (* duration internal-time-units-per-second))
                              :record-min-dt
                              (round (/ internal-time-units-per-second (max 1 max-fps))))))
      (%run-and-teardown mloop ctx stream node-id cap)
      (let ((recording
              (list :width (capture-width cap) :height (capture-height cap)
                    :stride (capture-stride cap) :format (capture-format cap)
                    :fps max-fps :dir (capture-record-dir cap)
                    :cursor-meta (capture-cursor-meta-seen cap)
                    :frames (nreverse (capture-frames cap)))))
        ;; Persist a manifest so the recording dir is self-contained and can be
        ;; reloaded later (load-recording) by the orchestrator / CLI.
        (with-open-file (s (format nil "~A/manifest.sexp" (capture-record-dir cap))
                           :direction :output :if-exists :supersede
                           :if-does-not-exist :create)
          (with-standard-io-syntax (prin1 recording s)))
        recording))))

(defun load-recording (dir)
  "Read the manifest written by RECORD-FRAMES back into a recording plist."
  (with-open-file (s (format nil "~A/manifest.sexp" (string-right-trim "/" dir)))
    (with-standard-io-syntax (read s))))

(defun format->ffmpeg-pixfmt (fmt)
  (cond ((= fmt +spa-video-format-bgrx+) "bgr0")
        ((= fmt +spa-video-format-rgbx+) "rgb0")
        ((= fmt +spa-video-format-bgra+) "bgra")
        ((= fmt +spa-video-format-rgba+) "rgba")
        ((= fmt +spa-video-format-xrgb+) "0rgb")
        ((= fmt +spa-video-format-argb+) "argb")
        (t "bgr0")))

(defun capture-frame-to-png (fd node-id png-path)
  "Capture one frame from NODE-ID (via portal FD) and write PNG-PATH.
Return a plist describing the frame."
  (let* ((raw (format nil "~A.raw" png-path))
         (cap (capture-one-frame fd node-id raw)))
    (unless (capture-done cap)
      (error "no frame captured (empty buffers: ~D, error: ~A)"
             (capture-n-empty cap) (capture-error cap)))
    (let ((w (capture-width cap)) (h (capture-height cap)))
      (uiop:run-program
       (list "ffmpeg" "-y" "-loglevel" "error"
             "-f" "rawvideo"
             "-pix_fmt" (format->ffmpeg-pixfmt (capture-format cap))
             "-s" (format nil "~Dx~D" w h)
             "-i" raw
             "-frames:v" "1" png-path)
       :output t :error-output t)
      (format t "  [pw] cursor meta: attached=~A last-id=~A meta-types=~A update-rc=~A~%"
              (capture-cursor-meta-seen cap) (capture-last-cursor-id cap)
              (capture-meta-types cap) (capture-update-rc cap))
      (list :width w :height h :format (capture-format cap)
            :stride (capture-stride cap)
            :cursor-x (capture-cursor-x cap) :cursor-y (capture-cursor-y cap)
            :frames-waited (- 120 (capture-frames-left cap))
            :cursor-meta-attached (capture-cursor-meta-seen cap)
            :png png-path))))
