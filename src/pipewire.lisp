;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
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
  (record-p nil) (record-dir nil) (streamed-p nil)
  ;; record-start is nil until the first frame actually arrives (the clock starts
  ;; then, so negotiation latency doesn't shorten the clip -- bead am4.7).
  (record-start nil) (record-duration 0) (record-min-dt 0) (record-last nil)
  (record-budget 0) (max-frames 0) (n-saved 0) (frames '())
  ;; pause/resume (SIGUSR1): while paused, frames aren't saved and paused time is
  ;; excluded from the timeline (record-start is shifted on resume).
  (paused nil) (pause-start nil)
  ;; streaming encoder (am4.18): frames piped to ffmpeg -> compressed intermediate
  (want-encoder t)
  (enc-proc nil) (enc-stream nil) (enc-fifo nil) (intermediate nil) (enc-fps 30)
  (done nil) (error nil) (n-empty 0))

(defvar *cap* nil)

;;; ------------------------------------------------------------------
;;; Pause/resume (green-screen-8ok). SIGUSR1 toggles: while paused we drop frames
;;; and, on resume, shift record-start forward by the paused span so the saved
;;; timeline is gapless and the paused time doesn't count against the duration cap.

(defvar *record-cap* nil "The capture currently recording, for the SIGUSR1 toggle.")
(defvar *sigusr1-installed* nil)

(defun %toggle-pause ()
  "Flip the active recording's pause state (SIGUSR1 handler)."
  (let ((cap *record-cap*))
    (when (and cap (capture-record-start cap))
      (let ((now (get-internal-real-time)))
        (if (capture-paused cap)
            (progn                       ; resume: shift the clock past the pause
              (when (capture-pause-start cap)
                (incf (capture-record-start cap) (- now (capture-pause-start cap)))
                (setf (capture-record-last cap) now))
              (setf (capture-paused cap) nil (capture-pause-start cap) nil)
              (format t "~&  [pw] resumed~%") (finish-output))
            (progn                       ; pause
              (setf (capture-paused cap) t (capture-pause-start cap) now)
              (format t "~&  [pw] paused (SIGUSR1 to resume)~%") (finish-output)))))))

(defun ensure-pause-handler ()
  "Install the SIGUSR1 pause toggle once."
  (unless *sigusr1-installed*
    (sb-sys:enable-interrupt sb-unix:sigusr1
                             (lambda (sig info ctx) (declare (ignore sig info ctx))
                               (%toggle-pause)))
    (setf *sigusr1-installed* t)))

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
  ;; C-stack callback: firewall so a condition can't unwind into PipeWire
  ;; (green-screen-zqb.3).
  (when *cap*
   (handler-case
    (progn
    (when (and (= state +pw-stream-state-error+) (not (cffi:null-pointer-p error)))
      (setf (capture-error *cap*) (cffi:foreign-string-to-lisp error)))
    (format t "  [pw] state -> ~A~@[ (~A)~]~%"
            (cond ((= state +pw-stream-state-streaming+) "streaming")
                  ((= state +pw-stream-state-paused+) "paused")
                  ((= state +pw-stream-state-connecting+) "connecting")
                  ((= state +pw-stream-state-unconnected+) "unconnected")
                  ((= state +pw-stream-state-error+) "error")
                  (t state))
            (capture-error *cap*))
    ;; Stop-on-user-end (am4.10): once we've been streaming, dropping back to
    ;; unconnected/error means the user ended the share (e.g. clicked GNOME's
    ;; Stop button) or the compositor tore it down -> finish the recording.
    (let ((cap *cap*))
      (cond
        ((= state +pw-stream-state-streaming+)
         (setf (capture-streamed-p cap) t))
        ((and (capture-record-p cap) (capture-streamed-p cap)
              (or (= state +pw-stream-state-unconnected+)
                  (= state +pw-stream-state-error+)))
         (format t "  [pw] share ended -> stopping recording~%")
         (setf (capture-done cap) t)
         (when (capture-loop cap) (pw-main-loop-quit (capture-loop cap)))))))
    (serious-condition (e)
      (ignore-errors
       (format *error-output* "  [pw] state-changed error: ~A~%" e))))))

(cffi:defcallback cb-param-changed :void
    ((data :pointer) (id :uint32) (param :pointer))
  (declare (ignore data))
  ;; This runs on PipeWire's C stack: a Lisp condition must never unwind across
  ;; the callback boundary (undefined behaviour), so firewall the whole body and
  ;; record the failure for the driver to surface (green-screen-zqb.3).
  (when (and *cap* (= id +spa-param-format+) (not (cffi:null-pointer-p param)))
    (handler-case
        (multiple-value-bind (w h fmt) (parse-format param)
          (setf (capture-width *cap*) w (capture-height *cap*) h (capture-format *cap*) fmt)
          (let* ((stride (* w 4))
                 (size (* stride h))
                 (bufpod nil) (curpod nil) (arr nil))
            (setf (capture-stride *cap*) stride)
            ;; Allocate under unwind-protect so a signal mid-setup can't leak the
            ;; foreign PODs (green-screen-zqb.5).
            (unwind-protect
                 (progn
                   (setf bufpod (octets->foreign (build-buffers-pod stride size))
                         curpod (octets->foreign (build-cursor-meta-pod))
                         arr    (cffi:foreign-alloc :pointer :count 2))
                   (setf (cffi:mem-aref arr :pointer 0) bufpod
                         (cffi:mem-aref arr :pointer 1) curpod)
                   (setf (capture-update-rc *cap*)
                         (pw-stream-update-params (capture-stream *cap*) arr 2)))
              (when arr    (cffi:foreign-free arr))
              (when bufpod (cffi:foreign-free bufpod))
              (when curpod (cffi:foreign-free curpod)))
            (format t "  [pw] negotiated ~Dx~D fmt=~D~%" w h fmt)))
      (serious-condition (e)
        (setf (capture-error *cap*) (format nil "param-changed: ~A" e))
        (ignore-errors
         (format *error-output* "  [pw] param-changed error: ~A~%" e))))))

(defun %frame->vector (dptr coff csize)
  "Copy CSIZE bytes at DPTR+COFF into a fresh (unsigned-byte 8) vector."
  (let ((vec (make-array csize :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (dst vec)
      (cffi:foreign-funcall "memcpy" :pointer dst
                            :pointer (ptr+ dptr coff) :size csize :pointer))
    vec))

(defun %write-frame-raw (path dptr coff csize)
  "memcpy CSIZE bytes at DPTR+COFF and write them to PATH (raw BGRx)."
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence (%frame->vector dptr coff csize) s)))

;;; ------------------------------------------------------------------
;;; Give-up cap: if the compositor only ever hands us empty / dmabuf-only
;;; buffers (no mapped pixels) we would otherwise spin forever -- capture-one-frame
;;; has no deadline. Bail after this many empties *before the first real frame*
;;; with a clear error instead of hanging (green-screen-zqb.6).
(defconstant +max-empty-buffers+ 600)

;;; Map a negotiated SPA video format to the ffmpeg rawvideo pixel format that
;;; describes the same byte order. Both the streaming encoder and the still-frame
;;; path feed ffmpeg raw bytes, so it must be told the *actual* layout or colours
;;; swap (green-screen-zqb.1).
(defun format->ffmpeg-pixfmt (fmt)
  (cond ((= fmt +spa-video-format-bgrx+) "bgr0")
        ((= fmt +spa-video-format-rgbx+) "rgb0")
        ((= fmt +spa-video-format-bgra+) "bgra")
        ((= fmt +spa-video-format-rgba+) "rgba")
        ((= fmt +spa-video-format-xrgb+) "0rgb")
        ((= fmt +spa-video-format-argb+) "argb")
        (t "bgr0")))

;;; The compositor uploads raw frames as a GL texture and only distinguishes the
;;; two 32bpp byte orders it can name: :rgba (R-G-B-A/X) and :bgra (B-G-R-A/X).
;;; The raw-file capture fallback stores frames in the *negotiated* format, so it
;;; must tell the compositor which order they are, or colours swap on an RGB-order
;;; source (green-screen-f5b). NIL / anything exotic (ARGB/XRGB, which a screencast
;;; portal never negotiates) falls back to :bgra, the common Wayland order.
(defun format->gl-source-format (fmt)
  (if (and (integerp fmt)
           (or (= fmt +spa-video-format-rgbx+) (= fmt +spa-video-format-rgba+)))
      :rgba
      :bgra))

;;; ------------------------------------------------------------------
;;; Streaming encoder: pipe raw BGRx frames to ffmpeg via a FIFO -> a near-
;;; lossless h264 intermediate, so long recordings don't hoard gigabytes of raw
;;; frames (green-screen-am4.18). Best-effort: on any failure the caller falls
;;; back to per-frame raw files.

(defun %has-encoder (name)
  (let ((out (with-output-to-string (s)
               (ignore-errors
                (uiop:run-program (list "ffmpeg" "-hide_banner" "-encoders")
                                  :output s :error-output nil :ignore-error-status t)))))
    (and (search name out) t)))

(defun %pick-encoder ()
  (cond ((%has-encoder "libx264") "libx264")
        ((%has-encoder "libopenh264") "libopenh264")
        (t nil)))

(defun %start-encoder (cap w h)
  "Launch ffmpeg reading rawvideo BGRx WxH from a FIFO into a compressed
intermediate; store the pieces in CAP. Return T on success, NIL to fall back."
  (let ((enc (%pick-encoder)))
    (when enc
      (handler-case
          (let* ((base (capture-record-dir cap))
                 (fifo (format nil "~A/frames.fifo" base))
                 (out  (format nil "~A/source.mp4" base))
                 (fps  (capture-enc-fps cap))
                 (bitrate (min 40000000 (max 8000000 (round (* w h fps 0.2))))))
            (ignore-errors (delete-file fifo))
            (ignore-errors (delete-file out))
            (uiop:run-program (list "mkfifo" fifo))
            (let ((proc (uiop:launch-program
                         (list "ffmpeg" "-y" "-loglevel" "error"
                               "-f" "rawvideo"
                               "-pix_fmt" (format->ffmpeg-pixfmt (capture-format cap))
                               "-s" (format nil "~Dx~D" w h)
                               "-r" (format nil "~D" fps) "-i" fifo
                               "-c:v" enc "-b:v" (format nil "~D" bitrate)
                               "-pix_fmt" "yuv420p" out)
                         :output nil :error-output nil)))
              (handler-case
                  ;; opening the write end (of the existing FIFO -- hence
                  ;; :if-exists) unblocks ffmpeg's FIFO read open. Only commit the
                  ;; encoder fields once this succeeds, so a failure leaves nothing
                  ;; dangling. Time-box the open: if ffmpeg never opens the read end
                  ;; (bad args, missing lib) the FIFO open would block forever, so
                  ;; treat a stall as failure and fall back to raw (green-screen-zqb.4).
                  (let ((stream (handler-case
                                    (sb-ext:with-timeout 10
                                      (open fifo :direction :output
                                                 :element-type '(unsigned-byte 8)
                                                 :if-exists :append))
                                  (sb-ext:timeout ()
                                    (error "encoder FIFO open timed out (ffmpeg did not start?)")))))
                    (setf (capture-enc-proc cap) proc
                          (capture-enc-fifo cap) fifo
                          (capture-intermediate cap) out
                          (capture-enc-stream cap) stream)
                    t)
                (error ()
                  ;; don't leave ffmpeg blocked forever on the FIFO
                  (ignore-errors (uiop:terminate-process proc :urgent t))
                  (ignore-errors (delete-file fifo))
                  (ignore-errors (delete-file out))
                  nil))))
        (error () nil)))))

(defun %stop-encoder (cap)
  "Flush and close the encoder FIFO (EOF), wait for ffmpeg, remove the FIFO."
  (when (capture-enc-stream cap)
    (ignore-errors (finish-output (capture-enc-stream cap)))
    (ignore-errors (close (capture-enc-stream cap)))
    (setf (capture-enc-stream cap) nil))
  (when (capture-enc-proc cap)
    ;; After EOF, ffmpeg only has to flush the trailer -- normally instant. Cap the
    ;; wait so a wedged encoder can't hang teardown forever (green-screen-zqb.4).
    (let ((proc (capture-enc-proc cap)))
      (ignore-errors
       (handler-case (sb-ext:with-timeout 30 (uiop:wait-process proc))
         (sb-ext:timeout ()
           (ignore-errors (uiop:terminate-process proc :urgent t))
           (ignore-errors (uiop:wait-process proc))))))
    (setf (capture-enc-proc cap) nil))
  (when (capture-enc-fifo cap)
    (ignore-errors (delete-file (capture-enc-fifo cap)))
    (setf (capture-enc-fifo cap) nil)))

(defun %record-frame (cap spabuf dptr csize cstride coff)
  "Recording-mode buffer handler: save throttled frames + per-frame cursor until
DURATION elapses from the first frame, then quit the loop. The caller (CB-PROCESS)
owns requeuing the buffer, so this never touches it."
  (let ((now (get-internal-real-time)))
    ;; Start the clock on the first real frame so DURATION is wall-clock of
    ;; actual capture, not shortened by portal/PipeWire negotiation (am4.7).
    (unless (capture-record-start cap)
      (setf (capture-record-start cap) now)
      ;; geometry from the real chunk (needed before starting the encoder)
      (when (> cstride 0)
        (setf (capture-stride cap) cstride
              (capture-width cap)  (floor cstride 4)
              (capture-height cap) (floor csize cstride)))
      ;; Prefer streaming to a compressed intermediate (am4.18) -- then frames are
      ;; small, so we capture at full rate for the whole duration. If it can't
      ;; start, fall back to raw files bounded by the disk budget and spread over
      ;; the duration (am4.17), keeping full-res frames but lowering the rate.
      (when (capture-want-encoder cap)
        (%start-encoder cap (capture-width cap) (capture-height cap)))
      (if (capture-enc-stream cap)
          (format t "  [pw] streaming to compressed intermediate -> ~A~%"
                  (capture-intermediate cap))
          (let* ((fbytes (max 1 csize))
                 (cap-frames (max 1 (floor (capture-record-budget cap) fbytes)))
                 (dur-units  (capture-record-duration cap))
                 ;; With no time cap, don't spread frames across a duration -- just
                 ;; run at MAX-FPS until the disk-budget frame count is reached.
                 (spread-dt  (if (and dur-units (plusp dur-units))
                                 (floor dur-units cap-frames) 0)))
            (setf (capture-max-frames cap) cap-frames
                  (capture-record-min-dt cap)
                  (max (capture-record-min-dt cap) spread-dt))
            (format t "  [pw] no encoder -- disk budget: up to ~D full-res frames ~
                       (~,1F fps~@[ over ~,0Fs~])~%"
                    cap-frames
                    (/ internal-time-units-per-second
                       (float (max 1 (capture-record-min-dt cap)) 1.0))
                    (when dur-units (/ dur-units internal-time-units-per-second 1.0))))))
    ;; Paused: drop the frame (no save, no deadline check -- paused time is
    ;; excluded; record-start is shifted forward on resume).
    (when (capture-paused cap)
      (return-from %record-frame))
    (let ((deadline (when (capture-record-duration cap)
                      (+ (capture-record-start cap) (capture-record-duration cap)))))
     (cond
      ;; safety caps: the optional max duration elapsed, or the disk-budget frame
      ;; backstop was hit (4K frames are ~37MB each). Normal stop is the user ending
      ;; the share (cb-state-changed). With no --duration, DEADLINE is NIL.
      ((or (and deadline (>= now deadline))
           (and (plusp (capture-max-frames cap))
                (>= (capture-n-saved cap) (capture-max-frames cap))))
       (setf (capture-done cap) t)
       (pw-main-loop-quit (capture-loop cap)))
      ((or (null (capture-record-last cap))
           (>= (- now (capture-record-last cap)) (capture-record-min-dt cap)))
       (multiple-value-bind (cx cy) (read-cursor spabuf)
         (let* ((i    (capture-n-saved cap))
                (enc  (capture-enc-stream cap))
                (path (unless enc
                        (format nil "~A/frame-~5,'0D.bgrx" (capture-record-dir cap) i))))
           (if enc                          ; stream to the encoder, else a raw file
               (ignore-errors (write-sequence (%frame->vector dptr coff csize) enc))
               (%write-frame-raw path dptr coff csize))
           (push (list :i i
                       :time (/ (float (- now (capture-record-start cap)) 1.0d0)
                                internal-time-units-per-second)
                       :cursor-x cx :cursor-y cy :path path)
                 (capture-frames cap))
           (when cx (setf (capture-cursor-meta-seen cap) t))
           (incf (capture-n-saved cap))
           (setf (capture-record-last cap) now))))
      (t nil)))))   ; throttled: skip this frame

(defun %process-buffer (cap b)
  "Handle one dequeued buffer B. Never requeues -- CB-PROCESS owns that, so the
buffer is returned to the pool on every path including a signalled error."
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
       (%record-frame cap spabuf dptr csize cstride coff))
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
            (pw-main-loop-quit (capture-loop cap)))
           ;; Got pixels but no cursor metadata yet -- wait for more frames.
           (t
            (decf (capture-frames-left cap))))))
      (t
       ;; screencast often emits a few empty buffers first; skip them. But if we
       ;; ONLY ever get empty / dmabuf-only buffers (no mapped pixels), the single-
       ;; frame path has no deadline and record mode's deadline lives in
       ;; %record-frame (never reached), so bound it: give up with an error once no
       ;; real frame has arrived after +MAX-EMPTY-BUFFERS+ (green-screen-zqb.6).
       (incf (capture-n-empty cap))
       (when (and (zerop (capture-n-saved cap))
                  (>= (capture-n-empty cap) +max-empty-buffers+))
         (setf (capture-error cap)
               (format nil "no usable frame after ~D empty buffers (dmabuf-only source?)"
                       (capture-n-empty cap))
               (capture-done cap) t)
         (pw-main-loop-quit (capture-loop cap)))))))

(cffi:defcallback cb-process :void ((data :pointer))
  (declare (ignore data))
  ;; Runs on PipeWire's C stack. Dequeue here, hand the buffer to %PROCESS-BUFFER
  ;; inside a firewall so no Lisp condition unwinds across the callback boundary,
  ;; and requeue in the cleanup so the buffer always returns to the pool exactly
  ;; once -- even on error (green-screen-zqb.3).
  (let ((cap *cap*))
    (when cap
      (let* ((stream (capture-stream cap))
             (b (pw-stream-dequeue-buffer stream)))
        (unless (cffi:null-pointer-p b)
          (unwind-protect
               (handler-case (%process-buffer cap b)
                 (serious-condition (e)
                   (setf (capture-error cap) (format nil "process: ~A" e))
                   (ignore-errors
                    (format *error-output* "  [pw] process error: ~A~%" e))
                   ;; a broken stream won't fix itself -- stop rather than spin
                   (setf (capture-done cap) t)
                   (ignore-errors (pw-main-loop-quit (capture-loop cap)))))
            (ignore-errors (pw-stream-queue-buffer stream b))))))))

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
      (%stop-encoder cap)               ; finalise the intermediate (no-op if unused)
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

(defun %free-bytes (dir)
  "Best-effort free bytes on DIR's filesystem via df; NIL if it can't be read."
  (ignore-errors
    (let* ((out (with-output-to-string (s)
                  (uiop:run-program (list "df" "-B1" "--output=avail" dir)
                                    :output s :error-output nil :ignore-error-status t)))
           (lines (remove "" (uiop:split-string out :separator '(#\Newline)) :test #'string=)))
      (parse-integer (string-trim " " (car (last lines))) :junk-allowed t))))

(defun record-frames (fd node-id &key (duration nil) (max-fps 30)
                                      (disk-budget nil)
                                      (audio nil)
                                      (dir "/tmp/takesy-rec"))
  "Record from NODE-ID into DIR until the user ends the share (stream drops after
streaming -- cb-state-changed quits the loop) or, when DURATION is non-NIL, that
many seconds elapse as a safety cap (NIL = no time cap; the disk budget still bounds
raw-fallback captures). Frames
are full-res BGRx (~37MB each at 4K), so the number kept is bounded by DISK-BUDGET
bytes and spread across the whole DURATION (the effective rate drops below MAX-FPS
when needed) -- so a busy screen fills the clip, not the disk in a few seconds.
AUDIO, when non-nil (:system | :mic | :both), records a parallel audio track over
the same window (best-effort) and stores its path as :audio in the manifest.
Return a plist (:width :height :stride :format :fps :dir :frames :audio)."
  (ensure-directories-exist (concatenate 'string (string-right-trim "/" dir) "/"))
  ;; clear stale frames from a previous recording so disk use stays bounded
  (dolist (f (directory (merge-pathnames "frame-*.bgrx"
                                         (concatenate 'string (string-right-trim "/" dir) "/"))))
    (ignore-errors (delete-file f)))
  ;; default budget: 70% of free space (best-effort), capped at 12 GB.
  (let ((budget (min 12000000000
                     (or disk-budget
                         (let ((free (%free-bytes dir)))
                           (if free (round (* 0.70 free)) 8000000000))))))
   (multiple-value-bind (mloop ctx stream) (%open-stream fd node-id)
    (let* ((cap (make-capture :loop mloop :stream stream
                              :record-p t
                              :record-dir (string-right-trim "/" dir)
                              ;; clock + frame budget are finalised on the first
                              ;; frame in %record-frame (size known then)
                              :record-duration
                              (when duration
                                (round (* duration internal-time-units-per-second)))
                              :record-budget budget
                              :record-min-dt
                              (round (/ internal-time-units-per-second (max 1 max-fps)))))
           ;; Parallel audio recorder (best-effort) spanning the capture window.
           (audio-handle (when audio (takesy/audio:start-audio dir audio))))
      ;; SIGUSR1 pauses/resumes this capture (kill -USR1 <pid>).
      (ensure-pause-handler)
      (setf *record-cap* cap)
      (format t "  [pw] pause/resume: kill -USR1 ~D~%" (sb-unix:unix-getpid))
      (unwind-protect
           (%run-and-teardown mloop ctx stream node-id cap)
        (setf *record-cap* nil))
      (let ((recording
              (list :width (capture-width cap) :height (capture-height cap)
                    :stride (capture-stride cap) :format (capture-format cap)
                    :fps max-fps :dir (capture-record-dir cap)
                    :cursor-meta (capture-cursor-meta-seen cap)
                    :intermediate (capture-intermediate cap)  ; nil if raw fallback
                    ;; stop-audio finalises the wav and validates it has data;
                    ;; NIL when audio was off or produced nothing.
                    :audio (when audio-handle (takesy/audio:stop-audio audio-handle))
                    :frames (nreverse (capture-frames cap)))))
        ;; Persist a manifest so the recording dir is self-contained and can be
        ;; reloaded later (load-recording) by the orchestrator / CLI.
        (with-open-file (s (format nil "~A/manifest.sexp" (capture-record-dir cap))
                           :direction :output :if-exists :supersede
                           :if-does-not-exist :create)
          (with-standard-io-syntax (prin1 recording s)))
        recording)))))

(defun load-recording (dir)
  "Read the manifest written by RECORD-FRAMES back into a recording plist."
  (with-open-file (s (format nil "~A/manifest.sexp" (string-right-trim "/" dir)))
    (with-standard-io-syntax (read s))))

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
