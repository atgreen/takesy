;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;; webcam.lisp
;;;;
;;;; Live webcam capture (green-screen-asp). The ScreenCast portal delivers the
;;;; screen only, so -- exactly like audio (audio.lisp) -- the webcam is a parallel
;;;; recorder: an ffmpeg v4l2 process writing <dir>/webcam.mp4 over the same
;;;; wall-clock window as the frame capture. The render stage already composites a
;;;; webcam clip as a circle picture-in-picture (compositor draw-webcam), so live
;;;; capture just has to produce that clip and record its path in the manifest.
;;;;
;;;; Everything here is best-effort: if no capture device resolves or ffmpeg won't
;;;; launch, we log and record the screen only rather than fail the run.

(defpackage #:takesy/webcam
  (:use #:cl)
  (:export #:start-webcam #:stop-webcam #:webcam-handle #:webcam-handle-path
           #:list-video-devices #:list-cameras #:resolve-device #:live-spec-p
           #:supports-mjpeg-p #:v4l2-input-args))

(in-package #:takesy/webcam)

(defun live-spec-p (spec)
  "T when SPEC asks for LIVE webcam capture: a /dev/video* node or \"auto\" (pick
the first working camera). Any other string is a pre-recorded file for the render
stage, not a live device."
  (and spec (stringp spec)
       (let ((s (string-trim " " spec)))
         (or (string-equal s "auto")
             (and (>= (length s) 11)
                  (string= (subseq s 0 10) "/dev/video"))))))

(defun list-video-devices ()
  "Sorted /dev/video* device node paths (may include non-capture metadata nodes)."
  (sort (mapcar #'namestring (directory #p"/dev/video*")) #'string<))

(defun %device-name (path)
  "Human-readable camera name for /dev/videoN via /sys/class/video4linux/videoN/name,
or NIL. No ioctl/v4l2-ctl needed."
  (let ((slash (position #\/ path :from-end t)))
    (when slash
      (let ((sys (format nil "/sys/class/video4linux/~A/name" (subseq path (1+ slash)))))
        (ignore-errors
         (when (probe-file sys)
           (with-open-file (s sys)
             (let ((line (read-line s nil nil)))
               (when line (string-trim '(#\Space #\Tab #\Newline #\Return) line))))))))))

(defun list-cameras ()
  "All /dev/video* nodes as (PATH . NAME) for a picker. Fast (no capture probe), so
it may include metadata companion nodes; the preview shows which ones give frames."
  (loop for p in (list-video-devices)
        collect (cons p (or (%device-name p) p))))

(defun %v4l2-capture-node-p (dev)
  "T when DEV is a real capture device: grab a single frame with ffmpeg and check
it exits cleanly. Bounded by a timeout so a wedged node can't hang the probe.
Skips metadata companion nodes (e.g. the /dev/video1 that shadows /dev/video0)."
  (ignore-errors
   (handler-case
       (sb-ext:with-timeout 4
         (zerop (nth-value 2
                 (uiop:run-program
                  (list "ffmpeg" "-hide_banner" "-loglevel" "error"
                        "-f" "v4l2" "-i" dev "-frames:v" "1" "-f" "null" "-")
                  :ignore-error-status t :output nil :error-output nil))))
     (sb-ext:timeout () nil))))

(defun supports-mjpeg-p (dev)
  "T when DEV advertises an MJPEG (compressed) v4l2 format. Capturing MJPEG lets a
USB webcam deliver full framerate, where the raw format is bandwidth-limited (and
often stuck at a low resolution). Bounded probe; NIL on error/timeout."
  (ignore-errors
   (handler-case
       (sb-ext:with-timeout 4
         (let ((out (with-output-to-string (s)
                      (uiop:run-program
                       (list "ffmpeg" "-hide_banner" "-f" "v4l2"
                             "-list_formats" "all" "-i" dev)
                       :output nil :error-output s :ignore-error-status t))))
           (and (search "mjpeg" (string-downcase out)) t)))
     (sb-ext:timeout () nil))))

(defun %v4l2-list-formats (dev)
  "Raw `ffmpeg -list_formats all` output for DEV (bounded probe), or \"\" on
error/timeout. The MJPEG line lists the supported sizes, e.g.
  ... mjpeg : Motion-JPEG : 1280x720 640x480 1920x1080 2592x1944"
  (or (ignore-errors
       (handler-case
           (sb-ext:with-timeout 4
             (with-output-to-string (s)
               (uiop:run-program (list "ffmpeg" "-hide_banner" "-f" "v4l2"
                                       "-list_formats" "all" "-i" dev)
                                 :output nil :error-output s :ignore-error-status t)))
         (sb-ext:timeout () "")))
      ""))

(defun %parse-mjpeg-sizes (out)
  "WxH tokens on the MJPEG line of OUT as (w . h) pairs, largest area first."
  (let ((line (find-if (lambda (l) (search "mjpeg" (string-downcase l)))
                       (uiop:split-string out :separator '(#\Newline))))
        (sizes '()))
    (when line
      (dolist (tok (uiop:split-string line :separator '(#\Space #\Tab #\Return)))
        (let ((x (position #\x tok)))
          (when x
            (let ((w (parse-integer tok :end x :junk-allowed t))
                  (h (parse-integer tok :start (1+ x) :junk-allowed t)))
              (when (and w h (plusp w) (plusp h))
                (pushnew (cons w h) sizes :test #'equal)))))))
    (sort sizes #'> :key (lambda (wh) (* (car wh) (cdr wh))))))

(defun %best-mjpeg-size (sizes &key (max-w 1920) (max-h 1080))
  "Largest SIZE within MAX-W x MAX-H, or NIL. Capped at 1080p: bigger MJPEG modes
strain USB bandwidth and CPU and are wasted on a small picture-in-picture inset."
  (find-if (lambda (wh) (and (<= (car wh) max-w) (<= (cdr wh) max-h))) sizes))

(defun v4l2-input-args (dev)
  "ffmpeg input options for capturing DEV: prefer MJPEG at the best resolution up
to 1080p -- full framerate on USB cameras, and far sharper than the ~640x480 the
driver hands out by default -- else the driver default (raw). Goes before the -i
on an ffmpeg v4l2 command line."
  (let ((formats (%v4l2-list-formats dev)))
    (when (search "mjpeg" (string-downcase formats))
      (append (list "-input_format" "mjpeg")
              (let ((size (%best-mjpeg-size (%parse-mjpeg-sizes formats))))
                (when size
                  (list "-video_size" (format nil "~Dx~D" (car size) (cdr size)))))))))

(defun resolve-device (spec)
  "Resolve a live SPEC to a concrete device path, or NIL. \"auto\" probes
/dev/video* for the first real capture node; an explicit /dev/videoN is trusted
if it exists (no probe)."
  (let ((s (and spec (string-trim " " spec))))
    (cond
      ((null s) nil)
      ((string-equal s "auto") (find-if #'%v4l2-capture-node-p (list-video-devices)))
      ((probe-file s) s)
      (t nil))))

(defstruct webcam-handle proc path device)

(defun start-webcam (dir spec &key (fps 30))
  "Launch an ffmpeg v4l2 recorder for the camera SPEC (a /dev/videoN path or
\"auto\") writing <DIR>/webcam.mp4, and return a WEBCAM-HANDLE, or NIL (logged)
when no device resolves or ffmpeg won't start."
  (let ((dev  (resolve-device spec))
        (path (format nil "~A/webcam.mp4" (string-right-trim "/" dir))))
    (if (null dev)
        (progn
          (format t "  [webcam] no usable capture device for ~S; recording screen only~%" spec)
          nil)
        (let* ((in-args (v4l2-input-args dev))   ; nil unless MJPEG (then incl. -video_size)
               (mjpeg-p (and in-args t))
               ;; MJPEG: copy the camera stream verbatim -- no generation loss and
               ;; no encode CPU during the concurrent 4K screen capture (the render
               ;; composites + encodes to H.264 once anyway). Without -c:v, ffmpeg
               ;; would default to mpeg4 and throw away quality. Raw fallback keeps
               ;; the prior default-codec behaviour.
               (out-args (if mjpeg-p
                             (list "-c:v" "copy")
                             (list "-pix_fmt" "yuv420p"))))
         (handler-case
            (let ((proc (uiop:launch-program
                         (append (list "ffmpeg" "-y" "-loglevel" "error" "-f" "v4l2")
                                 in-args
                                 (list "-framerate" (format nil "~D" fps) "-i" dev)
                                 out-args (list path))
                         :input :stream :output nil :error-output nil)))
              (format t "  [webcam] recording ~A ~A -> ~A~%"
                      dev
                      (let ((sz (member "-video_size" in-args :test #'string=)))
                        (cond ((and mjpeg-p sz) (format nil "(mjpeg ~A copy)" (second sz)))
                              (mjpeg-p "(mjpeg copy)")
                              (t "(raw)")))
                      path)
              (make-webcam-handle :proc proc :path path :device dev))
          (error (e)
            (format t "  [webcam] failed to start (~A); recording screen only~%" e)
            nil))))))

(defun %nonempty-mp4-p (path)
  "T if PATH exists and is larger than a bare container header (i.e. has frames)."
  (and (probe-file path)
       (ignore-errors
        (> (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s))
           1024))))

(defun stop-webcam (handle)
  "Gracefully stop the recorder so ffmpeg finalizes the mp4: write 'q' to its
stdin, then wait (bounded). Return the clip path if a webcam was actually
captured, else NIL. Best-effort -- never signals."
  (when handle
    ;; 'q' on ffmpeg's stdin is a graceful quit that flushes the container.
    (ignore-errors
     (let ((in (uiop:process-info-input (webcam-handle-proc handle))))
       (when in (write-char #\q in) (finish-output in) (close in))))
    ;; After 'q' ffmpeg just finalizes the mp4 -- bound the wait so a wedged
    ;; recorder can't hang capture teardown (mirrors audio's stop path).
    (let ((proc (webcam-handle-proc handle)))
      (ignore-errors
       (handler-case (sb-ext:with-timeout 15 (uiop:wait-process proc))
         (sb-ext:timeout ()
           (ignore-errors (uiop:terminate-process proc :urgent t))
           (ignore-errors (uiop:wait-process proc))))))
    (let ((path (webcam-handle-path handle)))
      (if (%nonempty-mp4-p path)
          path
          (progn (format t "  [webcam] no webcam captured (empty/missing clip)~%") nil)))))
