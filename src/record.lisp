;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;; record.lisp
;;;;
;;;; Bead green-screen-am4.3: the record orchestration -- the glue that makes
;;;; `takesy record` real. Capture a clip (portal -> PipeWire), turn its cursor
;;;; track into an auto-zoom timeline (Director), and render the real captured
;;;; frames through the compositor to an mp4. No synthetic anything.

(defpackage #:takesy/record
  (:use #:cl)
  (:local-nicknames (#:portal #:takesy/portal) (#:pw #:takesy/pipewire)
                    (#:dir #:takesy/director) (#:comp #:takesy/compositor)
                    (#:kf #:takesy/keyframe)
                    (#:evdev #:takesy/evdev) (#:wc #:takesy/webcam)
                    (#:li #:takesy/libinput))
  (:export #:recording->session #:compute-damage #:compute-content-bbox
           #:compose-recording #:record-to-mp4 #:load-image-rgba
           #:capture-recording #:render-recording #:render-recording-dir))

(in-package #:takesy/record)

(defun %read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun load-image-rgba (path)
  "Decode image PATH (png/jpg/... anything ffmpeg reads) to (values rgba-bytes w h).
No PNG reader in-tree, so reuse ffmpeg: ffprobe for the dimensions, then ffmpeg to
rawvideo RGBA. Signals if the file is missing or the byte count doesn't match."
  (unless (probe-file path)
    (error "cursor image not found: ~A" path))
  (let* ((dims (uiop:run-program
                (list "ffprobe" "-v" "error" "-select_streams" "v:0"
                      "-show_entries" "stream=width,height" "-of" "csv=p=0:s=x" path)
                :output '(:string :stripped t)))
         (xpos (or (position #\x dims)
                   (error "could not read dimensions of ~A (ffprobe: ~S)" path dims)))
         (w    (parse-integer dims :end xpos))
         (h    (parse-integer dims :start (1+ xpos)))
         (raw  (format nil "/tmp/takesy-cursor-~A.raw" (sxhash (namestring (truename path))))))
    (unwind-protect
         (progn
           (uiop:run-program
            (list "ffmpeg" "-y" "-loglevel" "error" "-i" path
                  "-f" "rawvideo" "-pix_fmt" "rgba" raw))
           (let ((bytes (%read-file-bytes raw)))
             (unless (= (length bytes) (* w h 4))
               (error "cursor image ~A decoded to ~D bytes, expected ~D (~Dx~D)"
                      path (length bytes) (* w h 4) w h))
             (values bytes w h)))
      (ignore-errors (delete-file raw)))))

;;; ------------------------------------------------------------------
;;; Decoding the compressed capture intermediate (green-screen-am4.18). ffmpeg
;;; decodes it to rawvideo BGRx through a FIFO we read sequentially; the compose
;;; loop only ever needs non-decreasing source frames, so a forward stream (no
;;; seeking) suffices.

(defstruct decoder proc stream fifo nbytes (idx -1) buf)

(defun %probe-video (path)
  "Return (values width height fps) for video PATH via ffprobe."
  (let* ((out (uiop:run-program
               (list "ffprobe" "-v" "error" "-select_streams" "v:0"
                     "-show_entries" "stream=width,height,avg_frame_rate"
                     "-of" "csv=p=0:s=," path)
               :output '(:string :stripped t)))
         (parts (loop with start = 0
                      for pos = (position #\, out :start start)
                      collect (subseq out start pos)
                      while pos do (setf start (1+ pos))))
         (w (parse-integer (first parts)))
         (h (parse-integer (second parts)))
         (rate (third parts))               ; "num/den"
         (slash (position #\/ rate))
         (fps (if slash
                  (/ (parse-integer rate :end slash)
                     (max 1 (parse-integer rate :start (1+ slash))))
                  (parse-integer rate))))
    (values w h (float fps 1.0))))

(defun %media-duration (path)
  "Duration of media PATH in seconds via ffprobe, or NIL if it can't be read."
  (ignore-errors
   (let* ((s (uiop:run-program
              (list "ffprobe" "-v" "error" "-show_entries" "format=duration"
                    "-of" "default=nw=1:nk=1" path)
              :output '(:string :stripped t) :ignore-error-status t))
          (d (let ((*read-eval* nil)) (read-from-string s nil nil))))
     (when (realp d) (float d 1.0)))))

(defun %source-lead (src-path span)
  "Seconds SRC-PATH (a parallel audio/webcam recorder) began BEFORE the screen's
first frame: since all recorders stop together, its lead is its duration minus the
SPAN of the screen capture. Used to trim each source's pre-roll so it lines up with
the picture (green-screen: PiP/audio sync). NIL path or unreadable -> 0."
  (if src-path
      (let ((d (%media-duration src-path)))
        (if d (max 0.0 (- d (float span 1.0))) 0.0))
      0.0))

(defun %open-decoder (intermediate w h &optional (pix-fmt "bgra") fps)
  "Launch ffmpeg decoding INTERMEDIATE to a FIFO of rawvideo PIX-FMT frames; return
a DECODER. The read end is opened after launch so ffmpeg's blocking FIFO-write open
unblocks. When FPS is given, force constant-rate output at exactly FPS -- so a
variable-rate source (e.g. a webcam that dropped below its nominal rate) decodes to
the same frame cadence the caller indexes by, instead of ffmpeg silently padding to
the container's nominal rate (which plays back in slow motion). green-screen sync."
  (let* ((fifo (format nil "~A.dec.fifo" intermediate))
         (nbytes (* w h 4)))
    (ignore-errors (delete-file fifo))
    (uiop:run-program (list "mkfifo" fifo))
    (let ((proc (uiop:launch-program
                 (append (list "ffmpeg" "-y" "-loglevel" "error" "-i" intermediate)
                         (when fps (list "-fps_mode" "cfr" "-r" (format nil "~,5F" fps)))
                         (list "-f" "rawvideo" "-pix_fmt" pix-fmt fifo))
                 :output nil :error-output nil)))
      (make-decoder :proc proc
                    :stream (open fifo :direction :input :element-type '(unsigned-byte 8))
                    :fifo fifo :nbytes nbytes
                    :buf (make-array nbytes :element-type '(unsigned-byte 8))))))

(defun %decoder-frame (dec target-idx)
  "Advance DEC to source frame TARGET-IDX (>= current) and return its bytes."
  (loop while (< (decoder-idx dec) target-idx)
        do (let ((got (read-sequence (decoder-buf dec) (decoder-stream dec))))
             (when (< got (decoder-nbytes dec)) (return))   ; EOF -> hold last
             (incf (decoder-idx dec))))
  (decoder-buf dec))

(defun %close-decoder (dec)
  (when dec
    (ignore-errors (close (decoder-stream dec)))
    (ignore-errors (uiop:wait-process (decoder-proc dec)))
    (ignore-errors (delete-file (decoder-fifo dec)))))

(defun %map-frames (rec fn)
  "Call (FN i bytes) for each source frame in order. Reads from the compressed
intermediate (decoded forward) when present, else per-frame raw files. BYTES may
be a reused buffer -- copy what you need before the next call."
  (let* ((frames (coerce (getf rec :frames) 'vector))
         (n (length frames))
         (inter (getf rec :intermediate)))
    (if inter
        (let ((dec (%open-decoder inter (getf rec :width) (getf rec :height))))
          (unwind-protect
               (dotimes (i n) (funcall fn i (%decoder-frame dec i)))
            (%close-decoder dec)))
        (dotimes (i n)
          (funcall fn i (%read-file-bytes (getf (aref frames i) :path)))))))

(defun %frame-luma-grid (bytes w h gw gh)
  "Sample a GW x GH coarse luma grid (one pixel per cell) from a BGRx frame."
  (let ((g (make-array (* gw gh) :element-type 'fixnum)))
    (dotimes (cy gh g)
      (dotimes (cx gw)
        (let* ((px (min (1- w) (floor (* (+ cx 0.5) w) gw)))
               (py (min (1- h) (floor (* (+ cy 0.5) h) gh)))
               (i  (* 4 (+ (* py w) px))))
          (setf (aref g (+ (* cy gw) cx))     ; BGRx: B + 2G + R
                (+ (aref bytes i) (* 2 (aref bytes (+ i 1))) (aref bytes (+ i 2)))))))))

(defun compute-damage (rec &key (gw 64) (gh 40) (threshold 48))
  "Diff consecutive captured frames on a coarse GW x GH grid; return a damage
track: (time x0 y0 x1 y1) UV bbox of changed cells, per frame that changed. This
is what lets the Director size the zoom to real screen activity, not just the
cursor."
  (let* ((fv (coerce (getf rec :frames) 'vector))
         (w (getf rec :width)) (h (getf rec :height))
         (prev nil) (out '()))
    (%map-frames rec
      (lambda (i bytes)
        (let ((cur (%frame-luma-grid bytes w h gw gh)))
          (when prev
            (let (minx miny maxx maxy)
              (dotimes (cy gh)
                (dotimes (cx gw)
                  (let ((k (+ (* cy gw) cx)))
                    (when (> (abs (- (aref cur k) (aref prev k))) threshold)
                      (setf minx (if minx (min minx cx) cx) maxx (if maxx (max maxx cx) cx)
                            miny (if miny (min miny cy) cy) maxy (if maxy (max maxy cy) cy))))))
              (when minx
                (push (list (float (getf (aref fv i) :time) 1.0)
                            (/ minx (float gw 1.0)) (/ miny (float gh 1.0))
                            (/ (1+ maxx) (float gw 1.0)) (/ (1+ maxy) (float gh 1.0)))
                      out))))
          (setf prev cur))))
    (nreverse out)))

(defun %sample-px (bytes w h fx fy)
  "Sample the BGRx pixel at fractional (FX,FY). Return (values b g r)."
  (let* ((x (min (1- w) (floor (* fx w))))
         (y (min (1- h) (floor (* fy h))))
         (i (* 4 (+ (* y w) x))))
    (values (aref bytes i) (aref bytes (+ i 1)) (aref bytes (+ i 2)))))

(defun %bg-color (bytes w h)
  "Estimate the background as the mean of the four corner pixels. (values b g r)."
  (let ((bs 0) (gs 0) (rs 0))
    (dolist (c '((0.01 . 0.01) (0.99 . 0.01) (0.01 . 0.99) (0.99 . 0.99)))
      (multiple-value-bind (b g r) (%sample-px bytes w h (car c) (cdr c))
        (incf bs b) (incf gs g) (incf rs r)))
    (values (/ bs 4.0) (/ gs 4.0) (/ rs 4.0))))

(defun compute-content-bbox (rec &key (gw 80) (gh 50) (threshold 40)
                                      (min-cover 0.9) (margin 0.015) (samples 8))
  "UV bounding box of the actual content -- pixels differing from a uniform
background (e.g. an empty/black desktop around a window) -- unioned across up to
SAMPLES frames. Return (list x0 y0 x1 y1) with MARGIN, or NIL when content
already covers >= MIN-COVER of the frame (nothing worth cropping)."
  (let* ((n (length (getf rec :frames))) (w (getf rec :width)) (h (getf rec :height)))
    (when (zerop n) (return-from compute-content-bbox nil))
    (let* ((minx nil) (miny nil) (maxx nil) (maxy nil)
           (stride (max 1 (floor n (max 1 (min samples n))))))
      (%map-frames rec
        (lambda (i bytes)
          (when (zerop (mod i stride))    ; sample every stride-th frame
            (multiple-value-bind (bb bg br) (%bg-color bytes w h)
              (dotimes (cy gh)
                (dotimes (cx gw)
                  (multiple-value-bind (pb pg pr)
                      (%sample-px bytes w h (/ (+ cx 0.5) gw) (/ (+ cy 0.5) gh))
                    (when (> (+ (abs (- pb bb)) (abs (- pg bg)) (abs (- pr br))) threshold)
                      (setf minx (if minx (min minx cx) cx) maxx (if maxx (max maxx cx) cx)
                            miny (if miny (min miny cy) cy) maxy (if maxy (max maxy cy) cy))))))))))
      (when (null minx) (return-from compute-content-bbox nil))
      (let* ((x0 (/ minx (float gw 1.0))) (y0 (/ miny (float gh 1.0)))
             (x1 (/ (1+ maxx) (float gw 1.0))) (y1 (/ (1+ maxy) (float gh 1.0)))
             (cover (* (- x1 x0) (- y1 y0))))
        (if (>= cover min-cover)
            nil                                   ; content fills the frame -> no crop
            (list (max 0.0 (- x0 margin)) (max 0.0 (- y0 margin))
                  (min 1.0 (+ x1 margin)) (min 1.0 (+ y1 margin))))))))

(defun %build-idle-warp (damage span &key (threshold 1.2) (keep 0.4))
  "From the DAMAGE track (list of (time ...) where the screen changed) over a
SPAN-second source timeline, build a monotonic time warp: a function OUT-TIME ->
SRC-TIME that keeps active stretches at 1x but cuts each idle gap (no screen
change) longer than THRESHOLD down to KEEP seconds. Return (values WARP-FN
OUT-DURATION KEPT-SEGMENTS), where KEPT-SEGMENTS is the list of (src-start .
src-end) intervals kept at 1x (for trimming audio in sync). Identity warp when
nothing is idle."
  (let* ((acts (sort (remove-duplicates
                      (loop for d in damage collect (float (first d) 1.0)))
                     #'<))
         (times (sort (remove-duplicates (append (list 0.0) acts (list (float span 1.0))))
                      #'<))
         (bps  (list (cons 0.0 0.0)))   ; (out . src) breakpoints
         (segs '())                     ; kept (src-start . src-end)
         (out 0.0) (prev 0.0) (seg-start 0.0))
    (dolist (tt (rest times))
      (let ((gap (- tt prev)))
        (cond
          ((> gap threshold)
           ;; idle: keep the first KEEP seconds at 1x, then cut to TT
           (incf out keep) (push (cons out (+ prev keep)) bps)
           (push (cons seg-start (+ prev keep)) segs)     ; close the kept run
           (push (cons out tt) bps)                       ; cut: src jumps, out held
           (setf seg-start tt))
          (t
           (incf out gap) (push (cons out tt) bps)))
        (setf prev tt)))
    (push (cons seg-start (float span 1.0)) segs)
    (let ((bpv (coerce (nreverse bps) 'vector)) (out-dur out))
      (values
       (lambda (to)
         ;; linear-interpolate SRC from OUT across the breakpoints
         (let ((n (length bpv)))
           (if (<= to (car (aref bpv 0))) (cdr (aref bpv 0))
               (loop for k from 1 below n
                     for b0 = (aref bpv (1- k)) for b1 = (aref bpv k)
                     when (<= to (car b1))
                       do (let ((dw (- (car b1) (car b0))))
                            (return (if (> dw 1e-6)
                                        (+ (cdr b0) (* (/ (- to (car b0)) dw)
                                                       (- (cdr b1) (cdr b0))))
                                        (cdr b1))))
                     finally (return (cdr (aref bpv (1- n))))))))
       out-dur
       (nreverse segs)))))

(defun %region-uv (region fw fh)
  "Convert REGION (list x y w h, source pixels) to a (x0 y0 x1 y1) source-UV crop,
clamped to the frame."
  (destructuring-bind (x y w h) region
    (list (max 0.0 (/ (float x 1.0) fw))
          (max 0.0 (/ (float y 1.0) fh))
          (min 1.0 (/ (float (+ x w) 1.0) fw))
          (min 1.0 (/ (float (+ y h) 1.0) fh)))))

(defun %reshape-crop (crop fw fh tw th)
  "Reshape CROP (x0 y0 x1 y1 source UV) so its pixel aspect matches TW:TH, centered
on the crop's centre and clamped to the frame. Used for social reframe (9:16, 1:1,
...): narrows width or reduces height to hit the target, keeping the active region
centered."
  (destructuring-bind (x0 y0 x1 y1) crop
    (let* ((cx (* 0.5 (+ x0 x1))) (cy (* 0.5 (+ y0 y1)))
           (cw (* (- x1 x0) fw)) (ch (* (- y1 y0) fh))
           (target (/ (float tw 1.0) (float th 1.0)))
           (cur    (/ cw (max 1.0 ch))))
      (if (> cur target)
          ;; too wide -> narrow the width to match the target aspect
          (let ((halfu (/ (* 0.5 ch target) fw)))
            (list (max 0.0 (- cx halfu)) y0 (min 1.0 (+ cx halfu)) y1))
          ;; too tall -> reduce the height
          (let ((halfv (/ (* 0.5 (/ cw target)) fh)))
            (list x0 (max 0.0 (- cy halfv)) x1 (min 1.0 (+ cy halfv))))))))

(defun %grad-peaks (grad n &key (k 1.2))
  "Indices of GRAD (length N) that are local maxima above mean + K*stddev, as UV
positions (index/N). Strong content edges."
  (let* ((mean (/ (reduce #'+ grad) n))
         (var (/ (reduce #'+ (map 'list (lambda (v) (expt (- v mean) 2)) grad)) n))
         (thr (+ mean (* k (sqrt var)))) (out '()))
    (loop for i from 1 below (1- n)
          when (and (> (aref grad i) thr)
                    (>= (aref grad i) (aref grad (1- i)))
                    (>  (aref grad i) (aref grad (1+ i))))
            do (push (/ (+ i 0.5) n) out))
    (nreverse out)))

(defun compute-edges (rec &key (gw 96) (gh 60) (samples 6))
  "Detect strong vertical/horizontal content edges (window/UI-panel boundaries)
from the mean of a few sampled frames' coarse luma. Return (values xs ys), sorted
UV edge positions in full-frame coordinates."
  (let* ((w (getf rec :width)) (h (getf rec :height))
         (n (length (getf rec :frames))))
    (when (or (zerop n) (zerop w) (zerop h)) (return-from compute-edges (values '() '())))
    (let ((acc (make-array (* gw gh) :initial-element 0)) (cnt 0)
          (stride (max 1 (floor n (max 1 (min samples n))))))
      (%map-frames rec
        (lambda (i bytes)
          (when (zerop (mod i stride))
            (let ((g (%frame-luma-grid bytes w h gw gh)))
              (dotimes (k (* gw gh)) (incf (aref acc k) (aref g k))))
            (incf cnt))))
      (when (zerop cnt) (return-from compute-edges (values '() '())))
      (flet ((cell (x y) (/ (aref acc (+ (* y gw) x)) (float cnt 1.0))))
        (let ((colg (make-array gw :initial-element 0.0))
              (rowg (make-array gh :initial-element 0.0)))
          (loop for x from 1 below gw do
            (loop for y below gh do (incf (aref colg x) (abs (- (cell x y) (cell (1- x) y))))))
          (loop for y from 1 below gh do
            (loop for x below gw do (incf (aref rowg y) (abs (- (cell x y) (cell x (1- y)))))))
          (values (%grad-peaks colg gw) (%grad-peaks rowg gh)))))))

(defun %edges-in-crop (edges c0 c1)
  "Re-express full-frame UV EDGES as crop-relative UV over [C0,C1], dropping any
outside the crop."
  (let ((d (- c1 c0)))
    (loop for e in edges for u = (/ (- e c0) d)
          when (<= 0.0 u 1.0) collect u)))

(defun %fuse-cursor (anchors motion &key (max-gap 0.12))
  "Fuse PipeWire cursor ANCHORS -- (time x y) full-frame px, absolute but gappy --
with libinput MOTION -- (time dx dy) continuous relative -- into a dense (time x y)
track. Within a gap longer than MAX-GAP, distribute the anchor-to-anchor
displacement along the motion PROFILE (so the pointer moves when it actually moved
and lands on both anchors -- acceleration cancels out because we scale to hit the
endpoints); short gaps and axes with no motion fall back to a straight line. No
motion track -> ANCHORS unchanged."
  (if (or (null motion) (< (length anchors) 2))
      anchors
      (let ((mv (sort (copy-list motion) #'< :key #'first)))
        (loop for (a b) on anchors
              append
              (cons a
                    (when (and b (> (- (first b) (first a)) max-gap))
                      (destructuring-bind (ta xa ya) a
                        (destructuring-bind (tb xb yb) b
                          (let ((cdx 0.0) (cdy 0.0) (segs '()))
                            (dolist (m mv)
                              (when (< ta (first m) tb)
                                (incf cdx (second m)) (incf cdy (third m))
                                (push (list (first m) cdx cdy) segs)))
                            (setf segs (nreverse segs))
                            (let ((tdx cdx) (tdy cdy) (span (- tb ta)))
                              (when (and segs (or (> (abs tdx) 1e-6) (> (abs tdy) 1e-6)))
                                (mapcar
                                 (lambda (s)
                                   (destructuring-bind (tm pdx pdy) s
                                     (let ((u (/ (- tm ta) span)))
                                       (list tm
                                             (if (> (abs tdx) 1e-6) (+ xa (* pdx (/ (- xb xa) tdx)))
                                                 (+ xa (* (- xb xa) u)))
                                             (if (> (abs tdy) 1e-6) (+ ya (* pdy (/ (- yb ya) tdy)))
                                                 (+ ya (* (- yb ya) u)))))))
                                 segs))))))))))))

(defun recording->session (rec &optional (crop '(0.0 0.0 1.0 1.0)))
  "Build a Director SESSION from a capture recording plist. CROP (x0 y0 x1 y1 in
source UV) reframes the session onto just the content region: the session
dimensions become the crop's pixel size and cursor positions are made relative to
the crop origin, so the Director works in cropped space. When a libinput motion
track is present, it's fused with the PipeWire cursor so the pointer stays accurate
through static-screen gaps."
  (destructuring-bind (cx0 cy0 cx1 cy1) crop
    (let* ((fw (getf rec :width)) (fh (getf rec :height))
           (ox (* cx0 fw)) (oy (* cy0 fh))
           (cw (max 1 (round (* (- cx1 cx0) fw))))
           (ch (max 1 (round (* (- cy1 cy0) fh))))
           (anchors (loop for f in (getf rec :frames)
                          when (getf f :cursor-x)
                            collect (list (float (getf f :time) 1.0)
                                          (float (getf f :cursor-x) 1.0)
                                          (float (getf f :cursor-y) 1.0))))
           (cursor (mapcar (lambda (s)
                             (dir:make-cursor-sample :time (first s)
                                                     :x (- (second s) ox)
                                                     :y (- (third s) oy)))
                           (%fuse-cursor anchors (getf rec :pointer-motion)))))
      (dir:make-session :width cw :height ch :cursor cursor :events '()
                        ;; click times (positioned later via the cursor track); the
                        ;; strongest attention evidence for the shot planner.
                        :clicks (mapcar (lambda (tc) (float tc 1.0))
                                        (getf rec :click-times))))))

(defun %crop-damage (damage crop)
  "Re-express DAMAGE rects (full-frame UV) in CROP-relative UV, dropping any that
fall entirely outside the crop."
  (destructuring-bind (cx0 cy0 cx1 cy1) crop
    (let ((dw (- cx1 cx0)) (dh (- cy1 cy0)))
      (loop for (tm x0 y0 x1 y1) in damage
            for nx0 = (/ (- x0 cx0) dw) for ny0 = (/ (- y0 cy0) dh)
            for nx1 = (/ (- x1 cx0) dw) for ny1 = (/ (- y1 cy0) dh)
            when (and (< nx0 1.0) (> nx1 0.0) (< ny0 1.0) (> ny1 0.0))
              collect (list tm (max 0.0 nx0) (max 0.0 ny0) (min 1.0 nx1) (min 1.0 ny1))))))

(defun %nearest-frame-index (frames tsec)
  "Index of the last source frame with :time <= TSEC (hold-previous); 0 before
the first. FRAMES is a vector in ascending :time order."
  (let ((idx 0))
    (loop for i below (length frames)
          while (<= (getf (aref frames i) :time) tsec)
          do (setf idx i))
    idx))

(defun %write-camera-log (path timeline nout fps src-time)
  "Write the per-output-frame camera path to PATH as CSV, sampling TIMELINE exactly
as the compositor does (at each output frame's source time). Columns: frame,
t_out, t_src, zoom, cx, cy, pan_per_s, dzoom_per_s. Also print a compact motion
summary -- the moments of sharpest zoom and pan change -- so the 'wild trip'
stretches are easy to spot. Returns the list of (t_out zoom cx cy) samples."
  (let ((rows '()) (px nil) (py nil) (pz nil)
        (max-pan 0.0) (max-pan-t 0.0) (max-dz 0.0) (max-dz-t 0.0)
        (reversals 0) (prev-dz-sign 0))
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (format s "frame,t_out,t_src,zoom,cx,cy,pan_per_s,dzoom_per_s~%")
      (dotimes (i nout)
        (let* ((tout (/ i (float fps 1.0)))
               (tsrc (funcall src-time i))
               (k (kf:sample-timeline timeline tsrc))
               (z (kf:keyframe-zoom k))
               (x (kf:keyframe-center-x k))
               (y (kf:keyframe-center-y k))
               (pan (if px (* fps (sqrt (+ (expt (- x px) 2) (expt (- y py) 2)))) 0.0))
               (dz  (if pz (* fps (- z pz)) 0.0)))
          (format s "~D,~,3F,~,3F,~,4F,~,4F,~,4F,~,4F,~,4F~%" i tout tsrc z x y pan dz)
          (push (list tout z x y) rows)
          (when (> pan max-pan) (setf max-pan pan max-pan-t tout))
          (when (> (abs dz) max-dz) (setf max-dz (abs dz) max-dz-t tout))
          (let ((sign (cond ((> dz 0.02) 1) ((< dz -0.02) -1) (t 0))))
            (when (and (/= sign 0) (/= prev-dz-sign 0) (/= sign prev-dz-sign))
              (incf reversals))
            (unless (zerop sign) (setf prev-dz-sign sign)))
          (setf px x py y pz z))))
    (format t "  [camera-log] wrote ~D samples -> ~A~%" nout path)
    (format t "  [camera-log] peak pan ~,2F/s @ ~,1Fs; peak zoom-rate ~,2F/s @ ~,1Fs; ~
               ~D zoom-direction reversals~%"
            max-pan max-pan-t max-dz max-dz-t reversals)
    (nreverse rows)))

(defun compose-recording (rec timeline &key (out "/tmp/takesy-record.mp4") (max-height 1200)
                                            (fps 24) (duration nil)
                                            (cursor-session nil)
                                            (cursor-image nil)
                                            (cursor-hotspot '(0.0 . 0.0))
                                            (cursor-size nil)
                                            (bg-image nil)
                                            (bg-blur nil)
                                            (clicks nil) (ripple-color '(1.0 1.0 1.0))
                                            (webcam nil) (webcam-pos :br) (webcam-size 0.22)
                                  (webcam-corner 1.0)
                                  (webcam-border 0.012) (webcam-border-color '(1.0 1.0 1.0))
                                  (webcam-zoom 1.0) (webcam-pan-x 0.0) (webcam-pan-y 0.0)
                                            (audio nil)
                                            (time-warp nil) (out-duration nil)
                                            (aspect nil)
                                            (camera-log nil)
                                            (crop '(0.0 0.0 1.0 1.0)))
  "Render REC's real BGRx frames through the compositor driven by TIMELINE, at a
STEADY output FPS over DURATION seconds (default: the captured time span). Static
stretches -- where the screencast emitted no frame -- hold the previous frame, so
the clip is always full-length and smooth. CROP (x0 y0 x1 y1 source UV) frames
only the content region. The output keeps the content's aspect and is sized so
its height is at most MAX-HEIGHT (never upscaled past the content), for sharpness
without an over-large file."
  (let* ((frames (coerce (getf rec :frames) 'vector))
         (nsrc (length frames)))
    (when (zerop nsrc) (error "recording has no frames"))
    (destructuring-bind (cx0 cy0 cx1 cy1) crop
      (let* ((span (float (getf (aref frames (1- nsrc)) :time) 1.0))
             ;; PiP/audio sync: the parallel audio & webcam recorders start before
             ;; the first screen frame, so each carries a pre-roll (= its duration -
             ;; SPAN, since all sources stop together). Trim that lead off the front
             ;; so both line up with the picture (green-screen sync bug).
             (webcam-lead (%source-lead webcam span))
             (audio-lead  (%source-lead audio span))
             ;; TIME-WARP maps output time -> source time (idle-removal); identity
             ;; otherwise. DUR is the OUTPUT length.
             (warp (or time-warp #'identity))
             (dur  (or out-duration duration span))
             (nout (max 1 (round (* fps dur))))
             (fw (getf rec :width)) (fh (getf rec :height))    ; full frame = texture
             (cw (* (- cx1 cx0) fw)) (ch (* (- cy1 cy0) fh))   ; cropped content px
             (content-aspect (/ cw (max 1.0 ch)))
             ;; Output canvas aspect: the content's, or a requested ASPECT (W . H).
             ;; The compositor CONTAINS the content in it (letterbox, no crop).
             (target-aspect (if aspect (/ (float (car aspect) 1.0) (cdr aspect)) content-aspect))
             (oh (* 2 (max 1 (round (/ (min ch (float max-height 1.0)) 2)))))
             (ow (* 2 (max 1 (round (/ (* oh target-aspect) 2)))))
             (cache-idx -1) (cache-bytes nil)
             (src-time (lambda (i) (funcall warp (/ i (float fps 1.0)))))  ; out frame -> src time
             (cursor-fn (when cursor-session   ; cursor coords are in cropped px
                          (lambda (i)
                            (multiple-value-bind (x y)
                                ;; hold across capture gaps rather than drawing the
                                ;; pointer at a fictional midpoint (green-screen).
                                (dir:cursor-at cursor-session (funcall src-time i)
                                               :max-gap dir:*cursor-gap-hold*)
                              (cons (/ x (float cw 1.0)) (/ y (float ch 1.0))))))))
        (format t "  [record] compositing ~D src frames -> ~D output frames ~
                   (~,1Fs @ ~Dfps) crop ~,2Fx~,2F of frame -> ~Dx~D~@[ +cursor~]~%"
                nsrc nout dur fps (- cx1 cx0) (- cy1 cy0) ow oh cursor-fn)
        (when (or (> webcam-lead 0.01) (> audio-lead 0.01))
          (format t "  [record] PiP sync: trimming ~,2Fs webcam / ~,2Fs audio pre-roll~%"
                  webcam-lead audio-lead))
        (when camera-log
          (%write-camera-log camera-log timeline nout fps src-time))
        ;; source frames come from the compressed intermediate (decoded forward)
        ;; when present, else per-frame raw files.
        ;; Optional webcam PiP: decode WEBCAM forward, mapping output time to the
        ;; webcam's own timeline (holds last frame past its end).
        (multiple-value-bind (ww wh wfps)
            (if webcam (%probe-video webcam) (values nil nil nil))
          (let ((dec (when (getf rec :intermediate)
                       (%open-decoder (getf rec :intermediate) fw fh)))
                ;; Force the webcam decoder to the SAME cadence webcam-fn indexes by
                ;; (its true avg rate), so a camera that ran below its nominal fps
                ;; doesn't decode padded-to-nominal and play in slow motion (fix).
                (wc-dec (when webcam (%open-decoder webcam ww wh "rgba" wfps)))
                ;; The intermediate is decoded to BGRA; the raw-file fallback keeps
                ;; the negotiated byte order, so tell the compositor which it is
                ;; (green-screen-f5b).
                (source-format (if (getf rec :intermediate)
                                   :bgra
                                   (pw:format->gl-source-format (getf rec :format)))))
            (unwind-protect
                 (let ((webcam-fn (when webcam
                                    ;; webcam frame at output time i (its own fps),
                                    ;; shifted past its pre-first-frame lead so it
                                    ;; lines up with the picture (PiP sync).
                                    (lambda (i)
                                      (%decoder-frame
                                       wc-dec
                                       (floor (* (+ (/ i (float fps 1.0)) webcam-lead) wfps)))))))
                  (flet ((src (i)   ; nearest source frame for output frame i (warped)
                          (let ((idx (%nearest-frame-index frames (funcall src-time i))))
                            (if dec
                                (%decoder-frame dec idx)
                                (progn
                                  (unless (= idx cache-idx)
                                    (setf cache-idx idx
                                          cache-bytes (%read-file-bytes (getf (aref frames idx) :path))))
                                  cache-bytes)))))
                   (comp:render-frame-sequence
                    timeline #'src nout ow oh
                    :fps fps :source-format source-format
                    :source-width fw :source-height fh
                    :time-fn src-time
                    :cursor-fn cursor-fn
                    :cursor-image cursor-image :cursor-hotspot cursor-hotspot
                    :cursor-size cursor-size
                    :bg-image bg-image :bg-blur bg-blur
                    :clicks clicks :ripple-color ripple-color
                    :webcam-fn webcam-fn :webcam-dims (when webcam (cons ww wh))
                    :webcam-pos webcam-pos :webcam-size webcam-size
                    :webcam-corner webcam-corner
                    :webcam-border webcam-border :webcam-border-color webcam-border-color
                    :webcam-zoom webcam-zoom :webcam-pan-x webcam-pan-x :webcam-pan-y webcam-pan-y
                    :content-aspect content-aspect
                    :audio audio :audio-offset audio-lead
                    :crop crop
                    :path out)))
              (%close-decoder dec)
              (%close-decoder wc-dec))))))))

(defun %beep (&key (freq 880) (dur 0.12))
  "Best-effort short audible beep for the count-in. Uses ffplay (ships with ffmpeg,
already required); falls back to the terminal bell. Non-blocking."
  (unless (ignore-errors
           (uiop:launch-program
            (list "ffplay" "-nodisp" "-autoexit" "-loglevel" "quiet" "-f" "lavfi"
                  "-i" (format nil "sine=frequency=~D:duration=~,3F,volume=0.35" freq dur))
            :output nil :error-output nil))
    (write-char #\Bell) (finish-output)))

(defun %persist-manifest (rec dir)
  "Re-write DIR/manifest.sexp from REC (after splicing in fields RECORD-FRAMES
didn't know at write time, e.g. :click-times)."
  (with-open-file (s (format nil "~A/manifest.sexp" (string-right-trim "/" dir))
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-standard-io-syntax (prin1 rec s))))

(defun capture-recording (&key (duration nil) (fps 24) (dir "/tmp/takesy-rec")
                               (audio nil) (capture-clicks t) (countdown 3)
                               (webcam nil)
                               (webcam-zoom 1.0) (webcam-pan-x 0.0) (webcam-pan-y 0.0))
  "Capture stage only: pop the screen-share dialog and record until you end the
share -- click GNOME's Stop button in the top bar. DURATION, when non-NIL, is an
optional safety cap in seconds (NIL = record until you click Stop). Frames (and the compressed intermediate) are written under DIR, and
RECORD-FRAMES persists a manifest.sexp there so the recording is self-contained and
can be re-rendered later with LOAD-RECORDING + RENDER-RECORDING. FPS only sets the
capture throttle (a bit above the output rate). AUDIO (:system | :mic | :both)
records a parallel audio track stored in the manifest. When CAPTURE-CLICKS, a
background thread records mouse-click times (evdev, best-effort -- needs the
`input' group) into :click-times for the render's click ripples. Return the
recording plist."
  (portal:with-screencast (fd node :cursor-mode portal:+cursor-metadata+)
    (format t "  [capture] recording... click the Stop button in the GNOME top bar ~
                 to finish~@[ (or ~,0Fs max)~].~%" duration)
    ;; Capture throttle a bit above the output rate so we keep enough source
    ;; frames; the real limit is the compositor's on-change delivery.
    ;; Warn up front when we can't read input devices -- otherwise ripples just
    ;; silently don't appear (clicks need the `input' group).
    (when (and capture-clicks (not (evdev:probe-readable-input-devices)))
      (format t "  [capture] note: no readable input device -- click ripples need~%~
                   read access to /dev/input. Add yourself to the 'input' group:~%~
                   sudo usermod -aG input $USER  (then log out/in). Recording~%~
                   without click data for now.~%"))
    ;; Countdown so you can get ready after picking the share source -- with an
    ;; audible beep per count and a higher "go" tone when recording starts.
    (when (and countdown (plusp countdown))
      (loop for n from countdown downto 1
            do (%beep :freq 880)
               (format t "  [capture] recording in ~D...~%" n) (finish-output) (sleep 1))
      (%beep :freq 1320 :dur 0.2))
    (let* ((done nil)
           ;; evdev click capture runs alongside the frame capture; base ~ capture
           ;; start so click times align with the frame timeline.
           (join (when capture-clicks
                   (ignore-errors (evdev:capture-click-times (lambda () done)))))
           ;; Continuous pointer motion via libinput -- fused with the (gappy)
           ;; PipeWire cursor at render so the pointer tracks even while the screen
           ;; is static. Best-effort; times are GET-INTERNAL-REAL-TIME (frame clock).
           (motion-join (when capture-clicks
                          (ignore-errors (li:capture-pointer-motion (lambda () done)))))
           ;; Live webcam (green-screen-asp): a parallel v4l2 recorder over the same
           ;; window, started here so it aligns with frame capture (past the
           ;; countdown), best-effort like audio.
           (wc-handle (when webcam (wc:start-webcam dir webcam :fps (max fps 30))))
           (rec  (pw:record-frames fd node :duration duration :max-fps (max fps 30)
                                   :dir dir :audio audio))
           (dirty nil))
      (setf done t)
      ;; Store the pointer-motion track (times made relative to the first frame so
      ;; the render can fuse it with the PipeWire cursor anchors).
      (when motion-join
        (let ((motion (ignore-errors (funcall motion-join)))
              (t0 (getf rec :record-start))
              (units (float (or (getf rec :time-units) 1000000) 1.0d0)))
          (when (and motion t0)
            (setf (getf rec :pointer-motion)      ; (time-seconds dx dy), frame clock
                  (mapcar (lambda (e) (list (float (/ (- (first e) t0) units) 1.0)
                                            (second e) (third e)))
                          motion)
                  dirty t)
            (format t "  [capture] pointer motion: ~D samples (fused for accurate cursor)~%"
                    (length motion)))))
      ;; Finalize the webcam clip and store its path so render composites it as PiP.
      (when wc-handle
        (let ((path (ignore-errors (wc:stop-webcam wc-handle))))
          (when path
            (setf (getf rec :webcam) path dirty t)
            ;; Persist the preview's framing so a later render reproduces it.
            (when (or (/= webcam-zoom 1.0) (/= webcam-pan-x 0.0) (/= webcam-pan-y 0.0))
              (setf (getf rec :webcam-zoom) webcam-zoom
                    (getf rec :webcam-pan-x) webcam-pan-x
                    (getf rec :webcam-pan-y) webcam-pan-y))
            (format t "  [capture] webcam clip -> ~A~%" path))))
      (when join
        (let ((cts (ignore-errors (funcall join))))
          (when cts
            (setf (getf rec :click-times) cts dirty t)
            (format t "  [capture] recorded ~D click(s) for ripples~%" (length cts)))))
      ;; Re-persist the manifest once if we spliced in anything RECORD-FRAMES
      ;; didn't know at write time (clicks and/or webcam).
      (when dirty (%persist-manifest rec dir))
      rec)))

(defun render-recording (rec &key (fps 24) (max-height 1200)
                                  (bg '(0.11 0.12 0.15))
                                  (corner 0.09)
                                  (margin 0.04)
                                  (cursor nil)
                                  (cursor-hotspot '(0.0 . 0.0))
                                  (cursor-size nil)
                                  (bg-image nil)
                                  (bg-blur nil)
                                  (ripples t)
                                  (aspect nil)
                                  (region nil)
                                  (trim-idle nil) (idle-threshold 1.2) (max-idle 0.4)
                                  (webcam nil) (webcam-pos :br) (webcam-size 0.22)
                                  (webcam-corner 1.0)
                                  (webcam-border 0.012) (webcam-border-color '(1.0 1.0 1.0))
                                  ;; NIL so the manifest's previewed framing can fill in
                                  (webcam-zoom nil) (webcam-pan-x nil) (webcam-pan-y nil)
                                  (cursor-omega-fast dir:*cursor-omega-fast*)
                                  (cursor-anticipate dir:*cursor-anticipate*)
                                  (camera-smoothing dir:*camera-cursor-tau*)
                                  (camera-log nil)
                                  (out "/tmp/takesy-record.mp4"))
  "Direct + composite stages: direct REC via the editorial camera and render its
real captured frames to a full-length mp4 at OUT, with the eased cursor overlay.
FPS is the OUTPUT rate; static stretches hold the last frame. The output length is
the ACTUAL captured span. CURSOR-OMEGA-FAST and CURSOR-ANTICIPATE tune the cursor
easing; CORNER is the rounded-corner radius (fraction of the min content dim; 0 =
square corners). CURSOR, if given, is a path to an image drawn in place of the
built-in arrow, with CURSOR-HOTSPOT (cons hx . hy, fraction of the image) as the
click point and CURSOR-SIZE its height as a fraction of the output height. Pure
function of REC -- no capture -- so it can be re-run against one capture with
different config. Return (values out n-frames)."
  (let ((dir:*cursor-omega-fast* cursor-omega-fast)
        (dir:*cursor-anticipate* cursor-anticipate)
        (dir:*camera-cursor-tau* camera-smoothing)
        (cursor-image (when cursor (multiple-value-list (load-image-rgba cursor))))
        (bg-image-data (when bg-image (multiple-value-list (load-image-rgba bg-image))))
        ;; Webcam PiP source: an explicit clip/file wins; otherwise fall back to a
        ;; webcam clip captured live during this recording (green-screen-asp).
        (webcam (or webcam (getf rec :webcam)))
        ;; Framing: an explicit flag wins, else the framing chosen in the preview
        ;; (stored in the manifest), else neutral (green-screen-1lb).
        (webcam-zoom  (or webcam-zoom  (getf rec :webcam-zoom)  1.0))
        (webcam-pan-x (or webcam-pan-x (getf rec :webcam-pan-x) 0.0))
        (webcam-pan-y (or webcam-pan-y (getf rec :webcam-pan-y) 0.0)))
    (let* (;; Base framing: an explicit REGION (x y w h source px) if given, else
           ;; auto-crop to the real content (trim empty desktop borders).
           ;; Everything downstream works in this cropped frame. ASPECT (cons w . h)
           ;; then reshapes it to a target output aspect (e.g. 9:16 vertical).
           ;; ASPECT changes only the OUTPUT canvas shape; the compositor CONTAINS
           ;; the content in it (letterbox), so we keep the full content crop here.
           (crop     (cond (region (%region-uv region (getf rec :width) (getf rec :height)))
                           ((compute-content-bbox rec))
                           (t '(0.0 0.0 1.0 1.0))))
           (session  (recording->session rec crop))
           (damage   (%crop-damage (compute-damage rec) crop))  ; where the screen changed
           (timeline (progn
                       (setf (dir:session-damage session) damage)
                       (dir:plan-timeline session :bg bg :corner corner
                                          :padding margin)))  ; editorial shot plan
           ;; eased cursor track (D2 spring) for the overlay -- METADATA hid the
           ;; real cursor, so we draw a smoothed one at the tracked position.
           (eased    (dir:make-session :width (dir:session-width session)
                                       :height (dir:session-height session)
                                       :cursor (dir:ease-cursor session)))
           ;; Idle removal: warp output time -> source time, cutting long
           ;; no-change gaps. WARP-INFO is (warp-fn out-duration segments) or NIL.
           (span     (float (getf (car (last (getf rec :frames))) :time) 1.0))
           (warp-info (when trim-idle
                        (multiple-value-list
                         (%build-idle-warp damage span
                                           :threshold idle-threshold :keep max-idle)))))
      (format t "  [render] ~D frames (~D cursor, ~D changed) -> ~D keyframes~%"
              (length (getf rec :frames))
              (length (dir:session-cursor session))
              (length damage)
              (length timeline))
      (when warp-info
        (format t "  [render] idle-trim: ~,1Fs -> ~,1Fs~%" span (second warp-info)))
      ;; No :duration -> compose uses the actual captured span (you decide the
      ;; length by when you click Stop).
      (compose-recording rec timeline :out out :max-height max-height
                         :fps fps :cursor-session eased :crop crop :aspect aspect
                         :camera-log camera-log
                         :cursor-image cursor-image
                         :cursor-hotspot cursor-hotspot :cursor-size cursor-size
                         :bg-image bg-image-data :bg-blur bg-blur
                         :clicks (when ripples (getf rec :click-times))
                         :webcam webcam :webcam-pos webcam-pos :webcam-size webcam-size
                    :webcam-corner webcam-corner
                    :webcam-border webcam-border :webcam-border-color webcam-border-color
                    :webcam-zoom webcam-zoom :webcam-pan-x webcam-pan-x :webcam-pan-y webcam-pan-y
                         :time-warp (first warp-info) :out-duration (second warp-info)
                         ;; audio was captured alongside the frames; mux it back in.
                         ;; Idle-trim would desync it -> drop it (synced trim is TODO).
                         :audio (if (and warp-info (getf rec :audio))
                                    (progn
                                      (format t "  [render] note: --trim-idle drops audio ~
                                                 (synced trim not yet implemented)~%")
                                      nil)
                                    (getf rec :audio))))))

(defun render-recording-dir (dir &rest render-args)
  "Load the manifest RECORD-FRAMES persisted under DIR and RENDER-RECORDING it.
This is the re-render entry point: capture once, then re-run direction with
different RENDER-ARGS (:bg, :zoom, :fps, ...) as often as you like."
  (apply #'render-recording (pw:load-recording dir) render-args))

(defun record-to-mp4 (&key (duration nil) (fps 24) (max-height 1200)
                           (bg '(0.11 0.12 0.15))
                           (corner 0.09)
                           (margin 0.04)
                           (cursor nil)
                           (cursor-hotspot '(0.0 . 0.0))
                           (cursor-size nil)
                           (bg-image nil)
                           (bg-blur nil)
                           (ripples t)
                           (aspect nil)
                           (region nil)
                           (trim-idle nil) (idle-threshold 1.2) (max-idle 0.4)
                           (webcam nil) (webcam-pos :br) (webcam-size 0.22)
                                  (webcam-corner 1.0)
                                  (webcam-border 0.012) (webcam-border-color '(1.0 1.0 1.0))
                                  (webcam-zoom 1.0) (webcam-pan-x 0.0) (webcam-pan-y 0.0)
                           (countdown 3)
                           (audio nil)
                           (cursor-omega-fast dir:*cursor-omega-fast*)
                           (cursor-anticipate dir:*cursor-anticipate*)
                           (camera-smoothing dir:*camera-cursor-tau*)
                           (camera-log nil)
                           (dir "/tmp/takesy-rec") (out "/tmp/takesy-record.mp4"))
  "Full `takesy record`: CAPTURE-RECORDING then RENDER-RECORDING in one shot --
capture the screen (METADATA cursor mode) until you click GNOME's Stop button (or
DURATION as a safety cap), then direct and composite to OUT. Kept as the one-call
path; the two halves are separately callable so a capture can be re-rendered.
Return (values out n-frames)."
  ;; A live webcam spec (/dev/video* or "auto") records during capture and the
  ;; render picks the clip up from the manifest; any other WEBCAM is a file for the
  ;; render's PiP. Never hand a live spec to render -- it would try to open it as a
  ;; file (green-screen-asp).
  (let* ((live (and (wc:live-spec-p webcam) webcam))
         (rec  (capture-recording :duration duration :fps fps :dir dir :audio audio
                                  :countdown countdown :webcam live)))
    (render-recording rec :fps fps :max-height max-height :bg bg :corner corner :margin margin
                          :cursor cursor :cursor-hotspot cursor-hotspot
                          :cursor-size cursor-size :bg-image bg-image :bg-blur bg-blur :ripples ripples
                          :aspect aspect :region region
                          :webcam (unless live webcam)
                          :trim-idle trim-idle :idle-threshold idle-threshold :max-idle max-idle
                          :webcam-pos webcam-pos :webcam-size webcam-size
                    :webcam-corner webcam-corner
                    :webcam-border webcam-border :webcam-border-color webcam-border-color
                    :webcam-zoom webcam-zoom :webcam-pan-x webcam-pan-x :webcam-pan-y webcam-pan-y
                          :cursor-omega-fast cursor-omega-fast
                          :cursor-anticipate cursor-anticipate
                          :camera-smoothing camera-smoothing
                          :camera-log camera-log
                          :out out)))
