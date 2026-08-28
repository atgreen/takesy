;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <anthony@atgreen.org>
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
                    (#:dir #:takesy/director) (#:comp #:takesy/compositor))
  (:export #:recording->session #:compute-damage #:compute-content-bbox
           #:compose-recording #:record-to-mp4))

(in-package #:takesy/record)

(defun %read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

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
  (let* ((frames (getf rec :frames)) (w (getf rec :width)) (h (getf rec :height))
         (prev nil) (out '()))
    (dolist (f frames (nreverse out))
      (let ((cur (%frame-luma-grid (%read-file-bytes (getf f :path)) w h gw gh)))
        (when prev
          (let (minx miny maxx maxy)
            (dotimes (cy gh)
              (dotimes (cx gw)
                (let ((k (+ (* cy gw) cx)))
                  (when (> (abs (- (aref cur k) (aref prev k))) threshold)
                    (setf minx (if minx (min minx cx) cx) maxx (if maxx (max maxx cx) cx)
                          miny (if miny (min miny cy) cy) maxy (if maxy (max maxy cy) cy))))))
            (when minx
              (push (list (float (getf f :time) 1.0)
                          (/ minx (float gw 1.0)) (/ miny (float gh 1.0))
                          (/ (1+ maxx) (float gw 1.0)) (/ (1+ maxy) (float gh 1.0)))
                    out))))
        (setf prev cur)))))

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
  (let* ((frames (coerce (getf rec :frames) 'vector))
         (n (length frames)) (w (getf rec :width)) (h (getf rec :height)))
    (when (zerop n) (return-from compute-content-bbox nil))
    (let ((minx nil) (miny nil) (maxx nil) (maxy nil)
          (k (max 1 (min samples n))))
      (dotimes (s k)
        (let* ((idx   (floor (* s n) k))
               (bytes (%read-file-bytes (getf (aref frames idx) :path))))
          (multiple-value-bind (bb bg br) (%bg-color bytes w h)
            (dotimes (cy gh)
              (dotimes (cx gw)
                (multiple-value-bind (pb pg pr)
                    (%sample-px bytes w h (/ (+ cx 0.5) gw) (/ (+ cy 0.5) gh))
                  (when (> (+ (abs (- pb bb)) (abs (- pg bg)) (abs (- pr br))) threshold)
                    (setf minx (if minx (min minx cx) cx) maxx (if maxx (max maxx cx) cx)
                          miny (if miny (min miny cy) cy) maxy (if maxy (max maxy cy) cy)))))))))
      (when (null minx) (return-from compute-content-bbox nil))
      (let* ((x0 (/ minx (float gw 1.0))) (y0 (/ miny (float gh 1.0)))
             (x1 (/ (1+ maxx) (float gw 1.0))) (y1 (/ (1+ maxy) (float gh 1.0)))
             (cover (* (- x1 x0) (- y1 y0))))
        (if (>= cover min-cover)
            nil                                   ; content fills the frame -> no crop
            (list (max 0.0 (- x0 margin)) (max 0.0 (- y0 margin))
                  (min 1.0 (+ x1 margin)) (min 1.0 (+ y1 margin))))))))

(defun recording->session (rec &optional (crop '(0.0 0.0 1.0 1.0)))
  "Build a Director SESSION from a capture recording plist. CROP (x0 y0 x1 y1 in
source UV) reframes the session onto just the content region: the session
dimensions become the crop's pixel size and cursor positions are made relative to
the crop origin, so the Director works in cropped space."
  (destructuring-bind (cx0 cy0 cx1 cy1) crop
    (let* ((fw (getf rec :width)) (fh (getf rec :height))
           (ox (* cx0 fw)) (oy (* cy0 fh))
           (cw (max 1 (round (* (- cx1 cx0) fw))))
           (ch (max 1 (round (* (- cy1 cy0) fh))))
           (cursor (loop for f in (getf rec :frames)
                         when (getf f :cursor-x)
                           collect (dir:make-cursor-sample
                                    :time (float (getf f :time) 1.0)
                                    :x (- (float (getf f :cursor-x) 1.0) ox)
                                    :y (- (float (getf f :cursor-y) 1.0) oy)))))
      (dir:make-session :width cw :height ch :cursor cursor :events '()))))

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

(defun compose-recording (rec timeline &key (out "/tmp/takesy-record.mp4") (scale 3)
                                            (fps 24) (duration nil)
                                            (cursor-session nil)
                                            (crop '(0.0 0.0 1.0 1.0)))
  "Render REC's real BGRx frames through the compositor driven by TIMELINE, at a
STEADY output FPS over DURATION seconds (default: the captured time span). Static
stretches -- where the screencast emitted no frame -- hold the previous frame, so
the clip is always full-length and smooth. CROP (x0 y0 x1 y1 source UV) frames
only the content region: the output size (and aspect) comes from the crop, and
the shader samples just that region. SCALE downsamples for a sane encode."
  (let* ((frames (coerce (getf rec :frames) 'vector))
         (nsrc (length frames)))
    (when (zerop nsrc) (error "recording has no frames"))
    (destructuring-bind (cx0 cy0 cx1 cy1) crop
      (let* ((span (float (getf (aref frames (1- nsrc)) :time) 1.0))
             (dur  (or duration span))
             (nout (max 1 (round (* fps dur))))
             (fw (getf rec :width)) (fh (getf rec :height))    ; full frame = texture
             (cw (* (- cx1 cx0) fw)) (ch (* (- cy1 cy0) fh))   ; cropped content px
             (ow (* 2 (max 1 (round (/ cw scale 2)))))         ; output = crop aspect
             (oh (* 2 (max 1 (round (/ ch scale 2)))))
             (cache-idx -1) (cache-bytes nil)
             (cursor-fn (when cursor-session   ; cursor coords are in cropped px
                          (lambda (i)
                            (multiple-value-bind (x y)
                                (dir:cursor-at cursor-session (/ i (float fps 1.0)))
                              (cons (/ x (float cw 1.0)) (/ y (float ch 1.0))))))))
        (format t "  [record] compositing ~D src frames -> ~D output frames ~
                   (~,1Fs @ ~Dfps) crop ~,2Fx~,2F of frame -> ~Dx~D~@[ +cursor~]~%"
                nsrc nout dur fps (- cx1 cx0) (- cy1 cy0) ow oh cursor-fn)
        (flet ((src (i)   ; nearest source frame for output time i/fps, cached
                 (let ((idx (%nearest-frame-index frames (/ i (float fps 1.0)))))
                   (unless (= idx cache-idx)
                     (setf cache-idx idx
                           cache-bytes (%read-file-bytes (getf (aref frames idx) :path))))
                   cache-bytes)))
          (comp:render-frame-sequence
           timeline #'src nout ow oh
           :fps fps :source-format :bgra
           :source-width fw :source-height fh
           :time-fn (lambda (i) (/ i (float fps 1.0)))
           :cursor-fn cursor-fn :crop crop
           :path out))))))

(defun record-to-mp4 (&key (duration 30.0) (fps 24) (scale 3)
                           (dir "/tmp/takesy-rec") (out "/tmp/takesy-record.mp4"))
  "Full `takesy record`: capture the screen (METADATA cursor mode, armed teardown)
until you end the share -- click GNOME's Stop button in the top bar -- or DURATION
seconds elapse as a safety cap. Then auto-zoom via the Director (dwell-based) and
render the real captured frames to a full-length mp4 at OUT, with the eased cursor
overlay. FPS is the OUTPUT rate; static stretches hold the last frame. The output
length is the ACTUAL captured span, not the cap. Return (values out n-frames)."
  (portal:with-screencast (fd node :cursor-mode portal:+cursor-metadata+)
    (format t "  [record] recording... click the Stop button in the GNOME top bar ~
                 to finish (or ~,0Fs max).~%" duration)
    ;; Capture throttle a bit above the output rate so we keep enough source
    ;; frames; the real limit is the compositor's on-change delivery.
    (let* ((rec      (pw:record-frames fd node :duration duration
                                       :max-fps (max fps 30) :dir dir))
           ;; Crop to the real content (trim empty desktop borders) -- everything
           ;; downstream works in this cropped frame.
           (crop     (or (compute-content-bbox rec) '(0.0 0.0 1.0 1.0)))
           (session  (recording->session rec crop))
           (damage   (%crop-damage (compute-damage rec) crop))  ; where the screen changed
           (timeline (progn (setf (dir:session-damage session) damage)
                            (dir:plan-timeline session)))  ; fit zoom to activity
           ;; eased cursor track (D2 spring) for the overlay -- METADATA hid the
           ;; real cursor, so we draw a smoothed one at the tracked position.
           (eased    (dir:make-session :width (dir:session-width session)
                                       :height (dir:session-height session)
                                       :cursor (dir:ease-cursor session))))
      (format t "  [record] captured ~D frames (~D cursor, ~D changed) -> ~D keyframes~%"
              (length (getf rec :frames))
              (length (dir:session-cursor session))
              (length damage)
              (length timeline))
      ;; No :duration -> compose uses the actual captured span (you decide the
      ;; length by when you click Stop).
      (compose-recording rec timeline :out out :scale scale
                         :fps fps :cursor-session eased :crop crop))))
