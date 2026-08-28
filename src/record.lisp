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
  (:export #:recording->session #:compute-damage #:compose-recording #:record-to-mp4))

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

(defun recording->session (rec)
  "Build a Director SESSION from a capture recording plist: the cursor track is
the frames that carried a cursor position, in capture order."
  (let* ((frames (getf rec :frames))
         (cursor (loop for f in frames
                       when (getf f :cursor-x)
                         collect (dir:make-cursor-sample
                                  :time (float (getf f :time) 1.0)
                                  :x (float (getf f :cursor-x) 1.0)
                                  :y (float (getf f :cursor-y) 1.0)))))
    (dir:make-session :width (getf rec :width) :height (getf rec :height)
                      :cursor cursor :events '())))

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
                                            (cursor-session nil))
  "Render REC's real BGRx frames through the compositor driven by TIMELINE, at a
STEADY output FPS over DURATION seconds (default: the captured time span). Static
stretches -- where the screencast emitted no frame -- hold the previous frame, so
the clip is always full-length and smooth regardless of how sparsely the
compositor delivered frames. SCALE downsamples the 4K source for a sane encode."
  (let* ((frames (coerce (getf rec :frames) 'vector))
         (nsrc (length frames)))
    (when (zerop nsrc) (error "recording has no frames"))
    (let* ((span (float (getf (aref frames (1- nsrc)) :time) 1.0))
           (dur  (or duration span))
           (nout (max 1 (round (* fps dur))))
           (sw (getf rec :width)) (sh (getf rec :height))
           (ow (* 2 (max 1 (round (/ sw scale 2)))))
           (oh (* 2 (max 1 (round (/ sh scale 2)))))
           (cache-idx -1) (cache-bytes nil)
           (cursor-fn (when cursor-session
                        (lambda (i)
                          (multiple-value-bind (x y)
                              (dir:cursor-at cursor-session (/ i (float fps 1.0)))
                            (cons (/ x (float sw 1.0)) (/ y (float sh 1.0))))))))
      (format t "  [record] compositing ~D src frames -> ~D output frames ~
                 (~,1Fs @ ~Dfps) ~Dx~D -> ~Dx~D~@[ +cursor~]~%"
              nsrc nout dur fps sw sh ow oh cursor-fn)
      (flet ((src (i)   ; nearest source frame for output time i/fps, cached
               (let ((idx (%nearest-frame-index frames (/ i (float fps 1.0)))))
                 (unless (= idx cache-idx)
                   (setf cache-idx idx
                         cache-bytes (%read-file-bytes (getf (aref frames idx) :path))))
                 cache-bytes)))
        (comp:render-frame-sequence
         timeline #'src nout ow oh
         :fps fps :source-format :bgra
         :source-width sw :source-height sh
         :time-fn (lambda (i) (/ i (float fps 1.0)))
         :cursor-fn cursor-fn
         :path out)))))

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
           (session  (recording->session rec))
           (damage   (compute-damage rec))           ; where the screen changed
           (timeline (progn (setf (dir:session-damage session) damage)
                            (dir:plan-timeline session)))  ; fit zoom to activity
           ;; eased cursor track (D2 spring) for the overlay -- METADATA hid the
           ;; real cursor, so we draw a smoothed one at the tracked position.
           (eased    (dir:make-session :width (getf rec :width) :height (getf rec :height)
                                       :cursor (dir:ease-cursor session))))
      (format t "  [record] captured ~D frames (~D cursor, ~D changed) -> ~D keyframes~%"
              (length (getf rec :frames))
              (length (dir:session-cursor session))
              (length damage)
              (length timeline))
      ;; No :duration -> compose uses the actual captured span (you decide the
      ;; length by when you click Stop).
      (compose-recording rec timeline :out out :scale scale
                         :fps fps :cursor-session eased))))
