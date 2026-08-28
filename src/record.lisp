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
  (:export #:recording->session #:compose-recording #:record-to-mp4))

(in-package #:takesy/record)

(defun %read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

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

(defun compose-recording (rec timeline &key (out "/tmp/takesy-record.mp4") (scale 3))
  "Render REC's real BGRx frames through the compositor driven by TIMELINE.
SCALE downsamples the output (capture / SCALE, rounded even) so 4K encodes stay
sane while the shader still samples the full-res source texture."
  (let* ((frames (coerce (getf rec :frames) 'vector))
         (n  (length frames))
         (sw (getf rec :width)) (sh (getf rec :height))
         (fps (max 1 (or (getf rec :fps) 30)))
         (ow (* 2 (max 1 (round (/ sw scale 2)))))
         (oh (* 2 (max 1 (round (/ sh scale 2))))))
    (when (zerop n) (error "recording has no frames"))
    (format t "  [record] compositing ~D frames ~Dx~D -> ~Dx~D~%" n sw sh ow oh)
    (comp:render-frame-sequence
     timeline
     (lambda (i) (%read-file-bytes (getf (aref frames i) :path)))
     n ow oh
     :fps fps :source-format :bgra
     :source-width sw :source-height sh
     :time-fn (lambda (i) (float (getf (aref frames i) :time) 1.0))
     :path out)))

(defun record-to-mp4 (&key (duration 4.0) (fps 12) (scale 3)
                           (dir "/tmp/takesy-rec") (out "/tmp/takesy-record.mp4"))
  "Full `takesy record`: capture DURATION seconds (METADATA cursor mode, armed
teardown), auto-zoom via the Director (dwell-based, no evdev needed), and render
the real captured frames to an mp4 at OUT. Return (values out n-frames)."
  (portal:with-screencast (fd node :cursor-mode portal:+cursor-metadata+)
    (let* ((rec      (pw:record-frames fd node :duration duration :max-fps fps :dir dir))
           (session  (recording->session rec))
           (timeline (dir:plan-timeline session)))   ; :auto -> dwell (no events)
      (format t "  [record] captured ~D frames, ~D cursor samples -> ~D keyframes~%"
              (length (getf rec :frames))
              (length (dir:session-cursor session))
              (length timeline))
      (compose-recording rec timeline :out out :scale scale))))
