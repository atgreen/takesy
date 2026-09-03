;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;; cli.lisp
;;;;
;;;; The `takesy` command-line entry point, built on clingon. Recording is the
;;;; default action: `takesy [options]` captures the screen, directs it with the
;;;; editorial camera, and writes an mp4 (via takesy/record). `capture` and
;;;; `render` split the pipeline so one capture can be re-rendered with any tuning.

(defpackage #:takesy/cli
  (:use #:cl)
  (:local-nicknames (#:rec #:takesy/record) (#:wc #:takesy/webcam)
                    (#:wcp #:takesy/webcam-preview))
  (:export #:main #:run #:*version*))

(in-package #:takesy/cli)

(defparameter *version* "1.5.0"
  "takesy version string, surfaced via `takesy --version`.")

;;; ------------------------------------------------------------------
;;; Value parsers (shared by the option handlers).

(defun %float (v default)
  "Parse a real-number option string V (no read-eval), or DEFAULT when absent."
  (if (and v (stringp v) (plusp (length v)))
      (let ((*read-eval* nil))
        (let ((n (read-from-string v)))
          (unless (realp n) (error "expected a number, got ~S" v))
          (float n 1.0)))
      default))

(defparameter +color-names+
  '(("black" 0.0 0.0 0.0) ("white" 1.0 1.0 1.0)
    ("gray" 0.5 0.5 0.5) ("grey" 0.5 0.5 0.5)
    ("dark" 0.11 0.12 0.15) ("charcoal" 0.13 0.13 0.14)
    ("navy" 0.08 0.11 0.20) ("slate" 0.16 0.19 0.24)))

(defun parse-color (str)
  "Parse a background colour: `#RRGGBB` / `RRGGBB` hex, or a name (black, white,
gray, dark, navy, slate, ...). Return (r g b) in 0..1."
  (let ((named (assoc (string-downcase (string-trim " " str)) +color-names+
                      :test #'string=)))
    (cond
      (named (rest named))
      (t (let ((s (string-left-trim "#" (string-trim " " str))))
           (unless (and (= (length s) 6) (every (lambda (c) (digit-char-p c 16)) s))
             (error "bad --bg colour ~S (use #RRGGBB or a name like dark/navy/black)" str))
           (flet ((h (a b) (/ (parse-integer s :start a :end b :radix 16) 255.0)))
             (list (h 0 2) (h 2 4) (h 4 6))))))))

(defun parse-xy (str)
  "Parse `X,Y` (two reals) into a (cons x . y). Used for --cursor-hotspot."
  (let ((c (position #\, str)))
    (unless c (error "expected X,Y (comma-separated), got ~S" str))
    (cons (%float (subseq str 0 c) 0.0) (%float (subseq str (1+ c)) 0.0))))

(defun parse-audio (str)
  "Parse --audio into a source mode: system/desktop/monitor -> :system,
mic/microphone -> :mic, both/on/mix -> :both, off/none or absent -> NIL."
  (if (null str)
      nil
      (let ((s (string-downcase (string-trim " " str))))
        (cond
          ((member s '("off" "none" "no") :test #'string=) nil)
          ((member s '("system" "desktop" "monitor") :test #'string=) :system)
          ((member s '("mic" "microphone" "input") :test #'string=) :mic)
          ((member s '("both" "on" "yes" "all" "mix") :test #'string=) :both)
          (t (error "bad --audio ~S (use system, mic, both, or off)" str))))))

(defun parse-aspect (str)
  "Parse --aspect `W:H` (e.g. 9:16, 1:1, 16:9) into a (cons w . h), or NIL."
  (if (null str)
      nil
      (let ((c (position #\: str)))
        (unless c (error "bad --aspect ~S (use W:H, e.g. 9:16, 1:1, 16:9)" str))
        (let ((w (parse-integer str :end c :junk-allowed nil))
              (h (parse-integer str :start (1+ c) :junk-allowed nil)))
          (unless (and (plusp w) (plusp h))
            (error "bad --aspect ~S (W and H must be positive)" str))
          (cons w h)))))

(defun parse-region (str)
  "Parse --region `X,Y,W,H` (source pixels) into a list (x y w h), or NIL."
  (if (null str)
      nil
      (let ((parts (loop with start = 0
                         for pos = (position #\, str :start start)
                         collect (parse-integer str :start start :end pos)
                         while pos do (setf start (1+ pos)))))
        (unless (= (length parts) 4)
          (error "bad --region ~S (use X,Y,W,H in pixels)" str))
        (when (or (minusp (third parts)) (minusp (fourth parts)))
          (error "bad --region ~S (W and H must be non-negative)" str))
        parts)))

(defun parse-webcam-pos (str)
  "Parse --webcam-pos into a corner keyword: br/bl/tr/tl (default :br)."
  (if (null str)
      :br
      (let ((s (string-downcase (string-trim " " str))))
        (cond ((member s '("br" "bottom-right") :test #'string=) :br)
              ((member s '("bl" "bottom-left")  :test #'string=) :bl)
              ((member s '("tr" "top-right")    :test #'string=) :tr)
              ((member s '("tl" "top-left")     :test #'string=) :tl)
              (t (error "bad --webcam-pos ~S (use br, bl, tr, tl)" str))))))

;;; ------------------------------------------------------------------
;;; Option sets. Fresh lists per command (clingon options are stateful).

(defun capture-options ()
  "Options that govern the capture stage (record + capture)."
  (list
   (clingon:make-option :string :description "recording dir"
                        :long-name "dir" :initial-value "/tmp/takesy-rec" :key :dir)
   (clingon:make-option :integer :description "optional max seconds (safety cap); default none -- click Stop to finish"
                        :long-name "duration" :key :duration)
   (clingon:make-option :integer :description "capture/output frames per second"
                        :long-name "fps" :initial-value 24 :key :fps)
   (clingon:make-option :string :description "record audio: system | mic | both | off"
                        :long-name "audio" :key :audio)
   (clingon:make-option :integer :description "count down N seconds before recording (0=off)"
                        :long-name "countdown" :initial-value 3 :key :countdown)
   ;; --webcam doubles as a live-capture trigger: a /dev/video* node or "auto"
   ;; records the camera during capture; any other path is a pre-recorded PiP clip
   ;; consumed at render time (handled in render-options for the standalone render).
   (clingon:make-option :string
                        :description "webcam PiP: /dev/videoN or auto (live), or a video/image file"
                        :long-name "webcam" :key :webcam)))

(defun render-options (&key (webcam t))
  "Options that govern the direct + composite stages (record + render). WEBCAM
includes the --webcam option; pass NIL for the combined `record' command, where
capture-options already carries --webcam (so it isn't declared twice)."
  (remove
   nil
   (list
   (clingon:make-option :string :description "output file (.gif path -> animated GIF, no audio)"
                        :short-name #\o :long-name "output"
                        :initial-value "/tmp/takesy-record.mp4" :key :output)
   (clingon:make-option :integer :description "max output height, px (never upscales)"
                        :long-name "height" :initial-value 1200 :key :height)
   (clingon:make-option :string :description "background: blur (default) | #RRGGBB | name (dark/navy/...)"
                        :long-name "bg" :key :bg)
   (clingon:make-option :string :description "background image (cover-fit behind the inset)"
                        :long-name "bg-image" :key :bg-image)
   (clingon:make-option :string :description "reframe to aspect W:H (letterboxed, no crop)"
                        :long-name "aspect" :key :aspect)
   (clingon:make-option :string :description "inset margin, fraction of frame (default 0.04)"
                        :long-name "margin" :initial-value "0.04" :key :margin)
   (clingon:make-option :string :description "fixed screen region X,Y,W,H (source px)"
                        :long-name "region" :key :region)
   (clingon:make-option :string :description "rounded-corner radius fraction (default 0.09; 0=square)"
                        :long-name "corner-radius" :initial-value "0.09" :key :corner-radius)
   (clingon:make-option :string :description "custom cursor image (png/...)"
                        :long-name "cursor" :key :cursor)
   (clingon:make-option :string :description "cursor hotspot X,Y as image fraction (default 0,0)"
                        :long-name "cursor-hotspot" :initial-value "0,0" :key :cursor-hotspot)
   (clingon:make-option :string :description "cursor height as output-height fraction (default 0.06)"
                        :long-name "cursor-size" :key :cursor-size)
   (clingon:make-option :boolean :description "click ripples + cursor press (default on)"
                        :long-name "ripples" :initial-value :true :key :ripples)
   (clingon:make-option :string :description "click-ripple prominence (1.0 = subtle original, default 1.6; higher = bolder)"
                        :long-name "ripple-intensity" :initial-value "1.6" :key :ripple-intensity)
   (clingon:make-option :string :description "camera style: calm (tutorial, default) or energetic (promo); recordings over 5min force calm"
                        :long-name "style" :initial-value "calm" :key :style)
   (clingon:make-option :choice :description "how the camera frames screen activity: reading (keep the left column/top context, default) or center (center on the change)"
                        :long-name "damage-anchor" :items '("reading" "center")
                        :initial-value "reading" :key :damage-anchor)
   (clingon:make-option :boolean :description "reduced-motion: cut between shots instead of animating pans/zooms (accessibility)"
                        :long-name "reduced-motion" :initial-value :false :key :reduced-motion)
   (clingon:make-option :boolean :description "print the motion-linter report (shot reasons + best-practice warnings)"
                        :long-name "lint" :initial-value :false :key :lint)
   (clingon:make-option :string :description "A/V sync nudge in seconds (+ pushes audio later to fix audio that leads the video; default 0)"
                        :long-name "audio-offset" :initial-value "0" :key :audio-offset)
   (clingon:make-option :string :description "webcam PiP sync nudge in seconds (+ pushes the webcam later to fix a webcam that leads the video; default 0)"
                        :long-name "webcam-offset" :initial-value "0" :key :webcam-offset)
   (when webcam
     (clingon:make-option :string
                          :description "webcam PiP: a video/image file (or /dev/videoN|auto for live)"
                          :long-name "webcam" :key :webcam))
   (clingon:make-option :choice :description "webcam inset corner"
                        :long-name "webcam-pos" :items '("br" "bl" "tr" "tl")
                        :initial-value "br" :key :webcam-pos)
   (clingon:make-option :string :description "webcam diameter as output-height fraction (default 0.22)"
                        :long-name "webcam-size" :initial-value "0.22" :key :webcam-size)
   (clingon:make-option :string :description "webcam corner radius, fraction of half-size: 1=circle, 0=square (default 1)"
                        :long-name "webcam-corner" :initial-value "1.0" :key :webcam-corner)
   (clingon:make-option :string :description "webcam border width, fraction of inset (default 0.012; 0=none)"
                        :long-name "webcam-border" :initial-value "0.012" :key :webcam-border)
   (clingon:make-option :string :description "webcam border colour: name or #RRGGBB (default white)"
                        :long-name "webcam-border-color" :initial-value "white" :key :webcam-border-color)
   (clingon:make-option :string :description "webcam framing zoom, >=1 (usually set in the auto preview)"
                        :long-name "webcam-zoom" :initial-value "1.0" :key :webcam-zoom)
   (clingon:make-option :string :description "webcam framing pan-x, source fraction -0.5..0.5"
                        :long-name "webcam-pan-x" :initial-value "0.0" :key :webcam-pan-x)
   (clingon:make-option :string :description "webcam framing pan-y, source fraction -0.5..0.5"
                        :long-name "webcam-pan-y" :initial-value "0.0" :key :webcam-pan-y)
   (clingon:make-option :flag :description "cut idle stretches to speed up demos (drops audio)"
                        :long-name "trim-idle" :key :trim-idle)
   (clingon:make-option :string :description "idle gap to cut, seconds (default 1.2)"
                        :long-name "idle-threshold" :initial-value "1.2" :key :idle-threshold)
   (clingon:make-option :string :description "kept time per cut gap, seconds (default 0.4)"
                        :long-name "max-idle" :initial-value "0.4" :key :max-idle)
   (clingon:make-option :string :description "cursor-spring stiffness when moving fast (default 60.0; higher tracks tighter)"
                        :long-name "cursor-omega-fast" :initial-value "60.0" :key :cursor-omega-fast)
   (clingon:make-option :string :description "seconds before a click to aim the cursor (default 0.15)"
                        :long-name "cursor-anticipate" :initial-value "0.15" :key :cursor-anticipate)
   (clingon:make-option :string :description "smooth hand tremor out of the auto-zoom camera, seconds (default 0.15; 0=off)"
                        :long-name "camera-smoothing" :initial-value "0.15" :key :camera-smoothing)
   (clingon:make-option :string :description "write the per-frame camera path (zoom/pan) to CSV"
                        :long-name "log-camera" :key :log-camera))))

;;; ------------------------------------------------------------------
;;; Turning parsed options into RENDER-RECORDING keyword args.

(defun %truthy (v)
  "Normalize a clingon boolean/flag value (:true/:false, t/nil) to a Lisp boolean."
  (case v ((:true t) t) ((:false nil) nil) (t (and v t))))

(defun %render-args (cmd)
  "Build the RENDER-RECORDING keyword-arg plist from CMD's parsed options."
  (flet ((g (k) (clingon:getopt cmd k)))
    (let* ((bg-str  (g :bg))
           (bg-blur (or (null bg-str) (string-equal (string-trim " " bg-str) "blur"))))
      (list :fps               (g :fps)
            :max-height        (g :height)
            :bg-blur           bg-blur
            :bg                (if (and bg-str (not bg-blur))
                                   (parse-color bg-str) '(0.11 0.12 0.15))
            :bg-image          (g :bg-image)
            ;; Only a FILE reaches render as --webcam; a live spec (/dev/videoN|auto)
            ;; is captured during recording and render picks the clip up from the
            ;; manifest, so don't hand render a device path (green-screen-asp).
            :webcam            (let ((w (g :webcam)))
                                 (unless (wc:live-spec-p w) w))
            :webcam-pos        (parse-webcam-pos (g :webcam-pos))
            :webcam-size       (%float (g :webcam-size) 0.22)
            :webcam-corner     (%float (g :webcam-corner) 1.0)
            :webcam-border     (%float (g :webcam-border) 0.012)
            :webcam-border-color (parse-color (or (g :webcam-border-color) "white"))
            ;; NIL when at default so a manifest's previewed framing wins on re-render;
            ;; an explicit flag overrides it.
            :webcam-zoom       (let ((z (%float (g :webcam-zoom) 1.0)))  (unless (= z 1.0) z))
            :webcam-pan-x      (let ((p (%float (g :webcam-pan-x) 0.0))) (unless (= p 0.0) p))
            :webcam-pan-y      (let ((p (%float (g :webcam-pan-y) 0.0))) (unless (= p 0.0) p))
            :ripples           (%truthy (g :ripples))
            :ripple-intensity  (%float (g :ripple-intensity) 1.6)
            :style             (let ((s (string-downcase (or (g :style) "calm"))))
                                 (cond ((string= s "energetic") :energetic)
                                       (t :calm)))
            :reduced-motion    (%truthy (g :reduced-motion))
            :damage-anchor     (if (string= (string-downcase (or (g :damage-anchor) "reading")) "center")
                                   :center :reading)
            :lint              (%truthy (g :lint))
            :audio-offset      (%float (g :audio-offset) 0.0)
            :webcam-offset     (%float (g :webcam-offset) 0.0)
            :aspect            (parse-aspect (g :aspect))
            :region            (parse-region (g :region))
            :trim-idle         (%truthy (g :trim-idle))
            :idle-threshold    (%float (g :idle-threshold) 1.2)
            :max-idle          (%float (g :max-idle) 0.4)
            :corner            (%float (g :corner-radius) 0.09)
            :margin            (%float (g :margin) 0.04)
            :cursor            (g :cursor)
            :cursor-hotspot    (let ((s (g :cursor-hotspot)))
                                 (if s (parse-xy s) '(0.0 . 0.0)))
            :cursor-size       (let ((s (g :cursor-size))) (when s (%float s 0.06)))
            :cursor-omega-fast (%float (g :cursor-omega-fast) 60.0)
            :cursor-anticipate (%float (g :cursor-anticipate) 0.15)
            :camera-smoothing  (%float (g :camera-smoothing) 0.15)
            :camera-log        (g :log-camera)
            :out               (g :output)))))

;;; ------------------------------------------------------------------
;;; Command handlers.

(defun %have-display-p ()
  "T when a desktop session is present (so we can pop a browser). takesy needs one
to capture the screen anyway; this just guards a truly headless invocation."
  (or (uiop:getenv "WAYLAND_DISPLAY") (uiop:getenv "DISPLAY")))

(defun %maybe-preview (live)
  "Whenever LIVE names a live camera (and a desktop is present), open the framing
preview to pick the camera + adjust zoom/pan -- always, since a webcam recording
wants framing. Return (values device zoom pan-x pan-y): the user's choices, or LIVE
with neutral framing when there's no display or the user cancels."
  (if (and live (%have-display-p))
      (let ((r (wcp:run-preview
                :initial-device (unless (string-equal live "auto") live))))
        (if r
            (values (getf r :device) (getf r :zoom) (getf r :pan-x) (getf r :pan-y))
            (values live 1.0 0.0 0.0)))
      (values live 1.0 0.0 0.0)))

(defun handle-record (cmd)
  "Full pipeline: capture then render in one shot (the default action)."
  (let* ((dur   (clingon:getopt cmd :duration))
         (fps   (clingon:getopt cmd :fps))
         (dir   (clingon:getopt cmd :dir))
         (audio (parse-audio (clingon:getopt cmd :audio)))
         (webcam (clingon:getopt cmd :webcam))
         (live  (and (wc:live-spec-p webcam) webcam))   ; /dev/videoN|auto -> live
         (ra    (%render-args cmd)))
    (format t "takesy: recording~@[ up to ~Ds~] @ ~Dfps, up to ~Dp tall~@[ +audio(~(~A~))~]~@[ +webcam(~A)~] -> ~A~%"
            dur fps (getf ra :max-height) audio live (getf ra :out))
    (format t "  a screen-share dialog will appear -- pick a source.~%~
                 click GNOME's Stop button (top bar) to finish; the cursor hides~%~
                 during capture (auto-zoom needs it) and is restored on exit.~%")
    (multiple-value-bind (dev z px py) (%maybe-preview live)
     (let ((recording (rec:capture-recording :duration (and dur (float dur 1.0)) :fps fps :dir dir
                                            :audio audio :webcam dev
                                            :webcam-zoom z :webcam-pan-x px :webcam-pan-y py
                                            :countdown (clingon:getopt cmd :countdown))))
      (multiple-value-bind (path n) (apply #'rec:render-recording recording ra)
        (format t "done: wrote ~A (~D frames)~%" path n)
        (format t "  re-render this capture with: takesy render ~A [--bg ... ]~%" dir)
        path)))))

(defun handle-capture (cmd)
  "Capture stage only: record to DIR for later (re-)rendering."
  (let* ((dur   (clingon:getopt cmd :duration))
         (fps   (clingon:getopt cmd :fps))
         (dir   (clingon:getopt cmd :dir))
         (audio (parse-audio (clingon:getopt cmd :audio)))
         (webcam (clingon:getopt cmd :webcam))
         (live  (and (wc:live-spec-p webcam) webcam)))
    (when (and webcam (not live))
      (format t "  [capture] note: --webcam ~S is a file, not a live device -- capture~%~
                   only records live cameras (/dev/videoN or auto). Pass the file at~%~
                   render time instead: takesy render ~A --webcam ~A~%" webcam dir webcam))
    (format t "takesy: capturing~@[ up to ~Ds~] @ ~Dfps~@[ +audio(~(~A~))~]~@[ +webcam(~A)~] -> ~A~%"
            dur fps audio live dir)
    (format t "  a screen-share dialog will appear -- pick a source.~%~
                 click GNOME's Stop button (top bar) to finish.~%")
    (multiple-value-bind (dev z px py) (%maybe-preview live)
     (let ((recording (rec:capture-recording :duration (and dur (float dur 1.0)) :fps fps :dir dir
                                            :audio audio :webcam dev
                                            :webcam-zoom z :webcam-pan-x px :webcam-pan-y py
                                            :countdown (clingon:getopt cmd :countdown))))
      (format t "done: captured ~D frames to ~A~%" (length (getf recording :frames)) dir)
      (format t "  render it with: takesy render ~A [--output out.mp4 --bg ... ]~%" dir)
      dir))))

(defun handle-render (cmd)
  "Direct + composite stages only: render a previously-captured DIR to an mp4."
  (let ((dir (first (clingon:command-arguments cmd))))
    (unless dir
      (error "render needs a recording DIR: takesy render DIR [options]"))
    (let ((ra (%render-args cmd)))
      (format t "takesy: rendering ~A -> ~A~%" dir (getf ra :out))
      (multiple-value-bind (path n) (apply #'rec:render-recording-dir dir ra)
        (format t "done: wrote ~A (~D frames)~%" path n)
        path))))

;;; ------------------------------------------------------------------
;;; Command tree.

(defun capture-command ()
  (clingon:make-command
   :name "capture" :description "Capture only; save to a dir for later rendering."
   :options (capture-options) :handler #'handle-capture))

(defun render-command ()
  (clingon:make-command
   :name "render" :description "Render a captured dir to an mp4 (re-run with any tuning)."
   :usage "DIR [options]"
   :options (render-options) :handler #'handle-render))

(defun record-command ()
  (clingon:make-command
   :name "record" :description "Capture your screen, direct it, and write an mp4."
   ;; capture-options already declares --webcam (its live-capture trigger), so drop
   ;; render-options' copy to avoid declaring the same option twice.
   :options (append (capture-options) (render-options :webcam nil)) :handler #'handle-record))

(defun top-level-command ()
  "The `takesy` command. With no sub-command it records (the default action)."
  (clingon:make-command
   :name "takesy"
   :version *version*
   :description "screen recorder for modern Linux desktops (Wayland & X11) with an
editorial auto-zoom camera. With no sub-command, takesy records your screen."
   :authors '("Anthony Green <green@moxielogic.com>")
   :license "GPL-3.0-or-later"
   :options (append (capture-options) (render-options :webcam nil))
   :handler #'handle-record
   :sub-commands (list (capture-command) (render-command) (record-command))))

(defun run (args)
  "Dispatch ARGS (the command line minus argv0). REPL-callable."
  (clingon:run (top-level-command) args))

(defun main ()
  (handler-case
      (progn (run (rest sb-ext:*posix-argv*))
             (finish-output)
             (sb-ext:exit :code 0))
    (error (e)
      (format *error-output* "takesy: ~A~%" e)
      (finish-output *error-output*)
      (sb-ext:exit :code 1))))
