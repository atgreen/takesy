;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;; audio.lisp
;;;;
;;;; Opt-in audio capture (green-screen: --audio). The ScreenCast portal delivers
;;;; video only, so audio is a parallel recorder: an ffmpeg PulseAudio process
;;;; (PipeWire exposes a pulse-compatible server) writing <dir>/audio.wav for the
;;;; same wall-clock window as the frame capture. The render stage muxes that wav
;;;; into the mp4, so audio rides the capture/render split for free.
;;;;
;;;; Everything here is best-effort: if a monitor/mic source can't be resolved or
;;;; ffmpeg won't launch, we log and record video only rather than fail the run.

(defpackage #:takesy/audio
  (:use #:cl)
  (:export #:start-audio #:stop-audio #:audio-handle #:audio-handle-path
           #:*sync-beep-freq* #:detect-sync-beep))

(in-package #:takesy/audio)

;;; ------------------------------------------------------------------
;;; Sync clapperboard (green-screen-kvi). The count-in plays a short tone AFTER the
;;; audio recorder is live, so it lands in audio.wav; DETECT-SYNC-BEEP finds its
;;; onset there. Anchoring to a marker in the ACTUAL recording (not stream
;;; durations) removes the stop-skew that made audio lead the picture.

(defparameter *sync-beep-freq* 1320
  "Frequency (Hz) of the count-in 'go' tone, reused as the A/V sync clapperboard.")

(defun %decode-wav-s16-mono (path sr &key (seconds 4.0))
  "Decode the first SECONDS of PATH to mono signed-16 PCM at SR Hz via ffmpeg;
return a (simple-array (signed-byte 16)) of samples, or NIL on failure."
  (let ((tmp (format nil "~A.sync~D.raw" path (random 100000))))
    (unwind-protect
         (when (ignore-errors
                (uiop:run-program
                 (list "ffmpeg" "-y" "-loglevel" "error" "-t" (format nil "~,3F" seconds)
                       "-i" (namestring path) "-ac" "1" "-ar" (format nil "~D" sr)
                       "-f" "s16le" tmp)
                 :output nil :error-output nil)
                (probe-file tmp))
           (with-open-file (s tmp :element-type '(unsigned-byte 8))
             (let* ((n (floor (file-length s) 2))
                    (out (make-array n :element-type '(signed-byte 16)))
                    (buf (make-array (* n 2) :element-type '(unsigned-byte 8))))
               (read-sequence buf s)
               (dotimes (i n out)
                 (let ((v (logior (aref buf (* i 2)) (ash (aref buf (1+ (* i 2))) 8))))
                   (setf (aref out i) (if (>= v 32768) (- v 65536) v)))))))
      (ignore-errors (delete-file tmp)))))

(defun %goertzel-energy (samples base win coeff)
  "Goertzel band power of WIN samples of SAMPLES starting at BASE, for a band whose
detector COEFF is 2*cos(2*pi*f/sr)."
  (let ((s0 0.0d0) (s1 0.0d0))
    (dotimes (i win)
      (let ((s (+ (* coeff s0) (- s1) (float (aref samples (+ base i)) 1.0d0))))
        (setf s1 s0 s0 s)))
    (/ (+ (* s0 s0) (* s1 s1) (- (* coeff s0 s1))) win)))

(defparameter *sync-beep-tonality* 4.0
  "A hop is TONAL at the beep frequency when its band energy exceeds an off-band
reference by this factor. A pure tone concentrates energy in its band; broadband
noise and click transients spread it, so they fail this test (green-screen-kvi).")

(defun detect-sync-beep (path &key (freq *sync-beep-freq*) (search 4.0) (sr 8000))
  "Onset time (seconds) of the sync beep -- a sustained pure tone at FREQ -- in the
first SEARCH seconds of the WAV at PATH. Uses two Goertzel bands: FREQ and an
off-band reference; a hop counts only when FREQ dominates the reference by
*sync-beep-tonality* AND rises well above the noise floor. The onset is the start of
the longest such run (>= ~30 ms). Returns NIL when there is no clear tone (audio
without the count-in), so the caller falls back to the duration estimate."
  (let ((samples (%decode-wav-s16-mono path sr :seconds search)))
    (when (and samples (> (length samples) sr))
      (let* ((win (round (* sr 0.02)))                        ; 20 ms window
             (hop (round (* sr 0.005)))                       ; 5 ms hop
             (cf  (* 2.0d0 (cos (* 2.0d0 pi (/ (float freq 1.0d0) sr)))))
             (rf  (* 2.0d0 (cos (* 2.0d0 pi (/ (float (- freq 400) 1.0d0) sr))))) ; off-band ref
             (nhops (floor (- (length samples) win) hop))
             (en  (make-array (max 1 nhops) :element-type 'double-float))
             (tonal (make-array (max 1 nhops) :element-type 'bit :initial-element 0)))
        (dotimes (h nhops)
          (let* ((base (* h hop))
                 (e (%goertzel-energy samples base win cf))
                 (r (%goertzel-energy samples base win rf)))
            (setf (aref en h) e)
            (when (> e (* *sync-beep-tonality* (max r 1.0d0))) (setf (aref tonal h) 1))))
        ;; noise floor from the median band energy; a real tone peaks far above it.
        (let* ((sorted (sort (copy-seq en) #'<))
               (median (aref sorted (floor (length sorted) 2)))
               (floor* (* 12.0d0 (max median 1.0d0)))
               (min-hops (max 1 (round (/ 0.03 (/ hop (float sr 1.0))))))
               (best-len 0) (best-start -1) (run 0) (run-start 0))
          (dotimes (h nhops)
            (cond ((and (= (aref tonal h) 1) (> (aref en h) floor*))
                   (when (zerop run) (setf run-start h))
                   (incf run)
                   (when (> run best-len) (setf best-len run best-start run-start)))
                  (t (setf run 0))))
          (when (>= best-len min-hops)
            (* best-start hop (/ 1.0 sr))))))))

(defun %pactl (subcommand)
  "Run `pactl SUBCOMMAND` and return its trimmed one-line output, or NIL on any
failure (pactl missing, no server, empty)."
  (ignore-errors
   (let ((out (uiop:run-program (list "pactl" subcommand)
                                :output '(:string :stripped t)
                                :ignore-error-status nil)))
     (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) out)))
       (when (plusp (length s)) s)))))

(defun %default-monitor ()
  "Pulse/PipeWire monitor source of the default sink (system/desktop audio), or
NIL. The monitor of sink SINK is named SINK.monitor."
  (let ((sink (%pactl "get-default-sink")))
    (when sink (format nil "~A.monitor" sink))))

(defun %default-source ()
  "Default input source (microphone), or NIL."
  (%pactl "get-default-source"))

(defun %sources-for (mode)
  "Resolve MODE (:system | :mic | :both) to a list of Pulse source names, dropping
any that couldn't be resolved."
  (remove nil
          (ecase mode
            (:system (list (%default-monitor)))
            (:mic    (list (%default-source)))
            (:both   (list (%default-monitor) (%default-source))))))

(defstruct audio-handle proc path)

(defun %record-command (sources path)
  "Build the ffmpeg command: one `-f pulse -i SRC` per source; a single source is
auto-mapped, multiple sources are amix'd into one track. Writes WAV to PATH."
  (let ((inputs (loop for src in sources append (list "-f" "pulse" "-i" src)))
        (mix    (when (> (length sources) 1)
                  (list "-filter_complex"
                        (format nil "~{[~D:a]~}amix=inputs=~D:duration=longest[a]"
                                (loop for i below (length sources) collect i)
                                (length sources))
                        "-map" "[a]"))))
    (append (list "ffmpeg" "-y" "-loglevel" "error")
            inputs mix (list path))))

(defun start-audio (dir mode)
  "Launch a PulseAudio recorder for MODE writing <DIR>/audio.wav and return an
AUDIO-HANDLE, or NIL (logged) when no source resolves or ffmpeg won't start.
MODE is :system, :mic, or :both."
  (let* ((path    (format nil "~A/audio.wav" (string-right-trim "/" dir)))
         (sources (%sources-for mode)))
    (if (null sources)
        (progn
          (format t "  [audio] no usable ~(~A~) source (pactl); recording video only~%" mode)
          nil)
        (handler-case
            (let ((proc (uiop:launch-program (%record-command sources path)
                                             :input :stream
                                             :output nil :error-output nil)))
              (format t "  [audio] recording ~(~A~) (~{~A~^ + ~}) -> ~A~%" mode sources path)
              (make-audio-handle :proc proc :path path))
          (error (e)
            (format t "  [audio] failed to start (~A); recording video only~%" e)
            nil)))))

(defun %nonempty-wav-p (path)
  "T if PATH exists and is larger than a bare 44-byte WAV header (i.e. has data)."
  (and (probe-file path)
       (ignore-errors
        (> (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s))
           44))))

(defun stop-audio (handle)
  "Gracefully stop the recorder so ffmpeg finalizes the WAV: write 'q' to its
stdin, then wait. Return the wav path if audio was actually captured, else NIL.
Best-effort -- never signals."
  (when handle
    ;; 'q' on ffmpeg's stdin is a graceful quit that flushes the container.
    (ignore-errors
     (let ((in (uiop:process-info-input (audio-handle-proc handle))))
       (when in (write-char #\q in) (finish-output in) (close in))))
    ;; After 'q' ffmpeg just finalizes the WAV -- normally instant. Bound the wait
    ;; so a wedged recorder can't hang capture teardown (green-screen-zqb.4).
    (let ((proc (audio-handle-proc handle)))
      (ignore-errors
       (handler-case (sb-ext:with-timeout 15 (uiop:wait-process proc))
         (sb-ext:timeout ()
           (ignore-errors (uiop:terminate-process proc :urgent t))
           (ignore-errors (uiop:wait-process proc))))))
    (let ((path (audio-handle-path handle)))
      (if (%nonempty-wav-p path)
          path
          (progn (format t "  [audio] no audio captured (empty/missing wav)~%") nil)))))
