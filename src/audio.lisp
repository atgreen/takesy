;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
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
  (:export #:start-audio #:stop-audio #:audio-handle #:audio-handle-path))

(in-package #:takesy/audio)

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
    (ignore-errors (uiop:wait-process (audio-handle-proc handle)))
    (let ((path (audio-handle-path handle)))
      (if (%nonempty-wav-p path)
          path
          (progn (format t "  [audio] no audio captured (empty/missing wav)~%") nil)))))
