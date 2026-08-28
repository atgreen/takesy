;;;; director.lisp
;;;;
;;;; Bead green-screen-wtd: the Director -- the post-processing "value-add" that
;;;; turns a raw capture (cursor track + input events) into a polished
;;;; motion plan: eased cursor path + auto-zoom keyframe timeline. Pure Lisp, no
;;;; GL/portal, so it is fully testable offline with synthetic sessions (the same
;;;; discipline the compositor used with synthetic textures). Its output is a
;;;; list of takesy/keyframe structs, which the compositor already renders.
;;;;
;;;; D1 (wtd.1): the input model + a deterministic synthetic-session generator.
;;;; Coordinates: cursor/click positions are in SCREEN PIXELS; the Director
;;;; normalizes to 0..1 UV (px->uv) when it emits keyframe centers.

(defpackage #:takesy/director
  (:use #:cl)
  (:local-nicknames (#:kf #:takesy/keyframe))
  (:export #:cursor-sample #:make-cursor-sample #:cursor-sample-p
           #:cursor-sample-time #:cursor-sample-x #:cursor-sample-y
           #:input-event #:make-input-event #:input-event-p
           #:input-event-time #:input-event-kind #:input-event-x #:input-event-y
           #:session #:make-session #:session-p
           #:session-width #:session-height #:session-cursor #:session-events
           #:session-duration
           #:px->uv #:cursor-at #:make-synthetic-session #:validate-session
           #:*cursor-omega* #:spring-step #:ease-cursor
           #:activity-segment #:make-activity-segment #:activity-segment-p
           #:activity-segment-t-start #:activity-segment-t-end
           #:activity-segment-focus-x #:activity-segment-focus-y
           #:activity-segment-n-clicks #:activity-segment-n-keys
           #:*activity-gap* #:detect-activity
           #:*dwell-speed* #:*dwell-min* #:detect-dwell-activity
           #:*zoom-level* #:*zoom-lead* #:*zoom-tail*
           #:schedule-zooms #:plan-timeline))

(in-package #:takesy/director)

;;; ------------------------------------------------------------------
;;; Input model.

(defstruct cursor-sample
  (time 0.0)     ; seconds
  (x 0.0) (y 0.0)) ; screen pixels

(defstruct input-event
  (time 0.0)     ; seconds
  (kind :click)  ; :click | :key
  (x nil) (y nil)) ; screen px for :click; nil for :key

(defstruct session
  (width 1920) (height 1200)
  (cursor '())   ; list of cursor-sample, ascending time
  (events '()))  ; list of input-event, ascending time

(defun session-duration (s)
  "Wall-clock span of the session's cursor track, in seconds."
  (let ((c (session-cursor s)))
    (if c (cursor-sample-time (car (last c))) 0.0)))

;;; ------------------------------------------------------------------
;;; Helpers.

(defun px->uv (x y width height)
  "Normalize a screen-pixel point to 0..1 UV, clamped to the frame."
  (flet ((clamp01 (v) (max 0.0 (min 1.0 (float v 1.0)))))
    (values (clamp01 (/ x width)) (clamp01 (/ y height)))))

(defun cursor-at (session time)
  "Piecewise-linear cursor position at TIME. Return (values x y) in screen px;
clamps to the track endpoints outside its span."
  (let ((c (session-cursor session)))
    (cond
      ((null c) (values 0.0 0.0))
      ((<= time (cursor-sample-time (first c)))
       (values (cursor-sample-x (first c)) (cursor-sample-y (first c))))
      ((>= time (cursor-sample-time (car (last c))))
       (let ((l (car (last c)))) (values (cursor-sample-x l) (cursor-sample-y l))))
      (t (loop for (a b) on c
               when (and b (<= (cursor-sample-time a) time)
                         (< time (cursor-sample-time b)))
                 do (let* ((span (- (cursor-sample-time b) (cursor-sample-time a)))
                           (u (if (zerop span) 0.0
                                  (/ (- time (cursor-sample-time a)) span))))
                      (return (values (+ (cursor-sample-x a)
                                         (* (- (cursor-sample-x b) (cursor-sample-x a)) u))
                                      (+ (cursor-sample-y a)
                                         (* (- (cursor-sample-y b) (cursor-sample-y a)) u))))))))))

;;; ------------------------------------------------------------------
;;; Cursor easing: a critically-damped spring (damping ratio = 1). The smoothed
;;; cursor chases the raw one with a natural, overshoot-free lag -- the calm
;;; pointer motion polished is known for. OMEGA is the angular frequency
;;; (rad/s): higher = snappier, lower = floatier. Tune it live at the REPL via
;;; *cursor-omega*; settling time is ~4/OMEGA seconds.

(defparameter *cursor-omega* 12.0
  "Default cursor-spring angular frequency (rad/s). REPL-tunable.")

(defun spring-step (p v x omega dt)
  "Advance a critically-damped spring one step: position P, velocity V chasing
target X over DT at angular frequency OMEGA. Exact for a target held over DT
(so it is stable at any DT). Return (values new-p new-v)."
  (if (<= dt 0.0)
      (values p v)
      (let* ((d0 (- p x))
             (b  (+ v (* omega d0)))
             (e  (exp (- (* omega dt))))
             (dt* (+ d0 (* b dt))))
        (values (+ x (* dt* e))
                (* (- b (* omega dt*)) e)))))

(defun ease-cursor (session &key (omega *cursor-omega*))
  "Return a smoothed copy of SESSION's cursor track: each raw sample eased by a
critically-damped spring (independent x/y). Times are preserved; the smoothed
path lags the raw one and never overshoots a step."
  (let ((track (session-cursor session)))
    (when (null track) (return-from ease-cursor '()))
    (let* ((first (car track))
           (px (cursor-sample-x first)) (py (cursor-sample-y first))
           (vx 0.0) (vy 0.0)
           (prev-t (cursor-sample-time first))
           (out (list (make-cursor-sample :time prev-t :x px :y py))))
      (dolist (cs (cdr track) (nreverse out))
        (let ((dt (- (cursor-sample-time cs) prev-t)))
          (multiple-value-setq (px vx)
            (spring-step px vx (cursor-sample-x cs) omega dt))
          (multiple-value-setq (py vy)
            (spring-step py vy (cursor-sample-y cs) omega dt))
          (setf prev-t (cursor-sample-time cs))
          (push (make-cursor-sample :time prev-t :x px :y py) out))))))

;;; ------------------------------------------------------------------
;;; Activity detection. Cluster input events into bursts by time gap; each burst
;;; becomes an activity segment the scheduler (D4) turns into a zoom. A segment's
;;; spatial focus is the centroid of its clicks (where the user is working), or,
;;; for key-only bursts, the cursor position at the burst's midpoint.

(defstruct activity-segment
  (t-start 0.0) (t-end 0.0)
  (focus-x 0.0) (focus-y 0.0)   ; screen px
  (n-clicks 0) (n-keys 0))

(defparameter *activity-gap* 0.8
  "Max seconds between consecutive events in one activity burst. REPL-tunable.")

(defun %segment-from (session events)
  "Build an activity-segment from a non-empty time-ordered EVENTS burst."
  (let* ((ts     (input-event-time (first events)))
         (te     (input-event-time (car (last events))))
         (clicks (remove-if-not (lambda (e) (eq (input-event-kind e) :click)) events))
         (nclk   (length clicks))
         (nkey   (- (length events) nclk)))
    (multiple-value-bind (fx fy)
        (if (plusp nclk)
            (values (/ (reduce #'+ clicks :key #'input-event-x) nclk)
                    (/ (reduce #'+ clicks :key #'input-event-y) nclk))
            (cursor-at session (* 0.5 (+ ts te))))
      (make-activity-segment :t-start ts :t-end te
                             :focus-x (float fx 1.0) :focus-y (float fy 1.0)
                             :n-clicks nclk :n-keys nkey))))

(defun detect-activity (session &key (gap *activity-gap*))
  "Cluster SESSION's events into activity segments: a new segment starts whenever
the gap to the previous event exceeds GAP. Return them in time order."
  (let ((evs (sort (copy-list (session-events session)) #'<
                   :key #'input-event-time)))
    (when (null evs) (return-from detect-activity '()))
    (let ((segments '()) (group '()) (last-t nil))
      (flet ((flush ()
               (when group
                 (push (%segment-from session (nreverse group)) segments)
                 (setf group '()))))
        (dolist (ev evs)
          (when (and last-t (> (- (input-event-time ev) last-t) gap))
            (flush))
          (push ev group)
          (setf last-t (input-event-time ev)))
        (flush))
      (nreverse segments))))

;;; ------------------------------------------------------------------
;;; Cursor-dwell activity (no-events fallback). When we have no click/key stream
;;; (e.g. no evdev access), infer activity from the cursor itself: stretches
;;; where it moves slowly (dwelling on something) become activity segments
;;; focused on where it lingered. Same activity-segment output, so the scheduler
;;; is unchanged. Reading real evdev clicks/keys is bead green-screen-am4.2's
;;; other half; this fallback keeps auto-zoom working without elevated perms.

(defparameter *dwell-speed* 250.0
  "Cursor speed (px/s) at or below which the pointer counts as dwelling.")
(defparameter *dwell-min* 0.4
  "Minimum dwell duration (s) to emit an activity segment. REPL-tunable.")

(defun detect-dwell-activity (session &key (speed *dwell-speed*) (min-dwell *dwell-min*))
  "Infer activity segments from slow (dwelling) stretches of the cursor track.
Each qualifying dwell yields a segment focused on the mean dwell position."
  (let ((track (session-cursor session)))
    (when (< (length track) 2) (return-from detect-dwell-activity '()))
    (let ((segments '()) (run '()) (run-start nil))
      (flet ((flush (end-t)
               (when (and run run-start (>= (- end-t run-start) min-dwell))
                 (let* ((n  (length run))
                        (cx (/ (reduce #'+ run :key #'cursor-sample-x) n))
                        (cy (/ (reduce #'+ run :key #'cursor-sample-y) n)))
                   (push (make-activity-segment
                          :t-start run-start :t-end end-t
                          :focus-x (float cx 1.0) :focus-y (float cy 1.0)
                          :n-clicks 0 :n-keys 0)
                         segments)))
               (setf run '() run-start nil)))
        (loop for (a b) on track while b
              for dt = (- (cursor-sample-time b) (cursor-sample-time a))
              for dist = (sqrt (+ (expt (- (cursor-sample-x b) (cursor-sample-x a)) 2)
                                  (expt (- (cursor-sample-y b) (cursor-sample-y a)) 2)))
              for spd = (if (> dt 0.0) (/ dist dt) 0.0)
              do (if (<= spd speed)
                     (progn
                       (unless run-start (setf run-start (cursor-sample-time a)))
                       (push a run))
                     (flush (cursor-sample-time a))))
        (flush (cursor-sample-time (car (last track)))))
      (nreverse segments))))

;;; ------------------------------------------------------------------
;;; Zoom scheduling. Each activity segment becomes a punch-in: ease from wide to
;;; a zoom centred on the segment focus shortly BEFORE it starts (LEAD), hold
;;; through it, then ease back to wide after an idle TAIL. Frame styling
;;; (padding/corner/shadow/bg) is held constant -- only zoom + pan animate, the
;;; polished convention. Output is a takesy/keyframe timeline, sorted and
;;; bracketed by wide frames at t=0 and the session end.

(defparameter *zoom-level* 2.0 "Punch-in zoom factor for activity. REPL-tunable.")
(defparameter *zoom-lead*  0.5 "Seconds to begin zooming in before a segment.")
(defparameter *zoom-tail*  0.6 "Seconds to stay zoomed after a segment before easing out.")

(defun %clean-timeline (kfs)
  "Sort keyframes by time; where two collide in time (e.g. a clamped lead-in
meets the opening frame), keep the more-zoomed one so activity wins."
  (let ((sorted (stable-sort (copy-list kfs) #'< :key #'kf:keyframe-time))
        (out '()))
    (dolist (k sorted (nreverse out))
      (if (and out (< (abs (- (kf:keyframe-time k) (kf:keyframe-time (car out)))) 1e-4))
          (when (> (kf:keyframe-zoom k) (kf:keyframe-zoom (car out)))
            (setf (car out) k))
          (push k out)))))

(defun schedule-zooms (session segments
                       &key (zoom *zoom-level*) (lead *zoom-lead*) (tail *zoom-tail*)
                            (padding 0.06) (corner 0.10)
                            (shadow-blur 0.05) (shadow-alpha 0.5)
                            (bg '(0.11 0.12 0.15)))
  "Turn activity SEGMENTS into a keyframe timeline over SESSION's duration."
  (let* ((w (session-width session)) (h (session-height session))
         (dur (session-duration session))
         (kfs '()))
    (flet ((frame (time zoom cx cy)
             (kf:make-keyframe :time (max 0.0 (min dur time))
                               :zoom zoom :center-x cx :center-y cy
                               :padding padding :corner-radius corner
                               :shadow-blur shadow-blur :shadow-alpha shadow-alpha
                               :bg-color bg)))
      (push (frame 0.0 1.0 0.5 0.5) kfs)                 ; open wide
      (dolist (seg segments)
        (multiple-value-bind (fx fy)
            (px->uv (activity-segment-focus-x seg) (activity-segment-focus-y seg) w h)
          (push (frame (- (activity-segment-t-start seg) lead) 1.0 0.5 0.5) kfs)
          (push (frame (activity-segment-t-start seg) zoom fx fy) kfs)  ; zoomed in
          (push (frame (activity-segment-t-end seg)   zoom fx fy) kfs)  ; hold
          (push (frame (+ (activity-segment-t-end seg) tail) 1.0 0.5 0.5) kfs))) ; ease out
      (push (frame dur 1.0 0.5 0.5) kfs)                 ; close wide
      (%clean-timeline (nreverse kfs)))))

(defun plan-timeline (session &key (activity :auto) (gap *activity-gap*)
                                   (zoom *zoom-level*)
                                   (lead *zoom-lead*) (tail *zoom-tail*)
                                   (padding 0.06) (corner 0.10)
                                   (shadow-blur 0.05) (shadow-alpha 0.5)
                                   (bg '(0.11 0.12 0.15)))
  "Full Director pass: SESSION -> activity segments -> zoom keyframe timeline
directly renderable by the compositor's render-timeline. ACTIVITY selects the
source: :events (clicks/keys), :dwell (cursor lingering), or :auto (events if
the session has any, else dwell)."
  (schedule-zooms session
                  (ecase activity
                    (:events (detect-activity session :gap gap))
                    (:dwell  (detect-dwell-activity session))
                    (:auto   (if (session-events session)
                                 (detect-activity session :gap gap)
                                 (detect-dwell-activity session))))
                  :zoom zoom :lead lead :tail tail
                  :padding padding :corner corner
                  :shadow-blur shadow-blur :shadow-alpha shadow-alpha :bg bg))

;;; ------------------------------------------------------------------
;;; Synthetic fixtures. A deterministic session (no RNG, so tests are stable):
;;; the pointer idles, darts to a hotspot where the user clicks a few times,
;;; then moves away. Waypoints are piecewise-linearly sampled at FPS.

(defparameter +demo-waypoints+
  ;; (time x y)
  '((0.0  200.0  150.0)
    (1.0  200.0  150.0)     ; idle top-left
    (1.4 1500.0  900.0)     ; dart to the hotspot
    (3.0 1520.0  905.0)     ; dwell (tiny drift) while clicking
    (3.5  300.0 1000.0)     ; leave
    (4.0  300.0 1000.0)))

(defparameter +demo-events+
  ;; clicks at the hotspot, plus a couple of keystrokes
  '((1.6 :click 1500.0 900.0)
    (1.9 :click 1512.0 902.0)
    (2.2 :click 1508.0 907.0)
    (2.5 :key   nil    nil)
    (2.8 :key   nil    nil)))

(defun %sample-waypoints (waypoints time)
  (loop for (a b) on waypoints
        for (ta xa ya) = a
        do (when (and b (<= ta time) (< time (first b)))
             (destructuring-bind (tb xb yb) b
               (let ((u (/ (- time ta) (- tb ta))))
                 (return-from %sample-waypoints
                   (values (+ xa (* (- xb xa) u)) (+ ya (* (- yb ya) u)))))))
        finally (destructuring-bind (tl xl yl) (car (last waypoints))
                  (declare (ignore tl))
                  (return (values xl yl)))))

(defun make-synthetic-session (&key (width 1920) (height 1200) (fps 60)
                                    (waypoints +demo-waypoints+)
                                    (events +demo-events+))
  "Build a deterministic SESSION for offline Director tests: cursor track sampled
at FPS from WAYPOINTS, plus discrete input EVENTS."
  (let* ((t-end (first (car (last waypoints))))
         (n     (1+ (round (* fps t-end))))
         (track (loop for i below n
                      for tsec = (/ i (float fps 1.0))
                      collect (multiple-value-bind (x y)
                                  (%sample-waypoints waypoints tsec)
                                (make-cursor-sample :time tsec :x x :y y)))))
    (make-session
     :width width :height height
     :cursor track
     :events (mapcar (lambda (e)
                       (destructuring-bind (tm kind x y) e
                         (make-input-event :time tm :kind kind :x x :y y)))
                     events))))

(defun validate-session (s)
  "Sanity-check a session: cursor times strictly ascending and in-frame, events
ascending and (for clicks) in-frame. Signal an error on any violation; else T."
  (let ((w (session-width s)) (h (session-height s)) (prev nil))
    (dolist (cs (session-cursor s))
      (let ((tm (cursor-sample-time cs)))
        (when (and prev (<= tm prev))
          (error "cursor time not ascending: ~A after ~A" tm prev))
        (setf prev tm))
      (unless (and (<= 0 (cursor-sample-x cs) w) (<= 0 (cursor-sample-y cs) h))
        (error "cursor sample out of frame: (~A,~A)"
               (cursor-sample-x cs) (cursor-sample-y cs))))
    (let ((pe nil))
      (dolist (ev (session-events s))
        (let ((tm (input-event-time ev)))
          (when (and pe (< tm pe)) (error "events not ascending: ~A after ~A" tm pe))
          (setf pe tm))
        (when (eq (input-event-kind ev) :click)
          (unless (and (<= 0 (input-event-x ev) w) (<= 0 (input-event-y ev) h))
            (error "click out of frame: (~A,~A)"
                   (input-event-x ev) (input-event-y ev))))))
    t))
