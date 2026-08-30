;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;; director.lisp
;;;;
;;;; Bead green-screen-wtd: the Director -- the post-processing "value-add" that
;;;; turns a raw capture (cursor track + input events) into a polished motion
;;;; plan: eased cursor path + auto-zoom keyframe timeline. Pure Lisp, no
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
           #:session-damage #:session-edges-x #:session-edges-y
           #:session-duration #:*zoom-fit-margin* #:*track-snap*
           #:px->uv #:cursor-at #:make-synthetic-session #:validate-session
           #:*cursor-omega-slow* #:*cursor-omega-fast* #:*cursor-speed-ref*
           #:*cursor-anticipate* #:spring-step #:ease-cursor
           #:activity-segment #:make-activity-segment #:activity-segment-p
           #:activity-segment-t-start #:activity-segment-t-end
           #:activity-segment-focus-x #:activity-segment-focus-y
           #:activity-segment-n-clicks #:activity-segment-n-keys
           #:*activity-gap* #:detect-activity
           #:*dwell-speed* #:*dwell-min* #:detect-dwell-activity
           #:*zoom-level* #:*zoom-min* #:*zoom-lead* #:*zoom-tail* #:*zoom-merge-gap*
           #:*track* #:*track-omega-pan* #:*track-omega-zoom* #:*track-anticipate*
           #:*track-text-follow*
           #:plan-tracked-timeline
           #:*damage-include-radius* #:merge-segments #:schedule-zooms #:plan-timeline))

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
  (events '())   ; list of input-event, ascending time
  (damage '())   ; list of (time x0 y0 x1 y1), changed-region UV bbox per frame
  (edges-x '())  ; sorted UV x of strong vertical content edges (panel boundaries)
  (edges-y '())) ; sorted UV y of strong horizontal content edges

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
;;; Cursor easing: a critically-damped spring (damping ratio = 1) with two smarts
;;; on top, matching how people actually move a pointer:
;;;
;;;   * SPEED-ADAPTIVE stiffness -- fast, deliberate motion (pointing at things)
;;;     gets a stiffer spring so it tracks closely (little lag); slow motion
;;;     (settling on a target) gets a softer spring for a calm finish.
;;;   * ANTICIPATION -- as the cursor nears a rest/click point (a dwell target),
;;;     steer the spring's target toward that point, so it arrives straight
;;;     instead of tracing the user's overshoot-and-correct wobble.
;;;
;;; All REPL-tunable.

(defparameter *cursor-omega-slow* 9.0
  "Spring stiffness (rad/s) when the pointer is slow/settling -- smoother.")
(defparameter *cursor-omega-fast* 30.0
  "Spring stiffness (rad/s) when the pointer moves fast -- less lag for pointing.")
(defparameter *cursor-speed-ref* 1800.0
  "Pointer speed (px/s) at which stiffness reaches *cursor-omega-fast*.")
(defparameter *cursor-anticipate* 0.4
  "Seconds before a rest/click target to start aiming the cursor straight at it.")

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

(defun ease-cursor (session &key (omega-slow *cursor-omega-slow*)
                                 (omega-fast *cursor-omega-fast*)
                                 (speed-ref *cursor-speed-ref*)
                                 (anticipate *cursor-anticipate*))
  "Return a smoothed copy of SESSION's cursor track. Each step uses a
critically-damped spring whose stiffness scales with pointer speed (responsive
when pointing, calm when settling); near a dwell/rest target the spring aims at
that target to cut overshoot. Times are preserved."
  (let ((track (session-cursor session)))
    (when (null track) (return-from ease-cursor '()))
    (let* ((rests (mapcar (lambda (s) (list (activity-segment-t-start s)
                                            (activity-segment-focus-x s)
                                            (activity-segment-focus-y s)))
                          (detect-dwell-activity session)))
           (first (car track))
           (px (cursor-sample-x first)) (py (cursor-sample-y first))
           (vx 0.0) (vy 0.0)
           (prev-t (cursor-sample-time first))
           (prev-x px) (prev-y py)
           (out (list (make-cursor-sample :time prev-t :x px :y py))))
      (dolist (cs (cdr track) (nreverse out))
        (let* ((tnow (cursor-sample-time cs))
               (dt   (- tnow prev-t))
               (rx   (cursor-sample-x cs)) (ry (cursor-sample-y cs)))
          (when (> dt 0.0)
            (let* ((speed (/ (sqrt (+ (expt (- rx prev-x) 2) (expt (- ry prev-y) 2))) dt))
                   (frac  (max 0.0 (min 1.0 (/ speed speed-ref))))
                   (omega (+ omega-slow (* (- omega-fast omega-slow) frac)))
                   (tx rx) (ty ry)
                   ;; the next rest/click target within the anticipation window
                   (next (find-if (lambda (r) (<= 0.0 (- (first r) tnow) anticipate)) rests)))
              (when next
                (destructuring-bind (rt fx fy) next
                  (let* ((u (- 1.0 (/ (- rt tnow) anticipate)))  ; 0 far -> 1 at target
                         (w (* u u)))                            ; ease-in the aim
                    (setf tx (+ (* (- 1.0 w) rx) (* w fx))
                          ty (+ (* (- 1.0 w) ry) (* w fy))))))
              (multiple-value-setq (px vx) (spring-step px vx tx omega dt))
              (multiple-value-setq (py vy) (spring-step py vy ty omega dt))))
          (setf prev-t tnow prev-x rx prev-y ry)
          (push (make-cursor-sample :time tnow :x px :y py) out))))))

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
(defparameter *dwell-min* 0.6
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
;;; (padding/corner/shadow/bg) is held constant -- only zoom + pan animate.
;;; Output is a takesy/keyframe timeline, sorted and
;;; bracketed by wide frames at t=0 and the session end.

(defparameter *zoom-level* 1.8 "Punch-in zoom factor for activity. REPL-tunable.")
(defparameter *zoom-min* nil
  "When set, a minimum zoom to force on detected activity even if it's too spread
out for the fit to punch in (e.g. a full-screen app), centred on the cursor's
working spot. NIL = only the smart damage-fit zoom. REPL-tunable.")
(defparameter *zoom-lead*  0.5 "Seconds to begin zooming in before a segment.")
(defparameter *zoom-tail*  0.8 "Seconds to stay zoomed after a segment before easing out.")
(defparameter *zoom-merge-gap* 2.5
  "Idle gap (s) below which adjacent activity stays zoomed and PANS between spots
instead of zooming out and back in -- avoids constant in/out. REPL-tunable.")
(defparameter *zoom-fit-margin* 0.18
  "Fractional margin kept around the activity bounding box when fitting zoom, so
screen changes near the edge of the active region aren't cropped. REPL-tunable.")
(defparameter *damage-include-radius* 0.33
  "Only screen changes whose centre is within this UV distance of where the cursor
is working count toward the zoom fit; distant incidental updates are ignored, so
one wide change elsewhere doesn't defeat the zoom entirely. REPL-tunable.")
(defparameter *damage-max-rect* 0.6
  "Screen changes wider or taller than this fraction of the frame are treated as
scrolling / global updates (not a localized thing to frame) and excluded from the
zoom-region estimate. REPL-tunable.")

(defun %activity-bbox (session t0 t1 &key (near *damage-include-radius*)
                                          (max-rect *damage-max-rect*))
  "UV bounding box of the working region in [T0,T1]: the cursor path, grown to
include the localized screen changes (damage) NEAR it -- skipping changes larger
than MAX-RECT (scroll/global). Return (values x0 y0 x1 y1) in 0..1, or NIL."
  (let ((w (session-width session)) (h (session-height session))
        (x0 nil) (y0 nil) (x1 nil) (y1 nil)
        (cxsum 0.0) (cysum 0.0) (cn 0))
    (flet ((acc (ux uy)
             (setf x0 (if x0 (min x0 ux) ux) y0 (if y0 (min y0 uy) uy)
                   x1 (if x1 (max x1 ux) ux) y1 (if y1 (max y1 uy) uy))))
      ;; cursor path first -- it anchors *where* the user is working
      (dolist (cs (session-cursor session))
        (when (<= t0 (cursor-sample-time cs) t1)
          (let ((ux (/ (cursor-sample-x cs) w)) (uy (/ (cursor-sample-y cs) h)))
            (acc ux uy) (incf cxsum ux) (incf cysum uy) (incf cn))))
      (let ((ccx (if (plusp cn) (/ cxsum cn) 0.5))
            (ccy (if (plusp cn) (/ cysum cn) 0.5)))
        (dolist (d (session-damage session))
          (destructuring-bind (dt dx0 dy0 dx1 dy1) d
            (when (and (<= t0 dt t1)
                       (<= (- dx1 dx0) max-rect) (<= (- dy1 dy0) max-rect))  ; skip scroll/global
              (let ((rcx (* 0.5 (+ dx0 dx1))) (rcy (* 0.5 (+ dy0 dy1))))
                (when (or (zerop cn)
                          (<= (sqrt (+ (expt (- rcx ccx) 2) (expt (- rcy ccy) 2))) near))
                  (acc dx0 dy0) (acc dx1 dy1))))))))
    (when x0 (values (max 0.0 x0) (max 0.0 y0) (min 1.0 x1) (min 1.0 y1)))))

(defun %damage-centroid (session t0 t1 &key (max-rect *damage-max-rect*))
  "UV centroid of the localized screen changes (damage) in [T0,T1], excluding
wide/global (scroll) rects. This is *where the screen is changing* -- the point a
screencast should frame, which for typing/editing is the text, not the mouse.
Return (values cx cy) or NIL when there is no localized damage."
  (let ((sx 0.0) (sy 0.0) (n 0))
    (dolist (d (session-damage session))
      (destructuring-bind (dt x0 y0 x1 y1) d
        (when (and (<= t0 dt t1)
                   (<= (- x1 x0) max-rect) (<= (- y1 y0) max-rect))
          (incf sx (* 0.5 (+ x0 x1))) (incf sy (* 0.5 (+ y0 y1))) (incf n))))
    (when (plusp n) (values (/ sx n) (/ sy n)))))

(defun %fit-zoom (bw bh max-zoom margin)
  "Largest zoom (<= MAX-ZOOM, >= 1) whose 1/zoom window still contains a BW x BH
UV box with MARGIN to spare."
  (let ((span (* (max bw bh) (+ 1.0 margin))))
    (if (<= span 0.0) max-zoom
        (max 1.0 (min max-zoom (/ 1.0 span))))))

(defun merge-segments (segments gap)
  "Group time-ordered SEGMENTS so any two separated by <= GAP seconds share a
group. Each group becomes one sustained zoom that pans across its segments."
  (when segments
    (let ((groups '()) (cur (list (first segments))))
      (dolist (s (rest segments))
        (if (<= (- (activity-segment-t-start s)
                   (activity-segment-t-end (first cur)))
                gap)
            (push s cur)
            (progn (push (nreverse cur) groups) (setf cur (list s)))))
      (push (nreverse cur) groups)
      (nreverse groups))))

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
                            (zoom-min *zoom-min*)
                            (padding 0.04) (corner 0.09)
                            (shadow-blur 0.03) (shadow-alpha 0.5)
                            (bg '(0.11 0.12 0.15)))
  "Turn activity SEGMENTS into a keyframe timeline over SESSION's duration.
Segments closer than *zoom-merge-gap* are merged into one sustained zoom that
pans between them, so brief pauses don't cause the view to zoom out and back in."
  (let* ((dur (session-duration session))
         (groups (merge-segments segments *zoom-merge-gap*))
         (kfs '()))
    (flet ((frame (time zoom cx cy)
             (kf:make-keyframe :time (max 0.0 (min dur time))
                               :zoom zoom :center-x cx :center-y cy
                               :padding padding :corner-radius corner
                               :shadow-blur shadow-blur :shadow-alpha shadow-alpha
                               :bg-color bg)))
      (push (frame 0.0 1.0 0.5 0.5) kfs)                 ; open wide
      (dolist (g groups)
        (let ((g-start (activity-segment-t-start (first g)))
              (g-end   (activity-segment-t-end (car (last g)))))
          ;; Fit the zoom to ALL activity (cursor + screen changes) across the
          ;; whole group, so nothing that moves in that window gets cropped.
          (multiple-value-bind (x0 y0 x1 y1) (%activity-bbox session g-start g-end)
            (let* ((bcx (if x0 (* 0.5 (+ x0 x1)) 0.5))
                   (bcy (if y0 (* 0.5 (+ y0 y1)) 0.5))
                   (zfit (if x0 (%fit-zoom (- x1 x0) (- y1 y0) zoom *zoom-fit-margin*)
                             zoom))
                   ;; ZOOM-MIN forces a punch-in even when activity is too spread
                   ;; out for the fit to zoom (e.g. a full-screen app).
                   (force (and zoom-min (> zoom-min zfit)))
                   (z   (if force zoom-min zfit)))
              ;; Centre on WHERE THE SCREEN IS CHANGING (localized damage) -- for
              ;; typing/editing the mouse is idle elsewhere, so the cursor is the
              ;; wrong focus. Fall back to the activity bbox centre when there's no
              ;; localized damage (e.g. pure mouse movement).
              (multiple-value-bind (dcx dcy) (%damage-centroid session g-start g-end)
                (let ((cx (or dcx bcx)) (cy (or dcy bcy)))
              (when (> z 1.02)              ; skip groups too spread out to zoom
                ;; Never begin the zoom before LEAD, so the clip always opens on
                ;; the whole region and eases in (activity that starts at t=0 must
                ;; still show a wide first frame).
                (let* ((zstart (max g-start lead))
                       (zend   (max g-end zstart)))
                  (push (frame (- zstart lead) 1.0 0.5 0.5) kfs)  ; ease in (wide)
                  (push (frame zstart z cx cy) kfs)               ; hold, fit
                  (push (frame zend   z cx cy) kfs)
                  (push (frame (+ zend tail) 1.0 0.5 0.5) kfs))))))))) ; ease out
      (push (frame dur 1.0 0.5 0.5) kfs)                 ; close wide
      (%clean-timeline (nreverse kfs)))))

;;; ------------------------------------------------------------------
;;; Tracked camera (green-screen-1em/muw/rac/bx2). Instead of one static zoom per
;;; activity group, drive a continuous camera: each moment targets where the
;;; screen is changing (with a forward look-ahead for anticipation), and a
;;; critically-damped spring glides the centre and zoom after it -- a smooth,
;;; cameraperson-style pan/zoom. Emitted as a dense keyframe path.

(defparameter *track* t
  "When true, plan-timeline uses the continuous tracked camera (smooth pan/zoom
following the changing region) instead of static per-group punch-ins.")
(defparameter *track-omega-pan* 6.0
  "Camera pan-spring stiffness (rad/s). Lower = slower, more cinematic glide.")
(defparameter *track-omega-zoom* 4.0
  "Camera zoom-spring stiffness (rad/s).")
(defparameter *track-anticipate* 0.35
  "Seconds of look-ahead: target where activity is heading so the frame leads it.")
(defparameter *track-window* 0.5
  "Half-width (s) of the activity window sampled around each moment.")
(defparameter *track-center-deadband* 0.012
  "UV deadband: target centre moves under this are ignored (anti-jitter).")
(defparameter *track-text-follow* 2.0
  "Recency bias when centring on damage: newer changes get up to (1+this)x weight,
so the camera follows the leading edge of new content (the caret when typing).
0 = plain centroid.")

(defun %window-activity (session t0 t1 &key (max-rect *damage-max-rect*))
  "Localized activity in [T0,T1]: return (values cx cy spread present-p) where
CX,CY is the centre (damage centroid, grown to include the cursor) and SPREAD is
the max UV extent of the activity bbox. PRESENT-P is NIL when nothing localized
is happening (idle -> the camera should ease back to wide)."
  (let ((w (session-width session)) (h (session-height session))
        (sx 0.0) (sy 0.0) (sw 0.0) (n 0)          ; sw = sum of recency weights
        (span (max 1e-3 (- t1 t0)))
        (x0 nil) (y0 nil) (x1 nil) (y1 nil))
    (flet ((acc (ax ay) (setf x0 (if x0 (min x0 ax) ax) y0 (if y0 (min y0 ay) ay)
                              x1 (if x1 (max x1 ax) ax) y1 (if y1 (max y1 ay) ay))))
      (dolist (d (session-damage session))
        (destructuring-bind (dt dx0 dy0 dx1 dy1) d
          (when (and (<= t0 dt t1) (<= (- dx1 dx0) max-rect) (<= (- dy1 dy0) max-rect))
            ;; recency weight: newer damage counts more (text-follow / leading edge)
            (let ((wt (+ 1.0 (* *track-text-follow* (/ (- dt t0) span)))))
              (incf sx (* wt 0.5 (+ dx0 dx1))) (incf sy (* wt 0.5 (+ dy0 dy1)))
              (incf sw wt))
            (incf n)
            (acc dx0 dy0) (acc dx1 dy1))))
      (if (plusp n)
          (let ((ccx (/ sx sw)) (ccy (/ sy sw)))
            ;; keep the pointer in frame: grow the bbox toward the cursor a little
            (multiple-value-bind (curx cury) (cursor-at session (* 0.5 (+ t0 t1)))
              (when curx (acc (/ curx w) (/ cury h))))
            (values ccx ccy (max (- x1 x0) (- y1 y0)) t))
          ;; no localized change -> not "present"; report the cursor for continuity
          (multiple-value-bind (curx cury) (cursor-at session (* 0.5 (+ t0 t1)))
            (if curx (values (/ curx w) (/ cury h) 0.0 nil)
                (values 0.5 0.5 0.0 nil)))))))

(defparameter *track-snap* nil
  "When true (and content edges are known), gently align the tracked frame to the
enclosing window/UI-panel boundaries.")
(defparameter *track-snap-tol* 0.05
  "Max UV nudge allowed when snapping a frame edge to a content edge.")

(defun %nearest (x xs)
  "Nearest value in list XS to X, or NIL if XS is empty."
  (when xs (let ((best (first xs)))
             (dolist (v (rest xs) best) (when (< (abs (- v x)) (abs (- best x))) (setf best v))))))

(defun %snap-1d (c hw edges tol)
  "Nudge centre C (window half-width HW) so the window's nearer edge aligns to the
closest content EDGE within TOL. The nudge is small, so the spring absorbs it."
  (if (null edges) c
      (let* ((el (%nearest (- c hw) edges)) (er (%nearest (+ c hw) edges))
             (dl (if el (- el (- c hw)) 1e9)) (dr (if er (- er (+ c hw)) 1e9)))
        (cond ((and (< (abs dl) tol) (<= (abs dl) (abs dr))) (+ c dl))
              ((< (abs dr) tol) (+ c dr))
              (t c)))))

(defun plan-tracked-timeline (session
                              &key (zoom *zoom-level*) (zoom-min *zoom-min*)
                                   (omega-pan *track-omega-pan*)
                                   (omega-zoom *track-omega-zoom*)
                                   (anticipate *track-anticipate*)
                                   (window *track-window*)
                                   (snap *track-snap*)
                                   (padding 0.04) (corner 0.09)
                                   (shadow-blur 0.03) (shadow-alpha 0.5)
                                   (bg '(0.11 0.12 0.15)))
  "Continuous tracked-camera timeline: a spring-damped centre + zoom that follow
the changing region over SESSION's duration. Covers pan tracking, adaptive
'breathing' zoom, and anticipatory lead-in. Returns a dense keyframe timeline."
  (let* ((dur (session-duration session))
         (dt (/ 1.0 60.0))                ; spring integration step
         (emit-every 3)                   ; keyframe density (~20/s)
         ;; camera state: centre (cx,cy) + velocities, zoom (z) + velocity.
         (cx 0.5) (cy 0.5) (vx 0.0) (vy 0.0)
         (z 1.0) (vz 0.0)
         (tcx 0.5) (tcy 0.5)              ; last committed target centre (deadband)
         (kfs '()) (i 0))
    (flet ((frame (time zoom ccx ccy)
             (kf:make-keyframe :time (max 0.0 (min dur (float time 1.0)))
                               :zoom (float zoom 1.0)
                               :center-x (float ccx 1.0) :center-y (float ccy 1.0)
                               :padding padding :corner-radius corner
                               :shadow-blur shadow-blur :shadow-alpha shadow-alpha
                               :bg-color bg)))
      (loop for tm = 0.0 then (+ tm (float dt 1.0))
            while (<= tm (+ dur (float dt 1.0)))
            do (multiple-value-bind (ax ay spread present)
                   ;; asymmetric window: look a little back, further forward
                   ;; (anticipation) so the camera leads the action.
                   (%window-activity session (max 0.0 (- tm window))
                                     (min dur (+ tm anticipate window)))
                 ;; target centre (deadband against micro-jitter) + target zoom.
                 (when (> (+ (abs (- ax tcx)) (abs (- ay tcy))) *track-center-deadband*)
                   (setf tcx ax tcy ay))
                 (let* ((zfit (if present
                                  (%fit-zoom spread spread zoom *zoom-fit-margin*)
                                  1.0))
                        (tz (cond ((not present) 1.0)               ; idle -> wide
                                  ((and zoom-min (> zoom-min zfit)) zoom-min)
                                  (t zfit))))
                   ;; Content-aware framing: nudge the target so the frame aligns
                   ;; to window/panel edges (pre-spring, so it stays smooth).
                   (when (and snap present (> tz 1.02))
                     (let ((hw (/ 0.5 tz)))
                       (setf tcx (%snap-1d tcx hw (session-edges-x session) *track-snap-tol*)
                             tcy (%snap-1d tcy hw (session-edges-y session) *track-snap-tol*))))
                   ;; advance the springs one step toward the targets
                   (multiple-value-setq (cx vx) (spring-step cx vx tcx omega-pan dt))
                   (multiple-value-setq (cy vy) (spring-step cy vy tcy omega-pan dt))
                   (multiple-value-setq (z  vz) (spring-step z  vz tz  omega-zoom dt))
                   (when (zerop (mod i emit-every))
                     (push (frame tm (max 1.0 z) cx cy) kfs))
                   (incf i))))
      ;; clean open/close; %clean-timeline sorts by time and dedups collisions
      (push (frame 0.0 1.0 0.5 0.5) kfs)
      (push (frame dur (max 1.0 z) cx cy) kfs)
      (%clean-timeline kfs))))

(defun plan-timeline (session &key (activity :auto) (gap *activity-gap*)
                                   (zoom *zoom-level*) (zoom-min *zoom-min*)
                                   (lead *zoom-lead*) (tail *zoom-tail*)
                                   (track *track*)
                                   (padding 0.04) (corner 0.09)
                                   (shadow-blur 0.03) (shadow-alpha 0.5)
                                   (bg '(0.11 0.12 0.15)))
  "Full Director pass: SESSION -> zoom keyframe timeline directly renderable by
the compositor. When TRACK, a continuous spring-damped camera follows the
changing region (smooth pan/zoom). Otherwise static per-group punch-ins, where
ACTIVITY selects the source: :events (clicks/keys), :dwell, or :auto."
  (if track
      (plan-tracked-timeline session :zoom zoom :zoom-min zoom-min
                             :padding padding :corner corner
                             :shadow-blur shadow-blur :shadow-alpha shadow-alpha :bg bg)
  (schedule-zooms session
                  (ecase activity
                    (:events (detect-activity session :gap gap))
                    (:dwell  (detect-dwell-activity session))
                    (:auto   (if (session-events session)
                                 (detect-activity session :gap gap)
                                 (detect-dwell-activity session))))
                  :zoom zoom :zoom-min zoom-min :lead lead :tail tail
                  :padding padding :corner corner
                  :shadow-blur shadow-blur :shadow-alpha shadow-alpha :bg bg)))

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
