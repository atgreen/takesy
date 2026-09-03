;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: GPL-3.0-or-later
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
           #:session-duration #:*zoom-fit-margin*
           #:px->uv #:cursor-at #:make-synthetic-session #:validate-session
           #:*cursor-omega-slow* #:*cursor-omega-fast* #:*cursor-speed-ref*
           #:*cursor-anticipate* #:*cursor-gap-hold* #:spring-step #:ease-cursor
           #:activity-segment #:make-activity-segment #:activity-segment-p
           #:activity-segment-t-start #:activity-segment-t-end
           #:activity-segment-focus-x #:activity-segment-focus-y
           #:activity-segment-n-clicks #:activity-segment-n-keys
           #:*activity-gap* #:detect-activity
           #:*dwell-speed* #:*dwell-min* #:detect-dwell-activity
           #:*zoom-level* #:*zoom-min* #:*zoom-lead* #:*zoom-tail* #:*zoom-merge-gap*
           #:*track* #:*cursor-contain* #:*cursor-window* #:*cursor-margin*
           #:*cursor-relax* #:*camera-cursor-tau*
           #:*camera-style* #:*long-form-seconds* #:apply-camera-style #:resolve-camera-style
           #:apply-resolution-cap #:*shot-overview* #:*shot-working* #:*shot-detail*
           #:*reduced-motion* #:lint-camera-plan
           #:*damage-include-radius* #:*damage-anchor*
           #:merge-segments #:schedule-zooms #:plan-timeline))

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
  (clicks '())   ; list of click times (s); positioned via the cursor track
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

(defun cursor-at (session time &key max-gap)
  "Piecewise-linear cursor position at TIME. Return (values x y) in screen px;
clamps to the track endpoints outside its span. When MAX-GAP is given and the two
bracketing samples are more than MAX-GAP seconds apart, HOLD the earlier sample
across the gap instead of interpolating: a gap means the capture got no cursor
updates (the screen was static while the pointer moved), so the true path is
unknown -- holding the last real position, then snapping when the next sample
arrives, avoids drawing the pointer at a fictional in-between spot."
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
                      (when (and max-gap (> span max-gap))
                        (return (values (cursor-sample-x a) (cursor-sample-y a))))
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

(defparameter *cursor-omega-slow* 20.0
  "Spring stiffness (rad/s) when the pointer is slow/settling -- smoother.")
(defparameter *cursor-omega-fast* 60.0
  "Spring stiffness (rad/s) when the pointer moves fast. Stiff enough that the drawn
cursor tracks the real one closely (a soft spring visibly lags a fast flick, which
reads as the pointer being in the wrong place -- especially once zoomed in).")
(defparameter *cursor-speed-ref* 1800.0
  "Pointer speed (px/s) at which stiffness reaches *cursor-omega-fast*.")
(defparameter *cursor-anticipate* 0.15
  "Seconds before a rest/click target to start aiming the cursor straight at it.
Small so the drawn cursor doesn't lead the real one noticeably.")
(defparameter *cursor-gap-hold* 0.2
  "Cursor-track gap (s) beyond which the drawn cursor HOLDS its last real position
instead of interpolating. On-change screencast delivery gives no cursor updates
while the screen is static, so a gap's midpoint is a fictional position -- holding
avoids drawing the pointer somewhere it never was.")

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
                   ;; (only when ANTICIPATE > 0, else the /anticipate below divides by 0)
                   (next (when (plusp anticipate)
                           (find-if (lambda (r) (<= 0.0 (- (first r) tnow) anticipate)) rests))))
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
(defparameter *track-min-activity* 0.0016
  "Twitch gate: total changed UV area within an activity window must reach this
before the camera treats it as a real shot to punch into. Rejects a lone blinking
cursor / clock tick / notification dot (a single grid cell is ~0.0004) so the
frame doesn't chase a corner twitch when nothing else is happening. 0 = off.")

;;; ==================================================================
;;; Evidence layer (Phase 1). Turns raw signals into a RANKED list of
;;; attention candidates over a short time window, per the movement
;;; guidelines: clicks are strongest, sustained localized damage is
;;; medium, the cursor is weak. Damage is aggregated into regions and
;;; accumulated over the window; isolated/global damage is discounted.
;;; The shot planner (Phase 2) consumes these candidates -- it never
;;; touches raw damage directly.
;;; ==================================================================

(defparameter *evi-window* 1.0
  "Accumulation window (s): evidence is summed over this span so several updates
resolve into one coherent target instead of reacting per damage frame.")
(defparameter *evi-min-evidence* 1.6
  "A region must accumulate at least this much recency-weighted evidence (roughly
this many recent damage frames) to count -- so SUSTAINED localized activity (e.g.
a typing caret, tiny but repeated) qualifies while a lone one-frame blink/twitch
does not. This is the twitch gate, on accumulation rather than area.")
(defparameter *evi-cluster* 0.14
  "UV radius (sum-of-axes) within which damage rects merge into one region.")
(defparameter *evi-click-lead* 0.8
  "Seconds BEFORE a click that its evidence begins -- lets the planner start moving
early so the move SETTLES before the action, not after it (audit: introduce -> move
-> settle -> action -> hold; 'do not wait for a click and then zoom to it'). Calm
leads by ~move-duration + a settle margin; Energetic leads less (akn.7). The commit-
dwell gate still prevents committing to a target the pointer only passes through.")
(defparameter *evi-click-hold* 1.1
  "Seconds AFTER a click that its (strong) evidence persists.")
(defparameter *evi-click-lead-per-dist* 0.7
  "Extra anticipation lead (s) per unit of move distance (UV, sum-of-axes) between
the current shot centre and the click. A far click (e.g. a tight corner shot to the
opposite side) needs a longer traverse, so the camera must start moving earlier to
SETTLE before the click lands rather than arriving with it. Scales *evi-click-lead*
up with distance (green-screen-fuj); a same-spot click keeps the base lead.")
(defparameter *evi-cursor-weight* 0.12
  "Strength of the bare-cursor candidate: weak supporting evidence only, never on
its own a reason to move.")
(defparameter *evi-global-dim* 0.72
  "A region wider than this (UV, max axis) is treated as a global repaint/scroll
and its strength is halved -- prefer sustained localized activity over a big wash.")

(defstruct (cand (:constructor make-cand (cx cy x0 y0 x1 y1 strength kind)))
  cx cy x0 y0 x1 y1 strength kind)   ; kind: :click :damage :cursor

(defparameter *click-burst-gap* 0.4
  "Consecutive clicks within this many seconds AND *click-burst-radius* of the last
kept click are a BURST -- a double-click, rapid repeats, or hammering one control /
table row -- and collapse into a single evidence event. One move per task step, not
per click (akn.5).")
(defparameter *click-burst-radius* 0.05
  "UV radius (sum-of-axes) within which clustered clicks collapse (see *click-burst-gap*).")

(defun %located-clicks (session)
  "Each captured click positioned by the cursor track: list of (t cx cy) UV, with
double-clicks and rapid same-spot bursts collapsed to a single event so a burst
earns one move, not one per click (akn.5)."
  (let ((w (session-width session)) (h (session-height session))
        (kept '()) (lt nil) (lx nil) (ly nil))
    (dolist (tc (session-clicks session) (nreverse kept))
      (multiple-value-bind (x y) (cursor-at session tc)
        (let ((cx (/ x w)) (cy (/ y h)))
          (unless (and lt (<= (- tc lt) *click-burst-gap*)
                       (<= (+ (abs (- cx lx)) (abs (- cy ly))) *click-burst-radius*))
            (push (list (float tc 1.0) cx cy) kept)   ; a fresh event: anchor here
            (setf lx cx ly cy))
          (setf lt tc))))))                            ; always advance the burst window

(defun %damage-regions (session t0 t1 &key (max-rect *damage-max-rect*))
  "Cluster localized damage in [T0,T1] into regions (greedy, by centre proximity),
accumulating recency-weighted evidence. Return a list of :damage CANDs, strongest
first. Isolated twitches (below *track-min-activity* total) and per-rect global
rects are excluded; a region grown globally wide is down-weighted."
  (let ((span (max 1e-3 (- t1 t0)))
        (regs '()))                     ; each: (cx cy x0 y0 x1 y1 wsum area)
    (dolist (d (session-damage session))
      (destructuring-bind (dt dx0 dy0 dx1 dy1) d
        (when (and (<= t0 dt t1) (<= (- dx1 dx0) max-rect) (<= (- dy1 dy0) max-rect))
          (let* ((mcx (* 0.5 (+ dx0 dx1))) (mcy (* 0.5 (+ dy0 dy1)))
                 (wt (+ 0.5 (/ (- dt t0) span)))            ; recency weight
                 (ar (* (- dx1 dx0) (- dy1 dy0)))
                 (hit (find-if (lambda (r)
                                 (<= (+ (abs (- mcx (first r))) (abs (- mcy (second r))))
                                     *evi-cluster*))
                               regs)))
            (if hit
                (setf (nth 2 hit) (min (nth 2 hit) dx0) (nth 3 hit) (min (nth 3 hit) dy0)
                      (nth 4 hit) (max (nth 4 hit) dx1) (nth 5 hit) (max (nth 5 hit) dy1)
                      (nth 6 hit) (+ (nth 6 hit) wt) (nth 7 hit) (+ (nth 7 hit) ar)
                      ;; weighted-drift the centre toward the new rect
                      (first hit) (+ (* 0.85 (first hit)) (* 0.15 mcx))
                      (second hit) (+ (* 0.85 (second hit)) (* 0.15 mcy)))
                (push (list mcx mcy dx0 dy0 dx1 dy1 wt ar) regs))))))
    (loop for (cx cy x0 y0 x1 y1 wsum area) in regs
          for maxdim = (max (- x1 x0) (- y1 y0))
          for str0 = (- 1.0 (exp (- (/ wsum 4.0))))   ; saturating in [0,1)
          for str = (if (> maxdim *evi-global-dim*) (* 0.5 str0) str0)
          ;; sustained accumulation (not one-off area) is the twitch gate
          when (>= wsum *evi-min-evidence*)
            collect (make-cand cx cy x0 y0 x1 y1 str :damage) into out
          finally (return (sort out #'> :key #'cand-strength)))))

(defun evidence-at (session t0 t1 &optional located-clicks shot-cx shot-cy)
  "Ranked attention candidates for the window [T0,T1], strongest first. LOCATED-
CLICKS is the precomputed (%located-clicks) list (pass it to avoid recomputing per
call). Clicks outrank damage; a bare cursor is a weak last resort. SHOT-CX/SHOT-CY,
when given, are the current shot centre: a click's anticipation lead grows with its
distance from there so a far click settles before it lands (green-screen-fuj)."
  (let* ((now t1)
         (clicks (or located-clicks (%located-clicks session)))
         (out (%damage-regions session t0 t1)))
    ;; clicks: strong, active from LEAD before to HOLD after the click, tested at
    ;; NOW (the window end) so the planner SEES the click coming *lead* early and
    ;; can start moving before it lands (anticipation, guideline 4).
    (dolist (c clicks)
      (destructuring-bind (tc ccx ccy) c
        (let ((lead (if shot-cx
                        (+ *evi-click-lead*
                           (* *evi-click-lead-per-dist*
                              (+ (abs (- ccx shot-cx)) (abs (- ccy shot-cy)))))
                        *evi-click-lead*)))
        (when (<= (- tc lead) now (+ tc *evi-click-hold*))
          ;; boost a co-located damage region, else add a click candidate
          (let ((near (find-if (lambda (r)
                                  (<= (+ (abs (- ccx (cand-cx r))) (abs (- ccy (cand-cy r))))
                                      *evi-cluster*))
                                out)))
            (if near
                (setf (cand-strength near) (+ 1.0 (cand-strength near))
                      (cand-kind near) :click)
                (push (make-cand ccx ccy (- ccx 0.06) (- ccy 0.06)
                                 (+ ccx 0.06) (+ ccy 0.06) 1.0 :click)
                      out)))))))
    ;; weak cursor fallback so there is always *a* candidate
    (multiple-value-bind (x y) (cursor-at session now)
      (let ((cx (/ x (session-width session))) (cy (/ y (session-height session))))
        (push (make-cand cx cy (- cx 0.04) (- cy 0.04) (+ cx 0.04) (+ cy 0.04)
                         *evi-cursor-weight* :cursor)
              out)))
    (stable-sort out #'> :key #'cand-strength)))

;;; ==================================================================
;;; Shot planner (Phase 2). Consumes ranked evidence into a SPARSE list
;;; of shots {kind, centre, zoom} from a small vocabulary, with the
;;; editorial discipline of the guidelines: establish first, hold, a
;;; cooldown + asymmetric hysteresis so we don't oscillate, frame two
;;; co-active regions together, and widen only on a real idle beat.
;;; ==================================================================

;;; Shot vocabulary + pacing. DEFAULTS are the CALM / tutorial preset (green-screen-
;;; akn): a calm first edit for long-form demos, where camera movement is an
;;; instructional cue, not decoration. APPLY-CAMERA-STYLE flips these to the
;;; ENERGETIC preset for short promo-style clips. Audit targets (long-form): 2-4
;;; reframes/min, coherent shots 8-20s, close-ups >=3-4s, detail zoom 1.5-1.75x.
(defparameter *shot-overview* 1.0 "Overview shot size: whole frame; context / between tasks.")
(defparameter *shot-working* 1.4 "Working shot size: the normal focused view (audit typical 1.25-1.5x).")
(defparameter *shot-detail* 1.6
  "Detail shot size: small controls / brief precision work. Calm keeps this in the
audit's 1.5-1.75x 'strong detail' band (was 1.9x); a resolution-aware cap may lower
it further so text never enlarges past captured pixels (akn.4).")
(defparameter *shot-min-hold* 3.5
  "Minimum seconds to hold a shot before any new move (cooldown). Gives the viewer
time to orient and read; enforces one move per BEAT, not per input event. Calm sets
this to the audit's 3-4s minimum-useful-close-up so shots are long and few (akn.1).")
(defparameter *shot-hysteresis* 2.0
  "To LEAVE the current shot a rival target's evidence must exceed the evidence still
holding the current frame by this factor. Asymmetry prevents oscillation; a high
calm value resists per-click cutting (anti-yo-yo, akn.1).")
(defparameter *shot-idle-widen* 2.5
  "Seconds with no worthwhile evidence before widening to Overview (a new beat).")
(defparameter *shot-reestablish-after* 15.0
  "Seconds the camera may stay continuously zoomed in before it RE-ESTABLISHES --
returns to Overview for a brief breath so the viewer re-orients, then pushes back
into the work. Calm holds work far longer between breaths (was 6s) so stillness
dominates; the breath itself is short (*shot-reestablish-hold*). Good screencasts
breathe, but rarely (green-screen-cjx / akn.9).")
(defparameter *shot-reestablish-hold* 1.5
  "Seconds a RE-ESTABLISH overview beat holds before pushing back into the work. Kept
short so the breath reads as a quick re-orientation, not a destination the camera
parks at -- unlike a normal shot it does NOT inherit the full *shot-min-hold* (akn.9).")
(defparameter *shot-worth* 0.25
  "Minimum candidate strength worth leaving Overview / taking a close view for.")
(defparameter *shot-both-radius* 0.45
  "Two strong candidates within this (UV, sum-of-axes) are framed TOGETHER in one
shot rather than alternated between.")
(defparameter *shot-establish* 1.0
  "Hold the opening Overview at least this long before the first close view.")
(defparameter *shot-establish-hold* 6.0
  "Seconds a WIDE shot from a scene change (context reset or reveal) lingers before
the camera may push back in -- much longer than a normal cut so the viewer takes in
the new page / dialog instead of the camera diving straight back to detail. The
push-in still also needs a settled target, so this is a floor, not a fixed wait.")
(defparameter *shot-move-eps* 0.14
  "Centre must shift more than this (UV, sum-of-axes) to count as a real reframe.")
(defparameter *shot-dead-zone* 0.7
  "Anti-yo-yo dead zone: if a new target already sits within this fraction of the
current frame's half-size from centre, it is ALREADY comfortably visible, so the
camera holds the shot instead of re-framing. This is what lets several clicks /
typing / state-changes happen inside ONE stable shot -- the audit's core long-form
rule -- rather than a cut per interaction (akn.1).")
(defparameter *shot-commit-dwell* 1.0
  "A new target must remain the dominant one for this long before the camera moves
to it. A brief flash -- a <1s repaint, popup, or result panel elsewhere -- never
persists long enough to earn a cut, so the camera stays on the task instead of
darting after transients (G2/G3/G9: one move per task step, not per event).")
(defparameter *shot-fit-lookahead* 2.0
  "Seconds ahead the shot-size fit looks at the POINTER's travel. A click's own box
is a point, so %bucket-zoom would always pick DETAIL -- but if the pointer then
roams across the screen, a tight shot forces cursor-containment to chase it (the
source of the residual pan jerk). Growing the shot box to include where the pointer
actually goes during the hold makes the director pick a shot wide enough to HOLD the
work, so the camera sits still instead. ~one hold (*shot-min-hold*) is the horizon we
can commit to; containment covers anything beyond (green-screen-cjx / g9f).")

(defparameter *damage-anchor* :reading
  "How a DAMAGE shot places its zoom window over the activity box (green-screen-pxi).
:reading anchors the window's TOP-LEFT to the activity's top-left (keeping the left
column / line-starts and the context above it in frame -- right for terminals,
editors, docs, any left/top-anchored text); :center centers on the box centroid (the
old behaviour, which pushes the left edge off-screen and dives into the middle of a
big repaint like a directory listing). Clicks are always centered on the click, so
this only affects damage/cursor-driven shots. REPL/render-tunable.")

(defstruct (shot (:constructor make-shot (t0 t1 kind cx cy zoom &optional reason)))
  t0 t1 kind cx cy zoom
  (reason nil))   ; why this shot was cut: :establish :push-in :reframe :idle :breathe :context

;;; Motion timing (Phase 3). Transitions are single composed gestures with
;;; ease-in/out; duration scales with distance (a bit, not proportionally).
(defparameter *move-dur-small* 0.50  "Small reframe duration (s) (audit 450-700ms).")
(defparameter *move-dur-normal* 0.65 "Normal pan/zoom duration (s) (audit 450-700ms).")
(defparameter *move-dur-large* 0.85  "Large transition duration (s); a bit over the 700ms typical, warranted by distance.")
(defparameter *move-small-mag* 0.15  "Centre shift (UV sum) below which a move is small.")
(defparameter *move-large-mag* 0.50  "Centre shift (UV sum) above which a move is large.")
(defparameter *move-far-spans* 1.0
  "Widen-through-Overview trigger, in VIEWPORT-SPANS the framed content slides on
screen: Euclidean UV centre distance scaled by the mean of the two zooms. Raw UV
distance ignores zoom, but the same jump is trivial when wide and disorienting when
tight -- what matters is how far content travels *in frame*. ~1.0 span = the target
leaves the frame entirely; below it a direct pan+zoom keeps the viewer oriented and
engaged, so only genuinely large jumps establish wide first (G7/G11). The old raw
sum-of-axes threshold (0.50) fired on a mere 0.68-span move and pulled all the way
out to the desktop for continuous same-area work (green-screen-g9f). Soft band is
~0.9-1.1; a progressive partial dip would remove the cliff entirely (g9f follow-up).")
(defparameter *move-dur-through* 1.3
  "Duration (s) of a widen-through-Overview transition. Longer than *move-dur-large*
because it is really TWO gestures (zoom out, then in): a single large-move budget
forced both phases to ~2x speed, which read as a whip at 4-5s (green-screen-g9f).")

;;; ------------------------------------------------------------------
;;; Camera style presets (green-screen-akn.2). CALM/tutorial is the product default
;;; for long-form demos -- a calm first edit the user can occasionally intensify.
;;; ENERGETIC/promo is an explicit opt-in for short teaser-style clips. The style
;;; sets the pacing/zoom/breathing tunables above; APPLY-CAMERA-STYLE is called once
;;; per render before planning. Auto-selection forces CALM for long recordings.
(defparameter *camera-style* :calm
  "Active camera style: :calm (tutorial default) or :energetic (promo opt-in).")
(defparameter *reduced-motion* nil
  "When true, render a REDUCED-MOTION timeline (akn.3, W3C animation guidance): the
shot PLAN is unchanged, but transitions become direct CUTS instead of animated
pans/zooms, and cursor-containment stops drifting the frame. Emphasis then comes
from the cursor / ripples, not camera movement -- for motion-sensitive viewers.")
(defparameter *long-form-seconds* 300.0
  "Recordings at least this long are treated as long-form: CALM is forced regardless
of the requested style, per the audit (calm default for >~5min).")

(defun apply-camera-style (style)
  "Set the pacing / zoom / breathing tunables for STYLE (:calm or :energetic). CALM
follows the long-form cinematography audit (fewer, longer, calmer shots); ENERGETIC
restores the livelier short-clip behaviour. Returns the style applied."
  (ecase style
    (:calm
     (setf *shot-working* 1.4   *shot-detail* 1.6
           ;; Hold the zoom through mid-task thinking pauses instead of yo-yoing out
           ;; to Overview and back (green-screen-pxi feedback): a 4.5s idle widened on
           ;; every gap between terminal commands. Only a genuinely long lull (task
           ;; done) should widen; context changes still reset wide immediately.
           *shot-min-hold* 3.5  *shot-hysteresis* 2.0  *shot-idle-widen* 12.0
           *shot-commit-dwell* 1.0 *shot-move-eps* 0.14 *shot-dead-zone* 0.7
           ;; Never breathe on a timer: widening to Overview without any new content
           ;; to reveal reads as gratuitous (a user watching a 60s hold saw the 30s
           ;; breath as an unnecessary out-and-back). Wide moments now come only from
           ;; a REASON -- context change, reveal, or a genuine long lull (green-screen-
           ;; fuj). Effectively off; REPL-lower it if a long clip ever needs a breath.
           *shot-reestablish-after* 1.0e9 *shot-reestablish-hold* 1.5
           *evi-click-lead* 0.8
           *move-dur-small* 0.50 *move-dur-normal* 0.65 *move-dur-large* 0.85
           *move-dur-through* 1.3))
    (:energetic
     (setf *shot-working* 1.4   *shot-detail* 1.9
           *shot-min-hold* 2.0  *shot-hysteresis* 1.5  *shot-idle-widen* 1.5
           *shot-commit-dwell* 0.7 *shot-move-eps* 0.12 *shot-dead-zone* 0.5
           *shot-reestablish-after* 8.0 *shot-reestablish-hold* 2.0
           *evi-click-lead* 0.5
           *move-dur-small* 0.45 *move-dur-normal* 0.70 *move-dur-large* 0.95
           *move-dur-through* 1.5)))
  (setf *camera-style* style))

(defun resolve-camera-style (requested duration)
  "Pick the effective style: long-form (>= *long-form-seconds*) forces :calm; else the
REQUESTED style (defaulting to :calm). Returns (values style forced-p)."
  (let ((req (or requested :calm)))
    (if (and duration (>= duration *long-form-seconds*) (not (eq req :calm)))
        (values :calm t)
        (values req nil))))

(defun apply-resolution-cap (cap)
  "Limit the DETAIL zoom so a close-up doesn't enlarge the source far past its native
pixels -- which softens text and UI edges (akn.4, TechSmith). CAP is the available
crop headroom = capture-height / delivery-height (>= 1). A 4K capture delivered at
1080p gives ~2x; a near-native delivery gives ~1x. We only tighten DETAIL, and never
below WORKING: on a low-headroom capture, mild softening at the working zoom is far
better UX than disabling auto-zoom entirely (which an aggressive cap did). Returns
the applied cap."
  (let ((c (max 1.0 (float cap 1.0))))
    (when (< c *shot-detail*)
      (setf *shot-detail* (max *shot-working* c)))
    c))

;;; Cursor containment (Phase 3b). Clicks and localized activity choose the shot,
;;; but a zoomed frame can let the pointer slide off-screen. Keep it in view by
;;; framing the pointer's RANGE over a short window straddling now (recent + near
;;; future): pan the least amount to include it, and -- crucially -- WIDEN (zoom
;;; out) rather than pan when the range is too wide for the current zoom. So the
;;; pointer influences the camera (it stays visible), and a pointer that darts back
;;; and forth makes the camera pull out to hold the whole span instead of swinging
;;; after it. Framing the RANGE (not a look-ahead point) means the centre isn't led,
;;; so the drawn pointer isn't pushed off to one side.
(defparameter *cursor-contain* t
  "When true, keep the pointer in view: frame its recent range, widening rather than
panning when it ranges wide.")
(defparameter *cursor-window* 0.6
  "Half-width (s) of the window around now whose cursor range feeds the held box.
Straddles now (recent + near future) and is wide enough to span a typical back-and-
forth, so the frame settles to hold the whole sweep instead of chasing it.")
(defparameter *cursor-margin* 0.10
  "Keep the pointer range at least this far (UV) inside the frame edge.")
(defparameter *cursor-relax* 0.2
  "How fast (UV/s) the held containment box shrinks back toward the pointer once it
stops ranging wide. It EXPANDS instantly to hold a dart, then relaxes slowly -- so a
back-and-forth pointer settles the frame wide instead of pumping the zoom.")
(defparameter *camera-cursor-tau* 0.15
  "Low-pass time constant (s) applied to the pointer BEFORE the camera frames it, so
hand tremor / high-frequency motion doesn't make the camera jitter. Only the camera
sees this smoothing; the drawn cursor overlay stays on the accurate track. 0 = off.")
(defparameter *contain-deadband* 0.72
  "Deadband for pointer containment: while the (smoothed) pointer sits within this
fraction of the frame's half-size from centre, containment stays SILENT and the
camera holds the director's shot dead still. Containment ramps in only as the
pointer crosses from here out to the frame edge. Without it, containment nudges on
almost every active frame (the pointer is rarely exactly centred), so the camera
drifts constantly instead of holding -- the residual 'restlessness' after the jerk
was sprung out (green-screen-g9f).")
(defparameter *cursor-contain-omega* 7.0
  "Stiffness (rad/s) of the critically-damped spring that EASES the containment
correction into the planned camera path. %contain-box is a hard per-frame geometric
correction off a box that jumps when the pointer darts; applied raw it doubled the
frames-in-motion and quadrupled peak acceleration -- the camera twitched all through
a clip while chasing the pointer. Springing the correction (a delta on top of the
planned shot) keeps deliberate moves crisp but turns containment into a slow, calm
drift. Lower = calmer but the pointer can ride nearer the edge before the frame
catches up (green-screen-g9f).")

(defun %smooth-cursor-track (samples tau)
  "Return a low-passed (EMA, time-constant TAU seconds) copy of a cursor SAMPLES
list, preserving times. Damps jitter with minimal lag. TAU<=0 or empty -> SAMPLES."
  (if (or (null samples) (<= tau 0.0))
      samples
      (let ((px (cursor-sample-x (first samples)))
            (py (cursor-sample-y (first samples)))
            (pt (cursor-sample-time (first samples)))
            (out '()))
        (dolist (s samples (nreverse out))
          (let* ((dt (max 0.0 (- (cursor-sample-time s) pt)))
                 (a (- 1.0 (exp (- (/ dt tau))))))   ; frame-rate-independent EMA
            (setf px (+ px (* a (- (cursor-sample-x s) px)))
                  py (+ py (* a (- (cursor-sample-y s) py)))
                  pt (cursor-sample-time s))
            (push (make-cursor-sample :time (cursor-sample-time s) :x px :y py) out))))))

(defun %cursor-window-bbox (session t0 t1)
  "UV bounding box the cursor covers over [T0,T1] (dense track plus interpolated
endpoints). Return (values x0 y0 x1 y1), or NIL with no cursor track."
  (let ((c (session-cursor session)))
    (when c
      (let ((w (float (session-width session) 1.0)) (h (float (session-height session) 1.0))
            (x0 nil) (y0 nil) (x1 nil) (y1 nil))
        (flet ((acc (px py)
                 (let ((ux (/ px w)) (uy (/ py h)))
                   (setf x0 (if x0 (min x0 ux) ux) x1 (if x1 (max x1 ux) ux)
                         y0 (if y0 (min y0 uy) uy) y1 (if y1 (max y1 uy) uy)))))
          (multiple-value-call #'acc (cursor-at session t0))
          (multiple-value-call #'acc (cursor-at session t1))
          (dolist (cs c)
            (when (<= t0 (cursor-sample-time cs) t1)
              (acc (cursor-sample-x cs) (cursor-sample-y cs)))))
        (values x0 y0 x1 y1)))))

(defun %relax-box (held wb dt)
  "Update the held containment box HELD (list x0 y0 x1 y1) toward the window box WB
(same shape): expand instantly to include WB, shrink toward it by at most
*cursor-relax**DT. Returns the new box. HELD or WB NIL -> the other."
  (cond ((null wb) held)
        ((null held) (copy-list wb))
        (t (destructuring-bind (hx0 hy0 hx1 hy1) held
             (destructuring-bind (wx0 wy0 wx1 wy1) wb
               (let ((r (* *cursor-relax* dt)))
                 (list (if (< wx0 hx0) wx0 (min wx0 (+ hx0 r)))   ; left edge
                       (if (< wy0 hy0) wy0 (min wy0 (+ hy0 r)))   ; top
                       (if (> wx1 hx1) wx1 (max wx1 (- hx1 r)))   ; right
                       (if (> wy1 hy1) wy1 (max wy1 (- hy1 r))))))))))  ; bottom

(defun %contain-box (z cx cy box &key (margin *cursor-margin*))
  "Adjust the shot (Z,CX,CY) to hold the UV BOX (x0 y0 x1 y1) with MARGIN: WIDEN the
zoom to fit it (never tighter than Z, never below overview) and pan the least amount
to include it -- so once widened to just fit, the box is centred. Returns
(values zoom cx cy). Unchanged at zoom 1 or with no BOX."
  (if (or (<= z 1.0) (null box))
      (values z cx cy)
      (destructuring-bind (x0 y0 x1 y1) box
        (let* ((span (max (- x1 x0) (- y1 y0)))
               (need (+ span (* 2.0 margin)))
               (zc   (max 1.0 (min z (if (plusp need) (/ 1.0 need) z))))
               (half (min 0.5 (/ 0.5 zc)))
               (inner (max 0.0 (- half margin)))
               (ncx cx) (ncy cy))
          (when (> x1 (+ ncx inner)) (setf ncx (- x1 inner)))
          (when (< x0 (- ncx inner)) (setf ncx (+ x0 inner)))
          (when (> y1 (+ ncy inner)) (setf ncy (- y1 inner)))
          (when (< y0 (- ncy inner)) (setf ncy (+ y0 inner)))
          (values zc
                  (min (- 1.0 half) (max half ncx))
                  (min (- 1.0 half) (max half ncy)))))))

(defun %smoother (u)
  "Smootherstep ease-in/out on [0,1]: zero velocity AND acceleration at both ends,
so a move starts and stops gently (no mechanical constant-speed slide)."
  (let ((x (max 0.0 (min 1.0 u)))) (* x x x (+ (* x (- (* x 6.0) 15.0)) 10.0))))

(defun %far-through-overview-p (a b)
  "T when the move slides the framed content far enough (in viewport-spans, i.e.
zoom-scaled centre travel) to establish wide through Overview rather than pan
directly. Both ends must be zoomed in; a wide end is already establishing."
  (and (> (shot-zoom a) 1.05) (> (shot-zoom b) 1.05)
       (>= (* (sqrt (+ (expt (- (shot-cx a) (shot-cx b)) 2)
                       (expt (- (shot-cy a) (shot-cy b)) 2)))
              (* 0.5 (+ (shot-zoom a) (shot-zoom b))))
           *move-far-spans*)))

(defun %move-dur (a b)
  "Transition duration between shots A and B. A widen-through-Overview move gets its
own (longer) budget -- it does two gestures; others bucket by centre distance."
  (if (%far-through-overview-p a b)
      *move-dur-through*
      (let ((mag (+ (abs (- (shot-cx a) (shot-cx b))) (abs (- (shot-cy a) (shot-cy b))))))
        (cond ((< mag *move-small-mag*) *move-dur-small*)
              ((< mag *move-large-mag*) *move-dur-normal*)
              (t *move-dur-large*)))))

(defun %xition (a b u)
  "Eased camera framing a fraction U in [0,1] through the A->B transition. Returns
(values zoom cx cy). A far tight->tight jump dips fully to Overview at the midpoint
(widen-then-push-in); otherwise zoom eases straight across with the pan."
  (let* ((e (%smoother u))
         (cx (+ (shot-cx a) (* (- (shot-cx b) (shot-cx a)) e)))
         (cy (+ (shot-cy a) (* (- (shot-cy b) (shot-cy a)) e)))
         (z (if (%far-through-overview-p a b)
                (if (< u 0.5)
                    (+ (shot-zoom a) (* (- *shot-overview* (shot-zoom a)) (%smoother (* 2.0 u))))
                    (+ *shot-overview* (* (- (shot-zoom b) *shot-overview*)
                                          (%smoother (- (* 2.0 u) 1.0)))))
                (+ (shot-zoom a) (* (- (shot-zoom b) (shot-zoom a)) e)))))
    (values z cx cy)))

(defun %bucket-zoom (x0 y0 x1 y1 &optional allow-detail)
  "Pick a vocabulary shot size for an activity box. Returns (values zoom kind),
capped so the box is never cropped (leaves *zoom-fit-margin* around it). DETAIL is
only chosen when ALLOW-DETAIL -- reserved for click-driven precision on a small
control; damage-driven activity (typing/editing) tops out at WORKING so the field
and surrounding context stay visible (G5/G8)."
  (let* ((maxdim (max (- x1 x0) (- y1 y0)))
         (raw (if (<= maxdim 0.0) *shot-detail*
                  (/ 1.0 (* maxdim (+ 1.0 *zoom-fit-margin*))))))
    (cond ((and allow-detail (>= raw *shot-detail*)) (values *shot-detail* :detail))
          ((>= raw *shot-working*) (values *shot-working* :working))
          ((>= raw 1.2)            (values (min *shot-working* raw) :working))
          (t                       (values *shot-overview* :overview)))))

(defparameter *scroll-min-rects* 4
  "This many wide (full-width) damage rects within a window reads as SCROLLING; the
camera then holds the viewport steady rather than reacting (the content is already
moving -- adding a camera pan would double the motion, G8/G11).")

(defun %activity-ahead-p (session t0 t1 clicks)
  "T when strong evidence (a worthwhile damage region, or a click) occurs in the
future window [T0,T1] -- used so the camera doesn't widen during a gap that is
about to be filled (no widen-then-repunch)."
  (or (some (lambda (r) (>= (cand-strength r) *shot-worth*))
            (%damage-regions session t0 t1))
      (some (lambda (lc) (<= t0 (first lc) t1)) clicks)))

(defun %scrolling-p (session t0 t1)
  "T when [T0,T1] is dominated by wide, full-width damage -- the signature of
scrolling / a large repaint. Such damage never forms a target (it exceeds
*damage-max-rect*); this just lets the planner HOLD instead of widening."
  (>= (count-if (lambda (d)
                  (destructuring-bind (dt x0 y0 x1 y1) d
                    (declare (ignore y0 y1))
                    (and (<= t0 dt t1) (> (- x1 x0) *damage-max-rect*))))
                (session-damage session))
      *scroll-min-rects*))

(defparameter *context-change-area* 0.85
  "UV area a single damage rect must cover to read as a CONTEXT CHANGE -- a page
navigation, window switch, or new application/workspace -- as opposed to local
editing. Raised so an ordinary in-app content refresh (which leaves the chrome
intact) doesn't trip it; only a near-total repaint does (akn.6).")
(defparameter *context-reset-cooldown* 20.0
  "Seconds a RESET-to-wide suppresses further context-change resets. A burst of
navigation (loading a page, clicking through a few views of the SAME task) would
otherwise pull the camera wide again and again -- the in-out-in-out yo-yo. One
reset marks the transition; the ensuing work then plays in a held shot (akn.6).")

(defun %context-change-p (session t0 t1)
  "T when [T0,T1] contains a near-full-frame damage rect: a task/window/app change
that warrants an establishing RESET to Overview (akn.6)."
  (some (lambda (d)
          (destructuring-bind (dt x0 y0 x1 y1) d
            (and (<= t0 dt t1)
                 (>= (* (- x1 x0) (- y1 y0)) *context-change-area*))))
        (session-damage session)))

(defparameter *reveal-min-size* 0.4
  "A damage region at least this big (UV max axis) that appears AWAY from the current
shot reveals it: the camera widens to Overview so the whole thing is visible. This
is the dialog/menu/panel case -- a new element the tight shot doesn't contain -- that
*evi-global-dim* + hysteresis would otherwise bury (green-screen-fuj).")
(defparameter *reveal-dist* 0.25
  "How far (UV, sum-of-axes) a reveal region's centre must sit from the CURRENT shot
centre to count as 'somewhere else' -- so a modal that pops elsewhere reveals, while
more output in the same place (a big ls in the terminal we're already framing) does
not, and the held zoom stays put.")

(defun %reveal-candidate (ev cx cy)
  "The first evidence region big enough and far enough from the current shot centre
(CX,CY) to warrant a reveal-widen, or NIL. Clicks/cursor are excluded -- a click
reframes normally; this is for large NEW content that loses the hysteresis contest."
  (find-if (lambda (c)
             (and (eq (cand-kind c) :damage)
                  (>= (cand-strength c) *shot-worth*)
                  (>= (max (- (cand-x1 c) (cand-x0 c)) (- (cand-y1 c) (cand-y0 c)))
                      *reveal-min-size*)
                  (> (+ (abs (- (cand-cx c) cx)) (abs (- (cand-cy c) cy))) *reveal-dist*)))
           ev))

(defparameter *reveal-area* 0.35
  "A raw damage rect covering at least this fraction of the frame, coinciding with a
click, is a click-driven page-scale change (browser navigation, a full panel/dialog)
-- the whole view changed on purpose, so the camera stays / goes WIDE to show it
rather than punching into the click. compute-damage's per-frame bbox for such a
repaint is too wide for %damage-regions (>*damage-max-rect*), so it never becomes a
normal candidate; this catches it from the raw track. Keyboard-driven repaints of the
same size (a big ls) have no click and are unaffected (green-screen-fuj).")
(defparameter *reveal-repaint-lookahead* 1.5
  "Seconds to look AHEAD for the repaint, since it follows the click by a beat -- so
the reveal preempts the establish push-in instead of punching in then widening.")
(defparameter *reveal-cooldown* 2.5
  "Min seconds between click-repaint reveals -- long enough not to re-fire across one
repaint's multi-frame window, short enough that DISTINCT navigation clicks each
reveal. Deliberately NOT the 20s *context-reset-cooldown*: a browser nav 8s after a
context reset must still reveal (green-screen-fuj).")

(defun %click-repaint-p (session t0 t1 clicks &optional (area *reveal-area*))
  "T when a click in [T0,T1] coincides with a large repaint: a raw damage rect
covering >= AREA of the frame in the same window. A deliberate, view-scale change."
  (and (some (lambda (lc) (<= t0 (first lc) t1)) clicks)
       (some (lambda (d)
               (destructuring-bind (dt x0 y0 x1 y1) d
                 (and (<= t0 dt t1) (>= (* (- x1 x0) (- y1 y0)) area))))
             (session-damage session))))

(defun plan-shots (session &key (window *evi-window*))
  "Walk the recording and emit a sparse list of SHOTs. Evidence-driven, with
establish-first, cooldown, hysteresis, frame-both, reveal, and idle-widen."
  (let* ((dur (session-duration session))
         (dt (/ 1.0 30.0))
         (clicks (%located-clicks session))
         (shots '())
         (cur-kind :overview) (cur-cx 0.5) (cur-cy 0.5) (cur-z 1.0)
         (cur-start 0.0) (last-move 0.0) (last-strong -1.0e9)
         (last-wide 0.0)                                  ; last time the camera was wide
         (last-context -1.0e9)                            ; last context-change reset time
         (last-reveal -1.0e9)                             ; last click-repaint reveal time
         (reestablish-p nil)                              ; current shot is a breathe beat
         (cur-reason :establish)                          ; why the current shot was cut
         (pend-cx -9.0) (pend-cy -9.0) (pend-since 0.0))  ; target-dwell tracking
    (labels ((emit (endt)
               (push (make-shot cur-start (max cur-start endt) cur-kind cur-cx cur-cy cur-z
                                cur-reason)
                     shots))
             (switch (tm kind cx cy z &optional reest (reason :reframe))
               ;; Edge-guard (G8): clamp the centre so the frame stays inside the
               ;; content -- no black bars, and the focused area never sits under a
               ;; crop edge. REEST marks a re-establish breathe beat, which holds only
               ;; briefly (*shot-reestablish-hold*) rather than the full min-hold.
               (let* ((half (min 0.5 (/ 0.5 (max 1.0 z))))
                      (ccx (min (- 1.0 half) (max half cx)))
                      (ccy (min (- 1.0 half) (max half cy))))
                 (emit tm)
                 (setf cur-kind kind cur-cx ccx cur-cy ccy cur-z z
                       cur-start tm last-move tm reestablish-p reest cur-reason reason))))
      (loop for tm from 0.0 to dur by dt do
        (let* ((ev (evidence-at session (max 0.0 (- tm window)) tm clicks cur-cx cur-cy))
               (strong (remove-if (lambda (c) (< (cand-strength c) *shot-worth*)) ev))
               (primary (first strong))
               ;; retention: strongest evidence still near the CURRENT centre
               (ret (loop for c in ev
                          when (<= (+ (abs (- (cand-cx c) cur-cx))
                                      (abs (- (cand-cy c) cur-cy)))
                                   0.2)
                            maximize (cand-strength c))))
          (when strong (setf last-strong tm))
          ;; target-dwell: reset the pending target whenever the dominant candidate
          ;; jumps to a new place; a target must persist *shot-commit-dwell* before
          ;; we move to it, so brief flashes never earn a cut.
          (when primary
            (unless (<= (+ (abs (- (cand-cx primary) pend-cx))
                           (abs (- (cand-cy primary) pend-cy)))
                        *shot-move-eps*)
              (setf pend-cx (cand-cx primary) pend-cy (cand-cy primary) pend-since tm)))
          (when (eq cur-kind :overview) (setf last-wide tm))   ; track time spent wide
          (let ((held (>= (- tm last-move)                      ; cooldown elapsed?
                          ;; A scene-change wide (context reset / reveal) LINGERS so
                          ;; the viewer takes in the new page before the camera dives
                          ;; back in -- much longer than a normal cut or a breath beat
                          ;; (green-screen-fuj: 'zoom out, then don't go back in for a
                          ;; while').
                          (cond (reestablish-p *shot-reestablish-hold*)
                                ((member cur-reason '(:context :reveal)) *shot-establish-hold*)
                                (t *shot-min-hold*))))
                (dwelled (>= (- tm pend-since) *shot-commit-dwell*)))  ; target settled?
            (cond
              ;; scrolling: hold the viewport steady, do not react (G8/G11).
              ((%scrolling-p session (max 0.0 (- tm window)) tm) nil)
              ;; click-driven page-scale repaint (browser navigation, a full panel):
              ;; the whole view changed on purpose -- reveal it WIDE and never punch
              ;; into the click. Fires from Overview too (re-holds wide, so the
              ;; establish push-in is suppressed for min-hold), looking slightly ahead
              ;; since the repaint follows the click. Once per cooldown (fuj).
              ((and (> (- tm last-reveal) *reveal-cooldown*)
                    (%click-repaint-p session (max 0.0 (- tm window))
                                      (+ tm *reveal-repaint-lookahead*) clicks))
               (setf last-reveal tm)
               (switch tm :overview 0.5 0.5 *shot-overview* nil :reveal))
              ;; context change (page nav / window / app / workspace): RESET wide to
              ;; re-establish before the next task. Fires only from a close shot we've
              ;; held, and at most once per *context-reset-cooldown* -- so a burst of
              ;; navigation within one task doesn't yo-yo the camera in-out-in-out
              ;; (akn.6; the 1:12-1:27 problem).
              ((and held (not (eq cur-kind :overview))
                    (> (- tm last-context) *context-reset-cooldown*)
                    (%context-change-p session (max 0.0 (- tm window)) tm))
               (setf last-context tm)
               (switch tm :overview 0.5 0.5 *shot-overview* nil :context))
              ;; reveal: a large new element (dialog / menu / panel) appeared away
              ;; from the tight shot -- global-dim + hysteresis would leave the camera
              ;; frozen on the old spot, so override and widen to Overview to show the
              ;; whole thing. Gated by min-hold; distinct from a big ls in the SAME
              ;; place (that stays put, %reveal-candidate's distance test) (fuj).
              ((and held (not (eq cur-kind :overview))
                    (%reveal-candidate ev cur-cx cur-cy))
               (switch tm :overview 0.5 0.5 *shot-overview* nil :reveal))
              ;; idle beat: widen to Overview only once we've held the current shot,
              ;; activity has been gone a while, AND nothing resumes soon (no snap).
              ((and held (null strong) (not (eq cur-kind :overview))
                    (>= (- tm last-strong) *shot-idle-widen*)
                    (not (%activity-ahead-p session tm (+ tm *shot-min-hold*) clicks)))
               (switch tm :overview 0.5 0.5 *shot-overview* nil :idle))
              ;; re-establish beat: after a long continuous stretch zoomed in, breathe
              ;; back to Overview so the viewer re-orients, then establish-first pushes
              ;; back into the work. Gated by min-hold so it never cuts a fresh shot.
              ;; Flagged as a beat so it holds only briefly (a breath, not a park).
              ((and held (not (eq cur-kind :overview))
                    (> (- tm last-wide) *shot-reestablish-after*))
               (switch tm :overview 0.5 0.5 *shot-overview* t :breathe))
              ;; a target worth a close view
              (primary
               ;; frame-both: fold in a 2nd strong candidate that's nearby
               (let* ((second (find-if (lambda (c)
                                         (and (not (eq c primary))
                                              (>= (cand-strength c) *shot-worth*)
                                              (<= (+ (abs (- (cand-cx c) (cand-cx primary)))
                                                     (abs (- (cand-cy c) (cand-cy primary))))
                                                  *shot-both-radius*)))
                                       ev))
                      (allow-detail (eq (cand-kind primary) :click))
                      (x0 (cand-x0 primary)) (y0 (cand-y0 primary))
                      (x1 (cand-x1 primary)) (y1 (cand-y1 primary)))
                 (when second
                   (setf x0 (min x0 (cand-x0 second)) y0 (min y0 (cand-y0 second))
                         x1 (max x1 (cand-x1 second)) y1 (max y1 (cand-y1 second))))
                 ;; spread-aware fit: grow the box to include where the pointer travels
                 ;; during the coming hold, so a shot is only as tight as the work is
                 ;; localized -- the camera holds instead of containment chasing (cjx).
                 (multiple-value-bind (sx0 sy0 sx1 sy1)
                     (%cursor-window-bbox session tm (+ tm *shot-fit-lookahead*))
                   (when sx0
                     (setf x0 (min x0 sx0) y0 (min y0 sy0) x1 (max x1 sx1) y1 (max y1 sy1))))
                 (multiple-value-bind (z kind) (%bucket-zoom x0 y0 x1 y1 allow-detail)
                   ;; clamp the target to the edge-guard BEFORE deciding it moved,
                   ;; so activity that clamps to the same frame isn't re-cut forever.
                   (let* ((half (min 0.5 (/ 0.5 (max 1.0 z))))
                          ;; Reading-anchor damage (green-screen-pxi): put the window's
                          ;; top-left near the activity's top-left, so line-starts and
                          ;; the context above stay in frame instead of centering on the
                          ;; centroid (which crops the left/top and dives into the middle
                          ;; of a big repaint). Clicks are precise -> always centered.
                          (reading (and (eq *damage-anchor* :reading) (not allow-detail)))
                          (m   (* half *zoom-fit-margin*))   ; small inset off the edge
                          ;; Pull the frame toward the screen top-left as far as
                          ;; possible while still showing the activity's far corner
                          ;; (x1,y1) -- so the left column / line-starts and the
                          ;; context above stay in view, with the activity sitting
                          ;; toward the lower-right of the frame. max/min then pins
                          ;; to the screen edge-guard. :center keeps the old centroid.
                          (tx  (if reading (- (+ x1 m) half) (* 0.5 (+ x0 x1))))
                          (ty  (if reading (- (+ y1 m) half) (* 0.5 (+ y0 y1))))
                          (ncx (min (- 1.0 half) (max half tx)))
                          (ncy (min (- 1.0 half) (max half ty))))
                     (cond
                       ;; establish-first: from Overview, push in once we've held
                       ;; the establishing shot long enough and the target settled.
                       ((eq cur-kind :overview)
                        (when (and held dwelled (>= tm *shot-establish*))
                          (switch tm kind ncx ncy z nil :push-in)))
                       ;; already close: move only past the cooldown and when the new
                       ;; target clearly beats what's holding the current frame -- AND
                       ;; only when the target isn't already comfortably in frame.
                       (t
                        (let* ((cur-half (min 0.5 (/ 0.5 (max 1.0 cur-z))))
                               ;; anti-yo-yo: the target already sits inside the current
                               ;; frame's dead zone -> it's visible, hold the shot so a
                               ;; whole local task plays in ONE stable shot (akn.1).
                               (in-frame (and (<= (abs (- ncx cur-cx)) (* cur-half *shot-dead-zone*))
                                              (<= (abs (- ncy cur-cy)) (* cur-half *shot-dead-zone*))))
                               (moved (> (+ (abs (- ncx cur-cx)) (abs (- ncy cur-cy)))
                                         *shot-move-eps*))
                               (resized (not (eq kind cur-kind)))
                               ;; hold when the target is in-frame and no zoom change is
                               ;; needed -- a pure pan to chase a still-visible subject is
                               ;; exactly the restlessness to avoid; a zoom change (make
                               ;; legible) is still allowed.
                               (pan-only-in-frame (and in-frame (not resized))))
                          (when (and held dwelled (or moved resized) (not pan-only-in-frame)
                                     (>= (cand-strength primary)
                                         (* *shot-hysteresis* (max ret 0.15))))
                            (switch tm kind ncx ncy z)))))))))))))
      (emit dur)
      (nreverse shots))))

;;; ------------------------------------------------------------------
;;; Motion linter (akn.8). A post-plan report: flags moves that the audit warns
;;; against and labels each shot with why it exists. Advisory only -- it inspects
;;; the same SHOT list the compositor renders, so the numbers match the output.
(defparameter *lint-min-closeup* 3.0
  "Close-ups (working/detail) held less than this many seconds are flagged as too
short to read (audit: minimum useful close-up 3-4s).")
(defparameter *lint-moves-per-min* 6.0
  "Reframes/min above this over the clip is flagged as excess motion (audit warn >6).")

(defun %shot-reason (prev shot)
  "A short human reason for SHOT, from the trigger recorded when it was cut."
  (declare (ignore prev))
  (ecase (or (shot-reason shot) :reframe)
    (:establish "establish — opening context")
    (:push-in   (format nil "push-in — ~(~A~) on the settled target" (shot-kind shot)))
    (:reframe   (format nil "reframe — ~(~A~) on a new target" (shot-kind shot)))
    (:idle      "idle-widen — activity paused, pulled wide")
    (:breathe   "re-establish — periodic breath, re-orient")
    (:context   "reset — context change (page/window/app)")))

(defun lint-camera-plan (session &key (stream *standard-output*))
  "Plan SESSION and print an advisory motion report: per-shot reasons + audit
warnings (excess density, too-short close-ups, zoom yo-yo). Returns the warnings."
  (let* ((shots (plan-shots session))
         (dur   (max 1e-3 (session-duration session)))
         (nmoves (max 0 (1- (length shots))))
         (rate  (* 60.0 (/ nmoves dur)))
         (warns '()))
    (format stream "~&  [lint] ~D shots over ~,1Fs = ~,1F reframes/min~%" (length shots) dur rate)
    (when (> rate *lint-moves-per-min*)
      (push (format nil "excess motion: ~,1F reframes/min (audit calm <=~,1F) -- prefer stillness"
                    rate *lint-moves-per-min*) warns))
    ;; per-shot reasons + short close-up flags
    (loop for (prev shot) on (cons nil shots)
          while shot
          for dwell = (- (shot-t1 shot) (shot-t0 shot))
          do (format stream "  [lint]  ~5,1Fs ~9A z~,2F  ~A~%"
                     (shot-t0 shot) (shot-kind shot) (shot-zoom shot) (%shot-reason prev shot))
             (when (and (> (shot-zoom shot) 1.05) (< dwell *lint-min-closeup*))
               (push (format nil "short close-up at ~,1Fs held only ~,1Fs (audit >=~,1Fs)"
                             (shot-t0 shot) dwell *lint-min-closeup*) warns)))
    ;; zoom yo-yo: in/out zoom reversals between consecutive shots
    (let ((rev 0))
      (loop for (a b c) on shots while c
            for d1 = (- (shot-zoom b) (shot-zoom a))
            for d2 = (- (shot-zoom c) (shot-zoom b))
            when (< (* d1 d2) -1e-4) do (incf rev))
      (when (>= rev 3)
        (push (format nil "zoom yo-yo: ~D in/out reversals -- hold a region for the whole task" rev) warns)))
    (dolist (w (nreverse warns)) (format stream "  [lint] WARNING: ~A~%" w))
    warns))

(defun shots->keyframes (session shots
                         &key (padding 0.04) (corner 0.09)
                              (shadow-blur 0.03) (shadow-alpha 0.5)
                              (bg '(0.11 0.12 0.15)))
  "Motion layer (Phase 3): render the SHOT list into keyframes as composed,
duration-controlled ease-in/out transitions. Each transition occupies the TAIL of
the outgoing shot and ARRIVES at the next shot's start time (so a click-driven
destination is framed as the action lands); otherwise the shot is held perfectly
still. Long tight->tight jumps route through Overview (widen-then-push-in)."
  (let* ((dur (session-duration session))
         (dt (/ 1.0 60.0)) (emit-every 3)
         (sv (coerce shots 'vector)) (nsh (length sv))
         (kfs '()) (i 0)
         (have-cursor (and (session-cursor session) t))
         (held nil)                     ; decaying containment box (UV x0 y0 x1 y1)
         ;; Spring-smoothed containment correction (a DELTA on the planned path) and
         ;; its velocity, per axis + zoom. Eased so the pointer-follow drifts calmly
         ;; instead of twitching frame-to-frame (green-screen-g9f).
         (sdx 0.0) (sdy 0.0) (sdz 0.0) (vdx 0.0) (vdy 0.0) (vdz 0.0)
         ;; The camera frames a LOW-PASSED cursor so hand tremor doesn't jitter it;
         ;; the overlay still draws the accurate track (green-screen).
         (cam-session (make-session :width (session-width session)
                                    :height (session-height session)
                                    :cursor (%smooth-cursor-track (session-cursor session)
                                                                  *camera-cursor-tau*))))
    (flet ((frame (time zoom ccx ccy)
             (kf:make-keyframe :time (max 0.0 (min dur (float time 1.0)))
                               :zoom (float zoom 1.0)
                               :center-x (float ccx 1.0) :center-y (float ccy 1.0)
                               :padding padding :corner-radius corner
                               :shadow-blur shadow-blur :shadow-alpha shadow-alpha
                               :bg-color bg))
           (idx-at (tm)
             (let ((k 0))
               (dotimes (j nsh k)
                 (when (<= (shot-t0 (aref sv j)) tm) (setf k j))))))
      (when (plusp nsh)
        (loop for tm = 0.0 then (+ tm (float dt 1.0))
              while (<= tm (+ dur (float dt 1.0)))
              do (let* ((ci (idx-at tm))
                        (a (aref sv ci))
                        (b (when (< ci (1- nsh)) (aref sv (1+ ci))))
                        ;; transition occupies [t0_b - d, t0_b], arriving at t0_b;
                        ;; clamp d so it fits inside the outgoing shot's hold.
                        (d (when b (min (%move-dur a b)
                                        (max 0.1 (- (shot-t0 b) (shot-t0 a))))))
                        ;; Reduced motion: no eased transition at all -- hold the
                        ;; current shot's static framing; the frame CUTS when the shot
                        ;; index advances at the next t0 (akn.3).
                        (in-x (and b (not *reduced-motion*) (>= tm (- (shot-t0 b) d))))
                        (uf   (if in-x (/ (- tm (- (shot-t0 b) d)) d) 0.0))
                        ;; A widen-through-Overview move crosses z=1, where
                        ;; %contain-box is discontinuous (it snaps the centre to 0.5).
                        ;; Fade containment out across the middle of such a move so the
                        ;; snap lands where its weight is ~0, and full containment
                        ;; resumes at each end to match the held shots (green-screen-g9f).
                        (through-x (and in-x (%far-through-overview-p a b)))
                        ;; weight: 1 at the move's ends, 0 at its wide midpoint.
                        (cw   (if through-x (let ((s (- (* 2.0 uf) 1.0))) (* s s)) 1.0)))
                   (multiple-value-bind (z cx cy)
                       (if in-x
                           (%xition a b uf)
                           (values (shot-zoom a) (shot-cx a) (shot-cy a)))
                     ;; Grow/relax the held cursor box every step so it settles wide
                     ;; on a ranging pointer instead of pumping frame-to-frame.
                     (when (and *cursor-contain* have-cursor)
                       (multiple-value-bind (wx0 wy0 wx1 wy1)
                           (%cursor-window-bbox cam-session (- tm *cursor-window*)
                                                (+ tm *cursor-window*))
                         (setf held (%relax-box held (when wx0 (list wx0 wy0 wx1 wy1))
                                                (float dt 1.0)))))
                     ;; Containment target: how far to PAN the planned frame to keep
                     ;; the pointer in view. The DIRECTOR owns the zoom -- containment
                     ;; only pans, never widens (its zoom pull-outs during held detail
                     ;; shots were the most jarring motion in a clip; the director's
                     ;; shot choice, not the pointer, decides how tight we sit). The
                     ;; correction is gated by a deadband and eased with a critically-
                     ;; damped spring EVERY step, so it drifts in gently instead of
                     ;; snapping, and only near the frame edge (green-screen-g9f).
                     (let ((zz (max 1.0 z)) (tdx 0.0) (tdy 0.0) (tdz 0.0))
                       (when (and *cursor-contain* held (not *reduced-motion*))
                         (multiple-value-bind (kz kx ky) (%contain-box zz cx cy held)
                           (declare (ignore kz))  ; pan only; the director owns the zoom
                           ;; Deadband gate: how far the smoothed pointer sits from the
                           ;; planned centre, as a fraction of the frame half-size. 0 =
                           ;; centred, 1 = at the edge. Below *contain-deadband* the gate
                           ;; is 0 (hold dead still); it smoothersteps to 1 at the edge,
                           ;; so containment only engages as the pointer threatens to
                           ;; leave -- the camera holds the shot the rest of the time (g9f).
                           (let ((gate 1.0))
                             (when have-cursor
                               (multiple-value-bind (px py) (cursor-at cam-session tm)
                                 (let* ((w  (float (session-width session) 1.0))
                                        (h  (float (session-height session) 1.0))
                                        (half (max 1e-3 (min 0.5 (/ 0.5 zz))))
                                        (off  (/ (max (abs (- (/ px w) cx)) (abs (- (/ py h) cy)))
                                                 half)))
                                   (setf gate (%smoother (/ (- off *contain-deadband*)
                                                            (max 1e-3 (- 1.0 *contain-deadband*))))))))
                             (setf tdx (* cw gate (- kx cx))
                                   tdy (* cw gate (- ky cy))
                                   tdz 0.0))))               ; pan only; director owns zoom
                       (multiple-value-setq (sdx vdx)
                         (spring-step sdx vdx tdx *cursor-contain-omega* (float dt 1.0)))
                       (multiple-value-setq (sdy vdy)
                         (spring-step sdy vdy tdy *cursor-contain-omega* (float dt 1.0)))
                       (multiple-value-setq (sdz vdz)
                         (spring-step sdz vdz tdz *cursor-contain-omega* (float dt 1.0)))
                       (when (zerop (mod i emit-every))
                         (push (frame tm (+ zz sdz) (+ cx sdx) (+ cy sdy)) kfs))
                       (incf i))))))
      (push (frame 0.0 1.0 0.5 0.5) kfs)
      (%clean-timeline kfs))))

(defun plan-editor-timeline (session &key (padding 0.04) (corner 0.09)
                                          (shadow-blur 0.03) (shadow-alpha 0.5)
                                          (bg '(0.11 0.12 0.15)))
  "Editor model (Phase 2): evidence -> sparse shot list -> keyframes."
  (shots->keyframes session (plan-shots session)
                    :padding padding :corner corner
                    :shadow-blur shadow-blur :shadow-alpha shadow-alpha :bg bg))

(defun plan-timeline (session &key (activity :auto) (gap *activity-gap*)
                                   (zoom *zoom-level*) (zoom-min *zoom-min*)
                                   (lead *zoom-lead*) (tail *zoom-tail*)
                                   (track *track*)
                                   (padding 0.04) (corner 0.09)
                                   (shadow-blur 0.03) (shadow-alpha 0.5)
                                   (bg '(0.11 0.12 0.15)))
  "Full Director pass: SESSION -> zoom keyframe timeline directly renderable by
the compositor. When TRACK, the editorial shot planner directs the camera
(evidence -> shots -> composed motion). Otherwise static per-group punch-ins,
where ACTIVITY selects the source: :events (clicks/keys), :dwell, or :auto."
  (if track
      (plan-editor-timeline session :padding padding :corner corner
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
