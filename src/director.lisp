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
           #:session-duration #:*zoom-fit-margin*
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
           #:*track* #:*cursor-contain* #:*cursor-lead* #:*cursor-margin*
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
(defparameter *evi-click-lead* 0.45
  "Seconds BEFORE a click that its evidence begins -- lets the planner start moving
early so the destination is framed when the click lands (anticipation).")
(defparameter *evi-click-hold* 1.1
  "Seconds AFTER a click that its (strong) evidence persists.")
(defparameter *evi-cursor-weight* 0.12
  "Strength of the bare-cursor candidate: weak supporting evidence only, never on
its own a reason to move.")
(defparameter *evi-global-dim* 0.72
  "A region wider than this (UV, max axis) is treated as a global repaint/scroll
and its strength is halved -- prefer sustained localized activity over a big wash.")

(defstruct (cand (:constructor make-cand (cx cy x0 y0 x1 y1 strength kind)))
  cx cy x0 y0 x1 y1 strength kind)   ; kind: :click :damage :cursor

(defun %located-clicks (session)
  "Each captured click time positioned by the cursor track: list of (t cx cy) UV."
  (let ((w (session-width session)) (h (session-height session)))
    (loop for tc in (session-clicks session)
          collect (multiple-value-bind (x y) (cursor-at session tc)
                    (list (float tc 1.0) (/ x w) (/ y h))))))

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

(defun evidence-at (session t0 t1 &optional located-clicks)
  "Ranked attention candidates for the window [T0,T1], strongest first. LOCATED-
CLICKS is the precomputed (%located-clicks) list (pass it to avoid recomputing per
call). Clicks outrank damage; a bare cursor is a weak last resort."
  (let* ((now t1)
         (clicks (or located-clicks (%located-clicks session)))
         (out (%damage-regions session t0 t1)))
    ;; clicks: strong, active from LEAD before to HOLD after the click, tested at
    ;; NOW (the window end) so the planner SEES the click coming *lead* early and
    ;; can start moving before it lands (anticipation, guideline 4).
    (dolist (c clicks)
      (destructuring-bind (tc ccx ccy) c
        (when (<= (- tc *evi-click-lead*) now (+ tc *evi-click-hold*))
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
                      out))))))
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

(defparameter *shot-overview* 1.0 "Overview shot size: whole frame; context / between tasks.")
(defparameter *shot-working* 1.4 "Working shot size: the normal focused view.")
(defparameter *shot-detail* 1.9 "Detail shot size: small controls / brief precision work.")
(defparameter *shot-min-hold* 2.0
  "Minimum seconds to hold a shot before any new move (cooldown). Gives the viewer
time to orient and read; enforces one move per beat, not per input event.")
(defparameter *shot-hysteresis* 1.5
  "To LEAVE the current shot a rival target's evidence must exceed the evidence
still holding the current frame by this factor. Asymmetry prevents oscillation.")
(defparameter *shot-idle-widen* 1.5
  "Seconds with no worthwhile evidence before widening to Overview (a new beat).")
(defparameter *shot-worth* 0.25
  "Minimum candidate strength worth leaving Overview / taking a close view for.")
(defparameter *shot-both-radius* 0.45
  "Two strong candidates within this (UV, sum-of-axes) are framed TOGETHER in one
shot rather than alternated between.")
(defparameter *shot-establish* 1.0
  "Hold the opening Overview at least this long before the first close view.")
(defparameter *shot-move-eps* 0.12
  "Centre must shift more than this (UV, sum-of-axes) to count as a real reframe.")
(defparameter *shot-commit-dwell* 0.7
  "A new target must remain the dominant one for this long before the camera moves
to it. A brief flash -- a <1s repaint, popup, or result panel elsewhere -- never
persists long enough to earn a cut, so the camera stays on the task instead of
darting after transients (G2/G3/G9: one move per task step, not per event).")

(defstruct (shot (:constructor make-shot (t0 t1 kind cx cy zoom)))
  t0 t1 kind cx cy zoom)

;;; Motion timing (Phase 3). Transitions are single composed gestures with
;;; ease-in/out; duration scales with distance (a bit, not proportionally).
(defparameter *move-dur-small* 0.45  "Small reframe duration (s) (G7 350-550ms).")
(defparameter *move-dur-normal* 0.70 "Normal pan/zoom duration (s) (G7 550-850ms).")
(defparameter *move-dur-large* 0.95  "Large transition duration (s) (G7 800-1100ms).")
(defparameter *move-small-mag* 0.15  "Centre shift (UV sum) below which a move is small.")
(defparameter *move-large-mag* 0.50  "Centre shift (UV sum) above which a move is large.")
(defparameter *move-far* 0.50
  "Centre shift (UV sum) beyond which a tight->tight move routes THROUGH Overview
(widen-then-push-in) instead of a diagonal pan across the screen (G7/G11).")

;;; Cursor containment (Phase 3b). Clicks and localized activity choose the shot,
;;; but a zoomed frame can let the pointer slide off-screen. Keep it in view: pan
;;; the frame the minimum needed so the cursor stays inside the edge -- so the
;;; pointer's position also influences where the camera sits, without overriding the
;;; editorial framing. Anticipatory: we frame where the pointer WILL be (this is a
;;; recording, so the future is known), so the camera moves ahead of it.
(defparameter *cursor-contain* t
  "When true, keep the pointer inside the framed shot (pan to follow it to the edge).")
(defparameter *cursor-lead* 0.35
  "Seconds of look-ahead for containment -- frame where the pointer WILL be, so the
camera leads rather than reacts.")
(defparameter *cursor-margin* 0.10
  "Keep the pointer at least this far (UV) inside the frame edge before panning.")

(defun %contain-cursor (session tm z cx cy &key (margin *cursor-margin*))
  "Pan the shot centre (CX,CY) at zoom Z the least amount so the cursor at TM stays
MARGIN inside the frame edge, then clamp so the frame stays within the content. TM
is a near-future time (anticipation). Returns (values cx cy); unchanged at zoom 1
(the whole frame is visible) or with no cursor track."
  (if (or (null (session-cursor session)) (<= z 1.0))
      (values cx cy)
      (multiple-value-bind (px py) (cursor-at session tm)
        (let* ((w (session-width session)) (h (session-height session))
               (ux (/ px (float w 1.0))) (uy (/ py (float h 1.0)))
               (half (min 0.5 (/ 0.5 z)))
               (inner (max 0.0 (- half margin)))
               (ncx cx) (ncy cy))
          (cond ((> ux (+ cx inner)) (setf ncx (- ux inner)))
                ((< ux (- cx inner)) (setf ncx (+ ux inner))))
          (cond ((> uy (+ cy inner)) (setf ncy (- uy inner)))
                ((< uy (- cy inner)) (setf ncy (+ uy inner))))
          ;; keep the frame inside the content (same edge-guard as the shot planner)
          (values (min (- 1.0 half) (max half ncx))
                  (min (- 1.0 half) (max half ncy)))))))

(defun %smoother (u)
  "Smootherstep ease-in/out on [0,1]: zero velocity AND acceleration at both ends,
so a move starts and stops gently (no mechanical constant-speed slide)."
  (let ((x (max 0.0 (min 1.0 u)))) (* x x x (+ (* x (- (* x 6.0) 15.0)) 10.0))))

(defun %move-dur (a b)
  "Transition duration between shots A and B, bucketed by centre distance."
  (let ((mag (+ (abs (- (shot-cx a) (shot-cx b))) (abs (- (shot-cy a) (shot-cy b))))))
    (cond ((< mag *move-small-mag*) *move-dur-small*)
          ((< mag *move-large-mag*) *move-dur-normal*)
          (t *move-dur-large*))))

(defun %far-through-overview-p (a b)
  "T when the move is a long tight->tight jump that should route through Overview."
  (and (> (shot-zoom a) 1.05) (> (shot-zoom b) 1.05)
       (> (+ (abs (- (shot-cx a) (shot-cx b))) (abs (- (shot-cy a) (shot-cy b))))
          *move-far*)))

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

(defun plan-shots (session &key (window *evi-window*))
  "Walk the recording and emit a sparse list of SHOTs. Evidence-driven, with
establish-first, cooldown, hysteresis, frame-both, and idle-widen."
  (let* ((dur (session-duration session))
         (dt (/ 1.0 30.0))
         (clicks (%located-clicks session))
         (shots '())
         (cur-kind :overview) (cur-cx 0.5) (cur-cy 0.5) (cur-z 1.0)
         (cur-start 0.0) (last-move 0.0) (last-strong -1.0e9)
         (pend-cx -9.0) (pend-cy -9.0) (pend-since 0.0))  ; target-dwell tracking
    (labels ((emit (endt)
               (push (make-shot cur-start (max cur-start endt) cur-kind cur-cx cur-cy cur-z)
                     shots))
             (switch (tm kind cx cy z)
               ;; Edge-guard (G8): clamp the centre so the frame stays inside the
               ;; content -- no black bars, and the focused area never sits under a
               ;; crop edge.
               (let* ((half (min 0.5 (/ 0.5 (max 1.0 z))))
                      (ccx (min (- 1.0 half) (max half cx)))
                      (ccy (min (- 1.0 half) (max half cy))))
                 (emit tm)
                 (setf cur-kind kind cur-cx ccx cur-cy ccy cur-z z
                       cur-start tm last-move tm))))
      (loop for tm from 0.0 to dur by dt do
        (let* ((ev (evidence-at session (max 0.0 (- tm window)) tm clicks))
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
          (let ((held (>= (- tm last-move) *shot-min-hold*))    ; cooldown elapsed?
                (dwelled (>= (- tm pend-since) *shot-commit-dwell*)))  ; target settled?
            (cond
              ;; scrolling: hold the viewport steady, do not react (G8/G11).
              ((%scrolling-p session (max 0.0 (- tm window)) tm) nil)
              ;; idle beat: widen to Overview only once we've held the current shot,
              ;; activity has been gone a while, AND nothing resumes soon (no snap).
              ((and held (null strong) (not (eq cur-kind :overview))
                    (>= (- tm last-strong) *shot-idle-widen*)
                    (not (%activity-ahead-p session tm (+ tm *shot-min-hold*) clicks)))
               (switch tm :overview 0.5 0.5 *shot-overview*))
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
                 (multiple-value-bind (z kind) (%bucket-zoom x0 y0 x1 y1 allow-detail)
                   ;; clamp the target to the edge-guard BEFORE deciding it moved,
                   ;; so activity that clamps to the same frame isn't re-cut forever.
                   (let* ((half (min 0.5 (/ 0.5 (max 1.0 z))))
                          (ncx (min (- 1.0 half) (max half (* 0.5 (+ x0 x1)))))
                          (ncy (min (- 1.0 half) (max half (* 0.5 (+ y0 y1))))))
                     (cond
                       ;; establish-first: from Overview, push in once we've held
                       ;; the establishing shot long enough and the target settled.
                       ((eq cur-kind :overview)
                        (when (and held dwelled (>= tm *shot-establish*))
                          (switch tm kind ncx ncy z)))
                       ;; already close: move only past the cooldown and when the new
                       ;; target clearly beats what's holding the current frame
                       (t
                        (let ((moved (> (+ (abs (- ncx cur-cx)) (abs (- ncy cur-cy)))
                                        *shot-move-eps*))
                              (resized (not (eq kind cur-kind))))
                          (when (and held dwelled (or moved resized)
                                     (>= (cand-strength primary)
                                         (* *shot-hysteresis* (max ret 0.15))))
                            (switch tm kind ncx ncy z)))))))))))))
      (emit dur)
      (nreverse shots))))

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
         (kfs '()) (i 0))
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
                        (in-x (and b (>= tm (- (shot-t0 b) d)))))
                   (multiple-value-bind (z cx cy)
                       (if in-x
                           (%xition a b (/ (- tm (- (shot-t0 b) d)) d))
                           (values (shot-zoom a) (shot-cx a) (shot-cy a)))
                     (when (zerop (mod i emit-every))
                       (let ((zz (max 1.0 z)))
                         ;; keep the (near-future) pointer in view, panning the least
                         ;; amount needed -- the pointer influences where we sit.
                         (multiple-value-bind (ccx ccy)
                             (if *cursor-contain*
                                 (%contain-cursor session (+ tm *cursor-lead*) zz cx cy)
                                 (values cx cy))
                           (push (frame tm zz ccx ccy) kfs))))
                     (incf i)))))
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
