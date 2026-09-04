# Changelog

All notable changes to takesy are documented here. The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.6.0] - 2026-09-03

### Changed

- **`--webcam` is now a flag, not an option.** The camera and framing are chosen in
  the preview that opens before recording, so `--webcam` takes no argument.
  **Breaking:** `--webcam auto` / `--webcam /dev/videoN` are no longer accepted on
  record/capture. (For `takesy render`, `--webcam FILE` still composites a
  pre-recorded clip.)
- **More director tuning, from real terminal + browser screencasts:**
  - **Selections are a first-class zoom trigger** — highlighting code (or any color
    change) now gathers into one sustained region and zooms in, instead of trickling
    away as thin per-frame damage.
  - **Reveals for big new content** — a dialog/menu, or a click that repaints much of
    the screen (browser navigation), widens to show the whole thing; a *focused*
    click (like a highlight) can break that linger to zoom into the work.
  - **Holds through motion** — scrolling / animation / live-updating content (a run
    of big repaints) holds the current shot instead of chasing; the camera also holds
    a zoomed shot through in-frame activity rather than bouncing on minor zoom flips.
  - **Scene-change wides linger** before the camera dives back to detail.

### Added

- **`--speech-aware`** (default on): when narration is recorded, camera moves are
  nudged onto speech pauses — cut on the breath, not mid-sentence.
- **`--damage-anchor reading|center`**: reading-anchored framing keeps the left
  column / line-starts and context above in view instead of centering on the change.

## [1.5.0] - 2026-09-03

### Changed

- **Calmer, reason-driven auto-zoom camera.** A batch of director fixes from real
  screencast feedback:
  - **Holds the zoom through pauses.** It no longer widens to Overview on every
    thinking pause and punches back in — mid-task pauses hold the shot, and the
    camera never breathes out on a timer; wide moments come only from a real reason.
  - **Reveals big changes.** A dialog/menu appearing away from the shot, or a click
    that repaints much of the screen (e.g. a browser navigation), now widens to show
    the whole thing instead of staying zoomed in the corner.
  - **Scene-change wides linger.** After a page/context change the camera stays wide
    long enough to take the new view in, rather than diving straight back to detail.
  - **Reading-anchored framing.** Screen activity is framed keeping the left column /
    line-starts and the context above in view, instead of centring on the change and
    cropping it — tunable with `--damage-anchor reading|center`.
  - **Anticipation scales with distance.** A far-away click gets a longer lead so the
    camera settles before it lands.

## [1.4.1] - 2026-09-03

### Changed

- **Relicensed from the MIT License to GPL-3.0-or-later.** Every dependency
  linked into the build is license-compatible with GPLv3.
- Updated vendored dependencies (`ocicl latest`): `cl-xmlspam` now ships a
  BSD-3-Clause license (it was previously unlicensed), and `trivial-garbage`
  was bumped.

## [1.4.0] - 2026-09-03

### Added

- **Self-calibrating A/V sync (clapperboard).** The count-in "go" tone is now
  played *into* the recorded audio and detected there at render time, so the audio
  track is aligned to the picture from a real marker instead of the old
  `audio_duration − video_span` estimate (which over-trimmed and made the audio
  drift). Falls back to the estimate when a capture has no tone.
- **Sharper webcam.** The camera is captured at its native resolution (largest
  MJPEG mode up to 1080p, was the ~640×480 driver default) and stored losslessly
  (`-c:v copy`) instead of re-encoded to mpeg4 — a much crisper picture-in-picture,
  with less CPU during capture.
- **`--webcam-offset S`** — signed webcam-PiP sync nudge (`+` pushes the inset
  later), mirroring `--audio-offset`. Normally unneeded; the webcam lead is aligned
  automatically.
- **`--style calm|energetic`** — camera pacing preset (calm/tutorial default;
  recordings over 5 min force calm).
- **`--reduced-motion`** — cut between shots instead of animating pans and zooms
  (accessibility).
- **`--lint`** — print the motion-linter report (shot reasons + best-practice
  warnings).
- **`--ripple-intensity F`** — tune click-ripple strength and contrast halo.

### Fixed

- **Audio no longer starts slightly early.** The small, consistent ffplay
  playback latency the clapperboard leaves behind is now calibrated
  (`*sync-beep-latency*` = 0.15s), so recordings are audio-aligned out of the box.
- **Webcam picture-in-picture now lines up with the picture.** Its lead is measured
  tail-free from the capture clock, instead of `duration − span` — which folded the
  webcam's post-stop tail into the front trim and pushed the inset out of sync.
- Both `--audio-offset` and `--webcam-offset` are now documented in the README
  options table (`--audio-offset` was previously undocumented).

## [1.3.9] - 2026-09-01

### Changed

- **The auto-zoom camera is much calmer.** Three fixes to the editorial director,
  after studying a real recording that still felt jerky:
  - **No more gratuitous zoom-out on moderate reframes.** Whether to establish wide
    (widen-through-overview) vs. move directly is now decided by how far the framed
    content actually slides *on screen* (zoom-scaled viewport-spans), not raw UV
    distance. A moderate working→detail reframe now pans-and-tightens directly
    instead of pulling all the way out to the desktop and punching back in.
  - **Cursor-containment no longer twitches.** Keeping the pointer in frame used to
    apply a hard per-frame correction that doubled the camera's frames-in-motion and
    quadrupled its peak acceleration. It is now eased with a critically-damped spring
    and gated by a deadband (it holds dead still while the pointer sits comfortably
    inside the frame), and it only *pans* — the director owns the zoom, so there are
    no jarring zoom pull-outs during a held shot.
  - **The director picks wider shots for roaming activity.** Shot size now accounts
    for where the pointer travels during the hold, so a shot is only as tight as the
    work is localized — the camera holds still instead of chasing a wandering pointer.
  - Net on the test clip: peak camera acceleration down ~3.7×, perceptible pan and
    zoom motion each roughly halved, with the pointer still kept in frame.

## [1.3.8] - 2026-09-01

### Added

- **`--camera-smoothing S`** — expose the camera's hand-tremor smoothing (seconds;
  `0` = off, default `0.15`) as a CLI option, alongside the existing cursor-easing
  flags.

## [1.3.7] - 2026-09-01

### Changed

- **Camera no longer jitters on hand tremor.** Now that the pointer track is dense
  and accurate (libinput fusion), the auto-zoom camera was following the real
  high-frequency micro-motion of the hand. The camera now frames a low-passed
  cursor (`*camera-cursor-tau*`, 0.15s) so tremor is smoothed out of the framing;
  the drawn cursor overlay still tracks the accurate position.

## [1.3.6] - 2026-09-01

### Fixed

- **Capture regression from 1.3.5.** Requesting a positive minimum framerate made
  some compositors reject the format with `state -> error (no more input formats)`.
  Reverted to a `min=0` framerate range; cursor gaps are handled entirely at render
  (libinput fusion + gap hold), which doesn't touch format negotiation.

## [1.3.5] - 2026-09-01

### Fixed

- **Accurate pointer even when the screen is static** (the "pointer way off on some
  machines" report). Root cause: the screencast delivers cursor position only on
  screen change, so moving the mouse over a static window (HW cursor hidden) left
  gaps with no cursor samples, and the render interpolated a fictional path across
  them. Three-part fix:
  - **libinput motion fusion** — takesy now reads *continuous* relative pointer
    motion from libinput (the kernel input layer, below Wayland's restrictions) and
    fuses it with the accurate-but-gappy PipeWire cursor: PipeWire supplies exact
    anchor positions, libinput supplies the real path between them. The drawn cursor
    follows the actual motion (holding when still, moving when it moved) and lands on
    the true positions. Best-effort; needs the `input` group like click capture.
  - **Steady capture framerate** — we now ask the compositor for a positive minimum
    framerate instead of `0`, so cursor metadata keeps arriving during static
    stretches.
  - **Gap hold fallback** — with no motion track available, the cursor holds its last
    real position across a gap (`*cursor-gap-hold*`) rather than drawing a fictional
    midpoint.

## [1.3.4] - 2026-09-01

### Added

- **Audible count-in** — the 3-2-1 countdown now beeps each second (and a higher
  tone when recording starts), via ffplay.

### Fixed

- **The drawn pointer tracks the real pointer.** The cursor-smoothing spring was
  soft enough to visibly lag a fast flick (up to ~7% of frame width, magnified once
  zoomed), reading as the pointer being in the wrong place. Stiffened it so the
  drawn cursor stays within a couple of frame-pixels of the real position, and
  fixed a divide-by-zero when `--cursor-anticipate` was 0.

## [1.3.3] - 2026-09-01

### Fixed

- **Camera pointer-follow no longer offsets the pointer or lurches on back-and-forth
  motion.** The 1.3.2 containment framed a look-ahead point (which pushed the drawn
  pointer off to one side) and panned to chase the cursor (nauseating when the
  pointer moved back and forth). It now frames the pointer's recent *range* without
  leading the centre, and **widens** to hold a wide or oscillating range instead of
  panning after it — steady frame, centred pointer. Tunables `*cursor-window*` /
  `*cursor-relax*` / `*cursor-margin*`.

## [1.3.2] - 2026-09-01

### Changed

- **The auto-zoom camera keeps the pointer in view.** Clicks and screen activity
  still choose the shot, but the pointer's position now influences where the camera
  sits: a zoomed frame pans the minimum needed so the cursor never slides off-screen,
  and it does so *anticipatorily* — framing where the pointer is heading (the
  recording's known future), so the camera leads rather than reacts. Tunable via
  `*cursor-contain*` / `*cursor-lead*` / `*cursor-margin*`.

### Fixed

- **Webcam captures at full framerate.** The v4l2 recorder (and preview) now prefer
  `-input_format mjpeg` when the camera advertises it, instead of letting ffmpeg
  pick the raw format — which is USB-bandwidth-limited and drops many cameras to a
  low, choppy framerate. Falls back to the default for raw-only cameras.

## [1.3.1] - 2026-09-01

### Fixed

- **Packages now install a working libfixposix.** iolib dlopens the *unversioned*
  `libfixposix.so`, which ships in the `-devel` / `-dev` package (the runtime
  package only provides the versioned SONAME). The RPM now requires
  `libfixposix-devel` and the DEB `libfixposix-dev`, fixing
  `Error opening shared object "libfixposix.so"` on a clean install.

## [1.3.0] - 2026-09-01

### Changed

- **No default recording time limit** — `--duration` is now an optional safety cap
  with no default; recording runs until you click Stop. (The disk-budget backstop
  still bounds encoder-less captures.)

### Added

- **Webcam framing preview** — recording a live webcam now always opens a local web
  page first to pick the input camera and frame yourself (drag to pan, scroll to
  zoom). takesy owns the camera via v4l2 and streams a preview JPEG, so there's no
  getUserMedia/permission dance; the browser `<canvas>` reproduces the PiP shader's
  exact sampling, so the preview matches the recording. New framing controls
  `--webcam-zoom` / `--webcam-pan-x` / `--webcam-pan-y` (usually set in the preview)
  are saved to the manifest so a re-render reproduces the framing.
- **Webcam PiP framing** — `--webcam-corner` shapes the inset along one continuous
  control: `1` (default) is a circle, `0` a hard square, in between a rounded
  square. `--webcam-border` / `--webcam-border-color` set the frame width and
  colour. The inset shader is a unified rounded-box SDF — a circle is just the box
  whose corner radius equals its half-size.
- **Live webcam capture** — `--webcam /dev/videoN` (or `--webcam auto` to pick the
  first working camera) records the webcam *during* screen capture, in parallel
  like audio, and composites it as the circle picture-in-picture automatically.
  `--webcam FILE` still composites a pre-recorded clip at render time.

### Fixed

- **Webcam PiP no longer plays in slow motion** when the camera ran below its
  nominal rate (e.g. 30→20fps in low light). Such a clip is tagged at the nominal
  rate but holds fewer frames; the decoder was padding it back to nominal while the
  compositor indexed at the true rate, so it played at ~0.66×. The webcam is now
  decoded at exactly the rate it's indexed by.
- **Webcam/audio sync** — the parallel webcam and audio recorders start before the
  first screen frame, so each carried a pre-roll that made the picture lag the
  sound. The render now trims each source's lead (its duration minus the screen
  span, since all sources stop together), aligning the webcam PiP and audio to the
  first-frame origin.
- **Colours no longer swap** on captures whose negotiated pixel format isn't BGRA:
  the streaming encoder now uses the negotiated format, and the raw-frame fallback
  tells the compositor the correct byte order.
- **Capture-layer robustness** — firewalled the PipeWire C callbacks so a Lisp
  condition can't unwind into C; time-boxed the encoder/audio/webcam child waits;
  bounded empty/dmabuf-only buffers instead of hanging; plugged foreign-memory and
  EGL-display leaks on error paths; removed a stray `/tmp` debug write.

## [1.2.0] - 2026-08-30

### Changed

- **Editorial camera** — the auto-zoom is rebuilt as an evidence-ranked shot
  planner: it ranks attention (clicks > sustained localized activity > cursor),
  picks shots from a small vocabulary (overview / working / detail), holds each
  shot with a cooldown and hysteresis, moves in single composed eased gestures,
  establishes wide first, widens *through* the overview on large jumps, and won't
  chase brief flashes/popups. Far calmer and more legible than the previous
  spring-follower.
- **CLI is now built on clingon** with per-command options and `--version`.

### Added

- **`--log-camera PATH`** — write the per-frame camera path (zoom/pan) to CSV and
  print a motion summary.

### Removed

- The legacy spring-follower director and its tuning options (`--zoom`,
  `--zoom-min`, `--track`, `--snap`, `--track-linger`, `--pan-speed`,
  `--zoom-speed`, `--zoom-out-speed`, `--track-anticipate`, `--text-follow`,
  `--zoom-merge-gap`) — the editorial camera needs no per-move knobs.

## [1.1.0] - 2026-08-29

A big round of directing, compositing, and capture features, plus smarter
zoom defaults.

### Added

- **Click ripples** — an expanding ring on each click with a subtle cursor
  press animation (`--ripples on|off`, on by default), fed by best-effort
  evdev click capture during recording.
- **Blurred background** — `--bg blur` uses a frosted, blurred copy of the
  screen as the backdrop.
- **Social reframe** — `--aspect W:H` (e.g. `9:16`, `1:1`) reframes the output;
  the whole screen is **scaled to fit** inside it (letterboxed), never cropped.
- **`--margin F`** — configurable inset margin around the screen.
- **Fixed-region capture** — `--region X,Y,W,H` frames a specific screen
  rectangle instead of auto-cropping to content.
- **Trim idle time** — `--trim-idle on` cuts dead stretches (no screen change)
  to tighten long demos (`--idle-threshold`, `--max-idle`).
- **Webcam picture-in-picture** — `--webcam PATH` composites a webcam
  video/image as a circle-cropped inset (`--webcam-pos`, `--webcam-size`).
- **Countdown** — `--countdown N` shows a 3-2-1 before recording starts.
- **Pause / resume** — `kill -USR1 <pid>` toggles capture; paused time is cut
  and the timeline stays gapless.
- **`--zoom-min F`** — a floor zoom that forces a punch-in on activity too
  spread out for the auto-fit (e.g. full-screen apps), centered on the action.

### Changed

- **Defaults**: the background is now **blur** (pass `--bg <colour>` for a
  solid), and **`--zoom-min` defaults to 1.6** (pass `--zoom-min 0` to disable).
- The auto-zoom now centers on **where the screen is changing** (the damage
  centroid) rather than the mouse — so it frames the text you're typing even
  when the pointer is idle elsewhere.
- The clip now always **opens on the whole region** and eases into the zoom.

### Fixed

- `--aspect` cropped the screen instead of fitting it; it now letterboxes.
- Renders that started with immediate activity opened already-zoomed.

## [1.0.0] - 2026-08-29

First release. takesy records your screen and directs it into a polished
screencast — auto-zoom, eased cursor, and clean compositing — straight to an mp4
or GIF.

### Added

- **Auto-zoom on activity** — punches in where you're working, pans between
  spots, and eases back out, fitting the zoom to real screen changes so the
  action is never cropped.
- **Smoothed cursor** — a speed-adaptive, anticipatory spring that aims at your
  clicks, cutting the usual overshoot while staying responsive when you point.
- **Polished compositing** — the screen inset on a padded background with
  rounded corners (optional) and a soft drop shadow, rendered on the GPU
  (EGL/OpenGL).
- **Capture once, render many** — `takesy capture` writes a self-contained
  recording dir; `takesy render DIR` re-runs the direction and compositing, so
  you can try different framing, background, or cursor without re-recording.
- **Custom cursor images** — `--cursor PATH` (with `--cursor-hotspot` and
  `--cursor-size`) draws your own cursor instead of the built-in arrow.
- **Background image** — `--bg-image PATH` draws a cover-fit image behind the
  inset; `--bg` sets a solid colour otherwise.
- **Audio** — `--audio system|mic|both` records desktop sound, your mic, or both
  (mixed), muxed into the mp4 as AAC.
- **Animated GIF output** — an `--output` path ending in `.gif` produces a GIF
  (no audio) instead of an mp4.
- **Direction tuning** — `--zoom`, `--zoom-merge-gap`, `--cursor-omega-fast`,
  and `--cursor-anticipate` dial the auto-zoom and cursor feel.
- **Other options** — `--output`, `--duration`, `--fps`, `--height`, and
  `--corner-radius` (0 = square corners).
- **One self-contained binary** — builds to a single `takesy` executable via
  `make` (SBCL + ocicl).

### Notes

- Capture streams frames to a compressed intermediate, and render streams frames
  straight to the encoder, so long recordings stay small on disk.
- A killed run (SIGTERM/SIGINT) best-effort closes the portal session so it can't
  leave the desktop with a hidden cursor.

[1.2.0]: https://github.com/atgreen/takesy/releases/tag/v1.2.0
[1.1.0]: https://github.com/atgreen/takesy/releases/tag/v1.1.0
[1.0.0]: https://github.com/atgreen/takesy/releases/tag/v1.0.0
