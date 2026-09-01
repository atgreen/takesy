# Changelog

All notable changes to takesy are documented here. The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
