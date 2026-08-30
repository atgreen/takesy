# Changelog

All notable changes to takesy are documented here. The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[1.1.0]: https://github.com/atgreen/takesy/releases/tag/v1.1.0
[1.0.0]: https://github.com/atgreen/takesy/releases/tag/v1.0.0
