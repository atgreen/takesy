# Changelog

All notable changes to takesy are documented here. The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

[1.0.0]: https://github.com/atgreen/takesy/releases/tag/v1.0.0
