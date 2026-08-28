# takesy

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

> A screen recorder for modern Linux desktops (Wayland & X11).

**takesy** records your screen and turns it into a polished screencast: it
automatically zooms in on wherever you're working, smooths the cursor motion,
and composites the result — padded background, rounded corners, drop shadow — to
an mp4. No timeline, no manual keyframing; just record, and takesy directs the
shot for you.

<!-- Demo: drop a clip here, e.g. ![takesy demo](docs/demo.gif) -->

## Features

- **Auto-zoom on activity** — punches in where you're working, pans between
  spots, and eases back out.
- **Never crops the action** — watches the screen (frame diffs) and fits the
  zoom to the active region, so relevant changes always stay in frame.
- **Smoothed cursor** — a speed-adaptive spring that aims at your clicks (cutting
  the usual overshoot) yet stays responsive when you point at things.
- **Polished compositing** — the screen inset on a padded background with rounded
  corners and a soft drop shadow.
- **Click to stop** — record for as long as you like; finish with your desktop's
  screencast Stop button.
- **One self-contained binary** — builds to a single `takesy` executable.

## Requirements

- A modern Linux desktop (Wayland or X11) with `xdg-desktop-portal` + PipeWire
  (GNOME, KDE, or wlroots)
- `ffmpeg` with an H.264 encoder (`libx264` or `libopenh264`)
- [SBCL](https://www.sbcl.org/) and [ocicl](https://github.com/ocicl/ocicl) to
  build (dependencies resolve automatically)

## Build

```sh
make                # -> ./takesy   (or: sbcl --script build.lisp)
make install        # copies takesy to ~/.local/bin
```

## Usage

```sh
takesy                                 # record; click Stop in the top bar to finish
takesy --output talk.mp4 --duration 20
takesy help
```

Pick a source in the screen-share dialog, do your thing, then click your
desktop's screencast **Stop** button to finish. The cursor is hidden during
capture — takesy needs its position to drive the auto-zoom — and restored when
you're done. (If you haven't run `make install`, invoke it as `./takesy`.)

| Option | Meaning | Default |
| --- | --- | --- |
| `--output PATH` | output mp4 | `/tmp/takesy-record.mp4` |
| `--duration S` | maximum length — a safety cap; you set the length by clicking Stop | `30` |
| `--fps N` | output frame rate | `24` |
| `--height N` | maximum output height in px (never upscales past the content) | `1200` |

## How it works

1. **Capture** — receives frames and the cursor position over
   `xdg-desktop-portal` + PipeWire.
2. **Direct** (pure Lisp) — detects where you focused, plans eased auto-zoom
   keyframes fit to the active region, and smooths the cursor path.
3. **Composite** (GPU) — renders the zoom, background, rounded corners, drop
   shadow, and cursor overlay with EGL/OpenGL, then encodes to an mp4 via
   `ffmpeg`.

## Customizing

The direction is tunable from the Lisp side — for example
`takesy/director:*zoom-level*`, `*zoom-merge-gap*`, `*cursor-omega-fast*`, and
`*cursor-anticipate*` — so you can dial the zoom and cursor feel to taste.

## License

Created by [Anthony Green](https://github.com/atgreen); distributed under the
[MIT license](LICENSE).
