# takesy

A Wayland-native screen recorder for Linux, in the spirit of
[polished](https://screencast). It records your screen, automatically
zooms in on wherever you're working, smooths the cursor motion, and composites a
polished result — padded background, rounded corners, and a drop shadow — to an
mp4.

## Requirements

- A Linux Wayland session with `xdg-desktop-portal` (GNOME, KDE, or wlroots)
- `ffmpeg` with an H.264 encoder (`libx264` or `libopenh264`)
- [SBCL](https://www.sbcl.org/) and [ocicl](https://github.com/ocicl/ocicl) to
  build (dependencies resolve automatically)

## Build

```sh
sbcl --script build.lisp     # produces ./takesy
```

## Record

```sh
./takesy                            # record; click Stop in the top bar to finish
./takesy --output demo.mp4 --duration 20
./takesy help
```

Pick a source in the screen-share dialog, do your thing, then click your
desktop's screencast **Stop** button in the top bar to finish. The cursor is
hidden during capture (takesy needs its position to drive the auto-zoom) and is
restored when you're done.

| Option | Meaning | Default |
| --- | --- | --- |
| `--output PATH` | output mp4 | `/tmp/takesy-record.mp4` |
| `--duration S` | maximum length (a safety cap; you decide the length by clicking Stop) | 30 |
| `--fps N` | output frame rate | 24 |
| `--scale K` | downsample the output to 1/K of the captured resolution | 3 |

## How it works

1. **Capture** — records frames and the cursor position over
   `xdg-desktop-portal` + PipeWire.
2. **Direct** — finds where you focused, plans smooth auto-zoom moves, and eases
   the cursor (aiming at where you click, staying responsive when you point).
3. **Composite** — GPU-renders the zoom, padded background, rounded corners,
   drop shadow, and cursor overlay, then encodes to an mp4 with `ffmpeg`.
