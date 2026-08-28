# takesy

A Wayland-native screen recorder for Linux, in the spirit of [polished](https://screencast) —
auto-zoom into activity, smoothed cursor motion, and a polished composited output
(padded background, rounded corners, drop shadow). Written in Common Lisp.

> Status: **early feasibility spike.** We are proving the capture path before building the fun parts.

## Architecture

The value of a polished recorder is not the capture — it's the **post-processing**.
So the design separates the two:

```
   Wayland     ┌───────────┐  raw frames + cursor track + input events
   session ──► │  CAPTURE  │ ───────────────────────────────────────┐
               └───────────┘                                         │
                                                              ┌──────▼──────┐
                                                              │  DIRECTOR   │  pure Lisp:
                                                              │             │  auto-zoom keyframes,
                                                              └──────┬──────┘  cursor easing
                                                              ┌──────▼──────┐
                                                              │ COMPOSITOR  │  GL: zoom, background,
                                                              └──────┬──────┘  shadow, rounded corners
                                                                     ▼
                                                                 ffmpeg → mp4/gif
```

### Capture (the hard, Wayland-specific part)

Target: the universal **`xdg-desktop-portal` ScreenCast → PipeWire** path (works on GNOME/KDE/wlroots).

1. Negotiate a screencast over D-Bus with `org.freedesktop.portal.ScreenCast`
   (`CreateSession` → `SelectSources` → `Start`), yielding a PipeWire node id.
2. `OpenPipeWireRemote` returns a PipeWire fd (passed via SCM_RIGHTS).
3. Consume frames from PipeWire; read `SPA_META_Cursor` for absolute cursor position
   in screen space (Wayland hides the global pointer position from clients — the
   PipeWire cursor metadata is the clean way to recover it).

### Libraries (from the CL ecosystem)

| Role | Library | Reused / built |
| --- | --- | --- |
| Talk to the portal | `dbus` (death) | reused (see caveat below) |
| PipeWire client | *hand-rolled CFFI* | **built** — no CL bindings exist |
| DMA-BUF frame import | `cl-gbm` + `cl-egl` | reused |
| GPU compositor | `cl-opengl`, `cl-egl` | reused |
| Editor window / GL context | `cl-glfw3` / `glop` | reused |
| Click/key zoom triggers | `input-event-codes` (evdev) | reused |
| Narration audio | `cl-mixed` (PipeWire backend) | reused |
| Encoding | shell out to `ffmpeg` | reused |

**Caveat found during the spike:** the `dbus` library negotiates Unix-fd passing and parses
the `h` type, but its `transport-unix` uses plain stream I/O with no `recvmsg`/`SCM_RIGHTS`
ancillary handling — so it cannot retrieve the actual fd from `OpenPipeWireRemote`. That
transport needs a small extension (tracked in beads). The handshake up to the PipeWire
**node id** works with the library as-is.

## Running the spike

Requires: SBCL, ocicl (deps auto-resolve), and a live Wayland session with
`xdg-desktop-portal` (GNOME/KDE). Running it pops the desktop's "share your screen" dialog.

```sh
sbcl --script spike/run.lisp
```

## Development

Work is tracked in [beads](https://github.com/steveej/beads) (`bd ready`).
