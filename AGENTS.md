# AGENTS.md — working notes for takesy

Guidance for any agent (or human) hacking on this xdg-desktop-portal + PipeWire
screen recorder (works on modern Linux desktops -- Wayland & X11).
Read the **Hazards** section before running anything that opens a screencast.

## Project shape

- Pure Common Lisp (SBCL + ocicl). Systems in `takesy.asd`.
- Capture path is proven end-to-end: xdg-desktop-portal ScreenCast over the CL
  `dbus` client → SCM_RIGHTS fd → `libpipewire` (pure CFFI) → frame → PNG.
- Compositor MVP is proven end-to-end (bead `green-screen-7k8`): headless GL
  (`takesy/compositor`) renders an eased keyframe timeline
  (`takesy/keyframe`, the Director↔Compositor contract) → zoom/pan, padded
  background, rounded corners, drop shadow → H.264 mp4. Stack is **hybrid**:
  reuse `cl-opengl` for GL calls, hand-roll CFFI only for the EGL bootstrap
  (`src/egl.lisp`) that `cl-opengl` omits. Still on a stub timeline + still
  source; real capture-frame/Director wiring is `green-screen-7k8.7`.
- Work is tracked in **beads** (`bd ready`). Regenerate PipeWire ABI constants
  with `sh src/gen-abi.sh` (needs `pipewire-devel`).

## ⚠️ Hazards — learn from these, do not repeat

### 1. `cursor_mode=METADATA` hides the hardware cursor and can wedge Mutter

**What happened (2026-08-28):** the spike requested `cursor_mode=METADATA` in
`SelectSources` and did **not** cleanly close the portal session — it relied on
the D-Bus connection dropping at process exit. `METADATA` mode tells the
compositor to stop drawing the hardware cursor (it hands the cursor to the
client as metadata instead). After several capture runs, GNOME/Mutter was left
in a bad state: the on-screen **cursor disappeared and the laptop trackpad
stopped responding**. Only a logout/reboot reliably restored input. This
disrupted the user's machine mid-session.

**Rules:**
- **Default to `cursor_mode=EMBEDDED` (2).** The hardware cursor is never hidden
  and the cursor is composited into the frames. Only request `METADATA` (4) once
  we actually composite the cursor ourselves (the Director), and even then treat
  it as a mode that MUST be paired with clean teardown.
- **Always close the session and tear down PipeWire before exiting**, in an
  `unwind-protect`:
  - call `org.freedesktop.portal.Session.Close` on the `session_handle`;
  - `pw_stream_disconnect` / `pw_stream_destroy`, `pw_context_destroy`,
    `pw_main_loop_destroy`, `pw_deinit`.
  Do not rely on the D-Bus connection dropping to clean up.
- If a run is killed (timeout/SIGTERM) mid-session, assume the compositor may be
  left in a bad cursor/input state.

### 2. Interactive capture runs are disruptive — minimize them

Each run pops a GNOME "Share your screen" dialog and requires the user to pick a
source. It also, per hazard #1, can perturb the live desktop. Therefore:

- **Validate everything possible offline first.** SPA PODs are validated against
  real `spa_debug_pod` via `src/val-pod.c` — always do this before a live run.
  Parse/format logic can be tested on captured `.bin` dumps.
- **Batch what you need into a single run** rather than many trial runs.
- **Warn the user before each interactive run** and tell them exactly what to do
  (pick a source; the cursor may briefly hide).
- If you must kill a run, remember it may need a Session.Close it never got.

### 3. Recovering a wedged cursor / trackpad (no code, user-run)

- Toggle cursor: `gsettings set org.gnome.desktop.interface cursor-size 48`
  then back to its prior value.
- Toggle touchpad: `gsettings set org.gnome.desktop.peripherals.touchpad
  send-events disabled` then `enabled`.
- Reload the I²C-HID touchpad driver (root):
  `sudo modprobe -r i2c_hid_acpi && sudo modprobe i2c_hid_acpi`.
- **Reliable fix:** log out and back in, or reboot. Fully resets Mutter+libinput.
- The TrackPoint (red nub) usually still works even when the trackpad is dead —
  the user can navigate with it to log out.

## Testing notes

- `--script` does NOT load `~/.sbclrc`; `spike/run.lisp` bootstraps ocicl itself.
- Force a clean rebuild when in doubt:
  `rm -rf ~/.cache/common-lisp/*/home/green/git/green-screen`.
- Frame geometry: read stride/size from the buffer **chunk** (ground truth), not
  only the Format POD. The compositor wraps fixated Format values in
  `Choice(None)`, so scalars sit 16 bytes past the value-POD body — see
  `parse-format` / `scalar-offset` in `src/pipewire.lisp`.

### Compositor (GL) notes

- **Headless GL is offline/CI-friendly** — no share dialog, unlike capture. Each
  milestone has a self-checking entry point in `src/compositor.lisp`
  (`bringup-test`, `texture-1to1-test`, `zoom-crop-test`, `compose-test`,
  `compose-reduction-check`, `compose-shadow-test`, `render-demo`); load
  `takesy/compositor` and call one.
- **Headless context** comes up via `EGL_PLATFORM_SURFACELESS_MESA` +
  `EGL_DEFAULT_DISPLAY` (routes through glvnd to `libEGL_mesa`, Intel path). The
  `libEGL warning: pci id ... driver (null)` lines are **benign** — glvnd probes
  NVIDIA first, fails, falls back to Mesa. GBM-on-`/dev/dri/renderD128` is the
  fallback if surfaceless is ever unavailable.
- **`cl-opengl` status quirk:** `gl:get-shader`/`get-program` may return the
  status as `T`, `1`, `NIL`, `0`, or `:false`; normalise before testing (see
  `shader-ok-p`). Also `gl:check-framebuffer-status` returns
  `:framebuffer-complete-oes`, not `:framebuffer-complete`.
- **ffmpeg encoder varies:** Fedora's default ffmpeg has **no `libx264`** (ships
  `libopenh264`). Don't hardcode an encoder — `pick-h264-encoder` probes
  `ffmpeg -encoders` and falls back. `yuv420p` needs even width/height.
- **Orientation:** readback is bottom-origin and we save/encode **without** a
  vflip. Uploading source rows as a texture inverts (data row 0 → GL `v=0`) and
  `glReadPixels` inverts again — the two cancel, so RGBA rows arrive top-first
  and output is upright. In the shader, **image-down = increasing `P.y`** (the
  drop shadow offsets `+P.y`).
