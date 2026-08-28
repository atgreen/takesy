# AGENTS.md — working notes for green-screen

Guidance for any agent (or human) hacking on this Wayland-native screen recorder.
Read the **Hazards** section before running anything that opens a screencast.

## Project shape

- Pure Common Lisp (SBCL + ocicl). Systems in `green-screen.asd`.
- Capture path is proven end-to-end: xdg-desktop-portal ScreenCast over the CL
  `dbus` client → SCM_RIGHTS fd → `libpipewire` (pure CFFI) → frame → PNG.
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
