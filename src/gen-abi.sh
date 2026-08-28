#!/bin/sh
# Regenerate src/pw-abi.lisp from the installed PipeWire/SPA headers.
# Requires: gcc, pkg-config, pipewire-devel.
set -e
here=$(dirname "$0")
cc $(pkg-config --cflags libpipewire-0.3) -o "$here/gen-abi" "$here/gen-abi.c"
"$here/gen-abi" > "$here/pw-abi.lisp"
rm -f "$here/gen-abi"
echo "wrote $here/pw-abi.lisp"
