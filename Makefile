# SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <anthony@atgreen.org>
# SPDX-License-Identifier: MIT
# takesy -- a screen recorder for modern Linux desktops (Wayland & X11)

SBCL   ?= sbcl
PREFIX ?= $(HOME)/.local
BINDIR ?= $(PREFIX)/bin

SOURCES := build.lisp takesy.asd $(wildcard src/*.lisp)

.PHONY: all build record install uninstall clean

all: takesy

## takesy: build the standalone executable
takesy: $(SOURCES)
	$(SBCL) --script build.lisp

build: takesy

## record: build (if needed) and start recording
record: takesy
	./takesy

## install: copy takesy into $(BINDIR) (default ~/.local/bin)
install: takesy
	install -d $(DESTDIR)$(BINDIR)
	install -m 755 takesy $(DESTDIR)$(BINDIR)/takesy

## uninstall: remove the installed binary
uninstall:
	rm -f $(DESTDIR)$(BINDIR)/takesy

## clean: remove the executable and the compiled-source cache
clean:
	rm -f takesy
	rm -rf $(HOME)/.cache/common-lisp/*$(CURDIR)
