# SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: GPL-3.0-or-later
# takesy -- a screen recorder for modern Linux desktops (Wayland & X11)

SBCL   ?= sbcl
PREFIX ?= $(HOME)/.local
BINDIR ?= $(PREFIX)/bin

SOURCES := takesy.asd $(wildcard src/*.lisp)

# ASDF source registry: this repo, plus the ocicl/ tree that `ocicl install`
# populates (and CI restores). Lets the build find every dependency.
REGISTRY := (asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (uiop:getcwd)) (list :tree (merge-pathnames "ocicl/" (uiop:getcwd)))))

.PHONY: all build record install uninstall clean

all: takesy

## takesy: build the standalone executable (asdf:make dumps it via :build-operation)
takesy: $(SOURCES)
	$(SBCL) --non-interactive \
	        --eval "(require :asdf)" \
	        --eval '$(REGISTRY)' \
	        --eval "(asdf:make :takesy)"

build: takesy

## OPEN-SOURCE-NOTICES.txt: license notices for every vendored dependency
OPEN-SOURCE-NOTICES.txt: ocicl.csv
	@echo "Generating $@ ..."
	@echo "================================================================================" > $@
	@echo "takesy OPEN SOURCE NOTICES" >> $@
	@echo "================================================================================" >> $@
	@echo "" >> $@
	@echo "takesy is licensed under the GNU General Public License v3.0 or later." >> $@
	@echo "Copyright (C) 2026 Anthony Green <green@moxielogic.com>" >> $@
	@echo "" >> $@
	@echo "It is built with the open source Common Lisp libraries below; their" >> $@
	@echo "license notices follow." >> $@
	@echo "" >> $@
	@ocicl collect-licenses 2>/dev/null >> $@

## licenses: (re)generate the vendored-dependency license notices
licenses: OPEN-SOURCE-NOTICES.txt

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

## clean: remove build outputs and the compiled-source cache
clean:
	rm -f takesy OPEN-SOURCE-NOTICES.txt
	rm -rf $(HOME)/.cache/common-lisp/*$(CURDIR)
