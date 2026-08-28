;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <anthony@atgreen.org>
;;;; SPDX-License-Identifier: MIT
;;;; build.lisp -- build the `takesy` executable.
;;;;
;;;;   sbcl --script build.lisp    ->  ./takesy
;;;;
;;;; `--script` skips ~/.sbclrc, so we bootstrap ocicl's source registry
;;;; ourselves, load takesy/cli, and dump a standalone executable whose toplevel
;;;; is takesy/cli:main.

(let ((rt #p"/home/green/.local/share/ocicl/ocicl-runtime.lisp"))
  (when (probe-file rt) (load rt)))

(asdf:initialize-source-registry
 (list :source-registry (list :directory (uiop:getcwd)) :inherit-configuration))

(handler-case
    (asdf:load-system "takesy/cli")
  (error (e)
    (format *error-output* "~&Failed to load takesy/cli: ~A~%" e)
    (uiop:quit 1)))

;; find-symbol (not a read-time #'takesy/cli:main) so this file reads before the
;; package exists.
(let ((main (find-symbol "MAIN" (find-package :takesy/cli))))
  (unless main (error "takesy/cli:main not found"))
  (format t "~&Dumping ./takesy ...~%")
  (sb-ext:save-lisp-and-die "takesy"
                            :toplevel (fdefinition main)
                            :executable t))
