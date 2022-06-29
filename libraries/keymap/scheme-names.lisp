;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :keymap/scheme)

(defvar cua (make-scheme-name "cua"))
(defvar emacs (make-scheme-name "emacs" cua))
(defvar vi-normal (make-scheme-name "vi-normal" cua emacs))
(defvar vi-insert (make-scheme-name "vi-insert" cua))
