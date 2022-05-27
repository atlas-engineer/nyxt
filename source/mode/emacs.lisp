;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/emacs-mode
    (:documentation "Emacs-style bindings."))
(in-package :nyxt/emacs-mode)

(define-mode emacs-mode (nyxt/keymap-scheme-mode:keymap-scheme-mode)
  "Enable Emacs-style bindings.
To enable these bindings by default, add the mode to the list of default modes
in your configuration file.

Example:

\(define-configuration buffer
  ((default-modes (append '(emacs-mode) %slot-default%))))"
  ((glyph "ε")
   (nyxt/keymap-scheme-mode:scheme-name scheme:emacs)))
