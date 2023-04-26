;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/emacs
    (:documentation "Emacs-style bindings."))
(in-package :nyxt/mode/emacs)

(define-mode emacs-mode (nyxt/mode/keyscheme:keyscheme-mode)
  "Enable Emacs-style bindings.
To enable these bindings by default, add the mode to the list of default modes
in your configuration file.

Example:

\(define-configuration buffer
  ((default-modes (append '(emacs-mode) %slot-value%))))"
  ((glyph "ε")
   (keyscheme keyscheme:emacs)))
