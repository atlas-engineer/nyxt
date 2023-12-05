;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class renderer ()
  ((name "Default"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Specialize this class and bind an instance to `*renderer*' to set the default renderer."))

(define-generic install (renderer)
  "Setup for renderer.  This may have side effects.
See also `uninstall'."
  (:export-generic-name-p t))

(define-generic uninstall (renderer)
  "Revert the side effects induced by `install'."
  (:export-generic-name-p t))
