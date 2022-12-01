;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class renderer ()
  ((name "Default"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Specialize this class and bind an instance to `*renderer*' to set the default renderer."))

(export-always 'install)
(defgeneric install (renderer)
  (:documentation "Setup for renderer.  This may have side effects.
See also `uninstall'."))

(export-always 'uninstall)
(defgeneric uninstall (renderer)
  (:documentation "Revert the side effects induced by `install'."))
