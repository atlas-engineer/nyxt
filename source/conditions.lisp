;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'nyxt-error)
(define-condition nyxt-error (error)
  ((message :initarg :message :accessor message))
  (:report (lambda (c stream)
             (format stream "~a" (slot-value c 'message))))
  (:documentation "An error internal to Nyxt.
It should abort the ongoing command, but not the whole process."))

(define-condition web-context-error (nyxt-error)
  ((context :initarg :context :reader context)))

(export-always 'prompt-buffer-canceled)
(define-condition prompt-buffer-canceled (error)
  ())
(export-always 'prompt-buffer-non-interactive)
(define-condition prompt-buffer-non-interactive (error)
  ((name :initarg :name :accessor name))
  (:report (lambda (c stream)
             (format stream "Tried to invoke the prompt buffer (~a) when non-interactive."
                     (name c))))
  (:documentation "See `*interactive-p*'."))
