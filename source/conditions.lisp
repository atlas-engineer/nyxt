;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'nyxt-condition)
(define-condition nyxt-condition (error)
  ((message :initarg :message :accessor nyxt-condition-message))
  (:report (lambda (c stream)
             (format stream "~a" (slot-value c 'message))))
  (:documentation "An error internal to Nyxt. It should abort the ongoing command, but not the whole process."))

(define-condition nyxt-web-context-condition (nyxt-condition)
  ((context :initarg :context :reader context)))

(define-condition nyxt-prompt-buffer-canceled (error)
  ())
