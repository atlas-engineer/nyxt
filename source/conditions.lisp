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

(export-always 'browser-already-started)
(define-condition browser-already-started (nyxt-error)
  ()
  (:documentation "An existing instance of Nyxt is already running."))

(export-always 'prompt-buffer-canceled)
(define-condition prompt-buffer-canceled (error)
  ()
  (:documentation "Signaled when prompt buffer is exited abnormally (via ESC key, for example)."))
