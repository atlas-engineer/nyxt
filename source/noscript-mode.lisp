;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/noscript-mode
    (:use :common-lisp :trivia :nyxt)
  (:documentation "Disable Javascript."))
(in-package :nyxt/noscript-mode)

(define-mode noscript-mode ()
  "Disable Javascript in current buffer."
  ((destructor
    (lambda (mode)
      (ffi-buffer-enable-javascript-markup (buffer mode) t)))
   (constructor
    (lambda (mode)
      (ffi-buffer-enable-javascript-markup (buffer mode) nil)))))
