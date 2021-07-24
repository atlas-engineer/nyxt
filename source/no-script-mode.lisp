;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/no-script-mode
    (:use :common-lisp :trivia :nyxt)
  (:documentation "Disable Javascript."))
(in-package :nyxt/no-script-mode)

(define-mode no-script-mode ()
  "Disable Javascript in current buffer."
  ((destructor
    (lambda (mode)
      (ffi-buffer-enable-javascript-markup (buffer mode) t)))
   (constructor
    (lambda (mode)
      (ffi-buffer-enable-javascript-markup (buffer mode) nil)))))
