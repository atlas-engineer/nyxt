;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/nosound-mode
    (:use :common-lisp :trivia :nyxt)
  (:documentation "Disable sound."))
(in-package :nyxt/nosound-mode)

(define-mode nosound-mode ()
  "Disable sound in current buffer."
  ((destructor
    (lambda (mode)
      (ffi-buffer-enable-sound (buffer mode) t)))
   (constructor
    (lambda (mode)
      (ffi-buffer-enable-sound (buffer mode) nil)))))
