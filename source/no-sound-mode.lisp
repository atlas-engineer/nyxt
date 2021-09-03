;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/no-sound-mode
    (:use :common-lisp :nyxt)
  (:documentation "Disable sound."))
(in-package :nyxt/no-sound-mode)

(define-mode no-sound-mode ()
  "Disable sound in current buffer."
  ((destructor
    (lambda (mode)
      (ffi-buffer-enable-sound (buffer mode) t)))
   (constructor
    (lambda (mode)
      (ffi-buffer-enable-sound (buffer mode) nil)))))
