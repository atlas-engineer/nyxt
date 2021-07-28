;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/noimage-mode
    (:use :common-lisp :nyxt)
  (:documentation "Disable images."))
(in-package :nyxt/noimage-mode)

(define-mode noimage-mode ()
  "Disable images in current buffer."
  ((destructor
    (lambda (mode)
      (ffi-buffer-auto-load-image (buffer mode) t)))
   (constructor
    (lambda (mode)
      (ffi-buffer-auto-load-image (buffer mode) nil)))))
