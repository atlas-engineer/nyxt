;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/no-image-mode
    (:use :common-lisp :nyxt)
  (:documentation "Disable images."))
(in-package :nyxt/no-image-mode)

(define-mode no-image-mode ()
  "Disable images in current buffer."
  ((destructor
    (lambda (mode)
      (ffi-buffer-auto-load-image (buffer mode) t)))
   (constructor
    (lambda (mode)
      (ffi-buffer-auto-load-image (buffer mode) nil)))))
