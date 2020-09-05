;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/buffer-listing-mode
  (:use :common-lisp :trivia :nyxt)
  (:documentation "Mode for buffer-listings"))
(in-package :nyxt/buffer-listing-mode)

(define-mode buffer-listing-mode ()
  "Mode for buffer-listing."
  ())
