;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/no-sound-mode
    (:use :common-lisp :nyxt)
  (:documentation "Disable sound."))
(in-package :nyxt/no-sound-mode)

(define-mode no-sound-mode ()
  "Disable sound in current buffer.")

(defmethod enable ((mode no-sound-mode) &key)
  (ffi-buffer-enable-sound (buffer mode) nil))

(defmethod disable ((mode no-sound-mode) &key)
  (ffi-buffer-enable-sound (buffer mode) t))
