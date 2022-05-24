;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-and-set-package :nyxt/no-sound-mode
  (:documentation "Disable sound."))

(define-mode no-sound-mode ()
  "Disable sound in current buffer.")

(defmethod enable ((mode no-sound-mode) &key)
  (setf (ffi-buffer-sound-enabled-p (buffer mode)) nil))

(defmethod disable ((mode no-sound-mode) &key)
  (setf (ffi-buffer-sound-enabled-p (buffer mode)) t))
