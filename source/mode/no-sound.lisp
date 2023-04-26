;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/no-sound
    (:documentation "Disable sound."))
(in-package :nyxt/mode/no-sound)

(define-mode no-sound-mode ()
  "Disable sound in current buffer.")

(defmethod enable ((mode no-sound-mode) &key)
  (setf (ffi-buffer-sound-enabled-p (buffer mode)) nil))

(defmethod disable ((mode no-sound-mode) &key)
  (setf (ffi-buffer-sound-enabled-p (buffer mode)) t))
