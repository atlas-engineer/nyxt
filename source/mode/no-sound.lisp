;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/no-sound
  (:documentation "Package for `no-sound-mode', mode to disable sound.

Uses `ffi-buffer-sound-enabled-p' internally."))
(in-package :nyxt/mode/no-sound)

(define-mode no-sound-mode ()
  "Disable sound in current buffer.

See `nyxt/mode/no-sound' package documentation for implementation details and
internal programming APIs.")

(defmethod enable ((mode no-sound-mode) &key)
  (setf (ffi-buffer-sound-enabled-p (buffer mode)) nil))

(defmethod disable ((mode no-sound-mode) &key)
  (setf (ffi-buffer-sound-enabled-p (buffer mode)) t))
