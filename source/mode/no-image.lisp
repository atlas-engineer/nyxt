;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/no-image
  (:documentation "Package for `no-image-mode', mode to disable image display in buffer.
Uses `ffi-buffer-auto-load-image-enabled-p' internally."))
(in-package :nyxt/mode/no-image)

(define-mode no-image-mode ()
  "Disable images in current buffer.
Might need page reload to take effect.")

(defmethod enable ((mode no-image-mode) &key)
  (setf (ffi-buffer-auto-load-image-enabled-p (buffer mode)) nil))

(defmethod disable ((mode no-image-mode) &key)
  (setf (ffi-buffer-auto-load-image-enabled-p (buffer mode)) t))
