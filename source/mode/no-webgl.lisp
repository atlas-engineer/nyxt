;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/no-webgl-mode
    (:documentation "Disable WebGL."))
(in-package :nyxt/no-webgl-mode)

(define-mode no-webgl-mode ()
  "Disable WebGL in current buffer."
  ((previous-webgl-setting nil
                           :documentation "The state of WebGL before
no-webgl-mode was enabled.")))

(defmethod enable ((mode no-webgl-mode) &key)
  (setf (previous-webgl-setting mode) (ffi-buffer-webgl-enabled-p (buffer mode)))
  (setf (ffi-buffer-webgl-enabled-p (buffer mode)) nil))

(defmethod disable ((mode no-webgl-mode) &key)
  (setf (ffi-buffer-webgl-enabled-p (buffer mode)) (previous-webgl-setting mode)))
