;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/no-webgl
  (:documentation "Package for `no-webgl-mode', mode to disable WebGL.

Uses `ffi-buffer-webgl-enabled-p' internally and saves the settings to
`previous-webgl-setting'."))
(in-package :nyxt/mode/no-webgl)

(define-mode no-webgl-mode ()
  "Disable WebGL in current buffer.

See `nyxt/mode/no-webgl' package documentation for implementation details and
internal programming APIs."
  ((previous-webgl-setting
    nil
    :documentation "The state of WebGL before `no-webgl-mode' was enabled.")))

(defmethod enable ((mode no-webgl-mode) &key)
  (setf (previous-webgl-setting mode) (ffi-buffer-webgl-enabled-p (buffer mode)))
  (setf (ffi-buffer-webgl-enabled-p (buffer mode)) nil))

(defmethod disable ((mode no-webgl-mode) &key)
  (setf (ffi-buffer-webgl-enabled-p (buffer mode)) (previous-webgl-setting mode)))
