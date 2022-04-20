;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/no-webgl-mode
  (:use :common-lisp  :nyxt)
  (:documentation "Disable WebGL."))
(in-package :nyxt/no-webgl-mode)

(define-mode no-webgl-mode ()
  "Disable WebGL in current buffer."
  ((previous-webgl-setting nil
                           :documentation "The state of WebGL before
no-webgl-mode was enabled.")))

(defmethod enable ((mode no-webgl-mode) &key)
  (setf (previous-webgl-setting mode) (ffi-buffer-webgl-enabled-p (buffer mode)))
  (ffi-buffer-enable-webgl (buffer mode) nil))

(defmethod disable ((mode no-webgl-mode) &key)
  (ffi-buffer-enable-webgl (buffer mode) (previous-webgl-setting mode)))
