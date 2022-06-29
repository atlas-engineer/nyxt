;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/no-script-mode
    (:documentation "Disable Javascript."))
(in-package :nyxt/no-script-mode)

(define-mode no-script-mode ()
  "Disable Javascript in current buffer.
If the current URL is an internal one (for instance 'nyxt://...'), JavaScript is enabled.
It's automatically disabled when loading a non-internal URL.")

(defmethod enable ((mode no-script-mode) &key)
  (echo "Reload the buffer for no-script-mode to take effect."))

(defmethod disable ((mode no-script-mode) &key)
  (setf (ffi-buffer-javascript-markup-enabled-p (buffer mode)) t))

(defmethod nyxt:on-signal-load-started ((mode no-script-mode) url)
  (setf (ffi-buffer-javascript-markup-enabled-p (buffer mode)) (internal-url-p url)))
