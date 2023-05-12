;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/no-script
    (:documentation "Package for `no-script-mode', mode to disable JavaScript.

Uses `ffi-buffer-javascript-markup-enabled-p' internally and hooks into
`nyxt:on-signal-load-started' to allow toggling JS on internal pages."))
(in-package :nyxt/mode/no-script)

(define-mode no-script-mode ()
  "Disable Javascript in current buffer.

Internal URLs ('nyxt://...') always have JavaScript enabled, otherwise they
wouldn't be functional.  In other words, enabling `no-script-mode' is gracefully
ignored.

See `nyxt/mode/no-script' package documentation for implementation details and
internal programming APIs.")

(defmethod enable ((mode no-script-mode) &key)
  (echo "Reload the buffer for no-script-mode to take effect."))

(defmethod disable ((mode no-script-mode) &key)
  (setf (ffi-buffer-javascript-markup-enabled-p (buffer mode)) t))

(defmethod nyxt:on-signal-load-started ((mode no-script-mode) url)
  (setf (ffi-buffer-javascript-markup-enabled-p (buffer mode)) (internal-url-p url)))
