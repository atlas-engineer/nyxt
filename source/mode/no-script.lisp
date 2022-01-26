;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/no-script-mode
    (:use :common-lisp :nyxt)
  (:documentation "Disable Javascript."))
(in-package :nyxt/no-script-mode)

(define-mode no-script-mode ()
  "Disable Javascript in current buffer.
If the current URL is an internal one (for instance 'nyxt://...'), JavaScript is enabled.
It's automatically disabled when loading a non-internal URL."
  ((destructor
    (lambda (mode)
      (ffi-buffer-enable-javascript-markup (buffer mode) t)))
   (constructor
    (lambda (mode)
      (declare (ignore mode))
      (echo "Reload the buffer for no-script-mode to take effect.")))))

(defmethod nyxt:on-signal-load-started ((mode no-script-mode) url)
  (ffi-buffer-enable-javascript-markup (buffer mode) (internal-url-p url)))
