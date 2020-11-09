;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/dark-mode
    (:use :common-lisp :nyxt)
  (:documentation "Mode for darkening documents."))

(in-package :nyxt/dark-mode)

(define-mode dark-mode ()
  "Mode for darkening documents."
  ((constructor
    (lambda (mode)
      (initialize-display mode)))))

(defmethod nyxt:on-signal-notify-uri ((mode dark-mode) url)
  (declare (ignore url))
  (initialize-display mode))

(defmethod initialize-display ((mode dark-mode))
  (nyxt::darken (buffer mode)))
