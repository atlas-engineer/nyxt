;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nasdf)

;; TODO: Use full-fledged logging facility?
;; Maybe we want to keep this dependency-free though...

(defvar *log-prefix* "; ")

(defun logger (control-string &rest format-arguments)
  "Like `format' but assumes `*error-output*' as a stream."
  (let ((*standard-output* *error-output*))
    (fresh-line )
    (princ *log-prefix*)
    (apply #'format t control-string format-arguments)
    (fresh-line)))
