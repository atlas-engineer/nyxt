;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt-asdf)

(defmacro export-always (symbols &optional (package nil package-supplied?))
  "Like `export', but also evaluated at compile time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export ,symbols ,@(and package-supplied? (list package)))))

(defun env-true-p (env-variable)
  (let ((value (uiop:getenv env-variable)))
    (or (string-equal "true" value)
        (string-equal "on" value)
        (string-equal "1" value))))

(export-always '*git-program*)
(defvar *git-program* "git")
