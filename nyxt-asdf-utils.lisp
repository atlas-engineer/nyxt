;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defpackage :nyxt-asdf-utils
  (:use #:common-lisp)
  (:export
   #:set-new-translation
   #:npath))
(in-package :nyxt-asdf-utils)

(defun set-new-translation (host logical-directory
                            &optional (translated-directory (substitute #\/ #\; logical-directory)))
  ;; REVIEW: Use `*load-pathname*' or `*load-truename*' instead of
  ;; `*default-pathname-defaults*'?  We could also use
  ;; (asdf:system-source-directory :nyxt)), but only after the definition of the
  ;; system.
  (let ((logical-path (uiop:strcat host ":" logical-directory ";**;*.*.*"))
        (logical-fasl-path (uiop:strcat host ":" logical-directory ";**;*.fasl.*"))
        (path-translation (uiop:ensure-pathname
                           (uiop:subpathname* *default-pathname-defaults*
                                              translated-directory)
                           :ensure-directory t
                           :wilden t))
        (fasl-translation (uiop:ensure-pathname
                           (asdf:apply-output-translations *default-pathname-defaults*)
                           :wilden t)))
    (if (ignore-errors (logical-pathname-translations host))
        (flet ((set-alist (key value)
                 (let ((pair (assoc key (logical-pathname-translations host)
                                    :test #'string-equal)))
                   (if pair
                       (setf (cdr pair) (list value))
                       (push (list key value)
                             (logical-pathname-translations host))))))
          (set-alist logical-path path-translation)
          (set-alist logical-fasl-path fasl-translation))
        (setf (logical-pathname-translations host)
              ;; WARNING: fasl path must come first as it's more specific.
              (list (list logical-fasl-path fasl-translation)
                    (list logical-path path-translation))))))

(defun npath (path)
  "If NYXT_LOGICAL_PATH environment variable is set, use logical path source
location, otherwise use the translated path.

Tools such as Emacs (SLIME and SLY) may fail to make use of logical paths, say,
to go to the compilation error location."
  ;; REVIEW: Would not be necessary if https://github.com/slime/slime/issues/727
  ;; and https://github.com/atlas-engineer/nyxt/pull/2371 were fixed.
  (if (uiop:getenv "NYXT_LOGICAL_PATH")
      path
      (translate-logical-pathname path)))
