;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defun set-new-translation (host logical-directory
                            &optional (translated-directory (substitute #\/ #\; logical-directory)))
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
        (setf (cdr
               (assoc logical-path (logical-pathname-translations host)
                      :test #'string-equal))
              (list path-translation)
              (cdr
               (assoc logical-fasl-path (logical-pathname-translations host)
                      :test #'string-equal))
              (list fasl-translation))
        (setf (logical-pathname-translations host)
              ;; WARNING: fasl path must come first as it's more specific.
              (list (list logical-fasl-path fasl-translation)
                    (list logical-path path-translation))))))

(set-new-translation "NYXT" "nyxt-asdf")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun npath (path)
    "If NYXT_LOGICAL_PATH environment variable is set, use logical path source
location, otherwise use the translated path.

Tools such as Emacs (SLIME and SLY) may fail to make use of logical paths, say,
to go to the compilation error location."
    ;; REVIEW: Would not be necessary if https://github.com/slime/slime/issues/727
    ;; and https://github.com/atlas-engineer/nyxt/pull/2371 were fixed.
    (if (uiop:getenv "NYXT_LOGICAL_PATH")
        path
        (translate-logical-pathname path))))

(defsystem "nyxt-asdf"
  :version "1.0.0"
  :author "Atlas Engineer LLC"
  :homepage "https://nyxt.atlas.engineer"
  :description "ASDF helpers for Nyxt and its extensions."
  :license "BSD 3-Clause"
  :pathname #.(npath #p"NYXT:nyxt-asdf;")
  :components ((:file "package")
               (:file "nyxt-asdf")
               (:file "install")
               (:file "nyxt-options")
               (:file "submodules")
               (:file "tests")))
