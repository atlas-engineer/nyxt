;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defun set-new-translation (host pair)
  (if (logical-pathname-translations host)
      (pushnew pair
               (logical-pathname-translations host)
               :key #'first :test #'string-equal)
      (setf (logical-pathname-translations host)
            (list pair))))

(set-new-translation
 "NYXT"
 `("NYXT:nyxt-asdf;**;*.fasl.*"
   ,(uiop:ensure-pathname (asdf:apply-output-translations *default-pathname-defaults*)
                          :wilden t)))
(set-new-translation
 "NYXT"
 `("NYXT:nyxt-asdf;**;*.*.*"
   ,(uiop:ensure-pathname (uiop:subpathname* *default-pathname-defaults* "nyxt-asdf/") :wilden t)))

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
