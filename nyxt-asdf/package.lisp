;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

#+sb-package-locks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :nyxt-asdf)
    (sb-ext:unlock-package :nyxt-asdf)))

(uiop:define-package nyxt-asdf
  (:use #:common-lisp)
  (:import-from
   #:uiop
   #:absolute-pathname-p
   #:collect-sub*directories
   #:copy-file
   #:directory-files
   #:ensure-all-directories-exist
   #:ensure-directory-pathname
   #:ensure-pathname
   #:getenv
   #:inter-directory-separator
   #:merge-pathnames*
   #:native-namestring
   #:quit
   #:relativize-pathname-directory
   #:run-program
   #:strcat
   #:subpathp
   #:symbol-call)
  (:import-from
   #:asdf
   #:clear-configuration
   #:perform
   #:system-relative-pathname
   #:system-source-directory)
  (:documentation "ASDF helpers for Nyxt and its extensions."))

#+sb-package-locks
(sb-ext:lock-package :nyxt-asdf)
