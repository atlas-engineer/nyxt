;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

#+sb-package-locks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :nyxt-asdf)
    (sb-ext:unlock-package :nyxt-asdf)))

(uiop:define-package :nyxt-asdf
  (:use :cl)
  (:import-from :uiop
                #:absolute-pathname-p
                #:collect-sub*directories
                #:copy-file
                #:directory-files
                #:ensure-all-directories-exist
                #:ensure-directory-pathname
                #:ensure-pathname
                #:file-exists-p
                #:getenv
                #:inter-directory-separator
                #:merge-pathnames*
                #:native-namestring
                #:quit
                #:relativize-pathname-directory
                #:run-program
                #:split-string
                #:strcat
                #:subpathp
                #:symbol-call)
  (:import-from :asdf
                #:clear-configuration
                #:perform
                #:system-relative-pathname
                #:system-source-directory)
  (:documentation "ASDF helpers for Nyxt and its extensions.

To tell ASDF to fail loading a system on warnings, add this line to the system
definition:

  :around-compile \"NYXT-ASDF:FAIL-ON-WARNINGS\"

To report unbound exported symbols:

(defsystem my-system
  :defsystem-depends-on (\"nyxt-asdf\")
  :class :nyxt-compilation-test-system
  :depends-on (foo bar)
  :packages (:foo))

A system that installs files:

(defsystem \"my-project/install\"
  :defsystem-depends-on (\"nyxt-asdf\")
  :depends-on (alexandria)
  :components ((:nyxt-desktop-file \"assets/my-project.desktop\")
               (:nyxt-icon-directory \"assets/\")
               (:nyxt-binary-file \"my-project\")
               (:nyxt-library-file \"libraries/web-extensions/libmy.so\"
                                   :if-does-not-exist nil)
               (:nyxt-source-directory \"source\")
               (:nyxt-source-directory \"nyxt-asdf\")
               (:nyxt-source-directory \"libraries\"
                :exclude-subpath (\"web-extensions\") ; Do not install this non-Lisp source.
                :exclude-types (\"o\" \"c\" \"h\" ; C code and artifacts.
                                    \"fasl\"))))

A system that fetches the Git submodules:

(defsystem \"my-project/submodules\"
  :defsystem-depends-on (\"nyxt-asdf\")
  :class :nyxt-submodule-system)
"))

#+sb-package-locks
(sb-ext:lock-package :nyxt-asdf)
