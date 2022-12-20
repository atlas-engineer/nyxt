;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

#+sb-package-locks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :nasdf)
    (sb-ext:unlock-package :nasdf)))

(uiop:define-package :nasdf
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
  (:documentation "ASDF helpers for system setup, testing and installation.

To tell ASDF to fail loading a system on warnings, add this line to the system
definition:

  :around-compile \"NASDF:FAIL-ON-WARNINGS\"

To report unbound exported symbols:

(defsystem my-system
  :defsystem-depends-on (\"nasdf\")
  :class :nasdf-compilation-test-system
  :depends-on (foo bar)
  :packages (:foo))

A system that installs files:

(defsystem \"my-project/install\"
  :defsystem-depends-on (\"nasdf\")
  :depends-on (alexandria)
  :components ((:nasdf-desktop-file \"assets/my-project.desktop\")
               (:nasdf-icon-directory \"assets/\")
               (:nasdf-binary-file \"my-project\")
               (:nasdf-library-file \"libraries/web-extensions/libmy.so\"
                                   :if-does-not-exist nil)
               (:nasdf-source-directory \"source\")
               (:nasdf-source-directory \"nasdf\")
               (:nasdf-source-directory \"libraries\"
                :exclude-subpath (\"web-extensions\") ; Do not install this non-Lisp source.
                :exclude-types (\"o\" \"c\" \"h\" ; C code and artifacts.
                                    \"fasl\"))))

A system that fetches the Git submodules:

(defsystem \"my-project/submodules\"
  :defsystem-depends-on (\"nasdf\")
  :class :nasdf-submodule-system)

Shell command to add a submodule to the default directory:

    git submodule add https://github.com/atlas-engineer/history-tree _build/history-tree

To update it:

    git submodule update --remote _build/history-tree
"))

#+sb-package-locks
(sb-ext:lock-package :nasdf)
