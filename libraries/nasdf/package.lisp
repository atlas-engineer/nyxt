;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

#+sb-package-locks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :nasdf)
    (sb-ext:unlock-package :nasdf)))

(uiop:define-package :nasdf
  (:use #:cl #:uiop #:asdf)
  (:documentation "ASDF helpers for system setup, testing and installation.

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

A test system:

(defsystem \"my-project/tests\"
  :defsystem-depends-on (\"nasdf\")
  :class :nasdf-test-system
  :depends-on (alexandria lisp-unit2)
  :components ((:file \"tests\"))
  :test-suite-args (:package :my-project/tests))

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
