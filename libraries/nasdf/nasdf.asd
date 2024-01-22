;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; Can't depend on CL libraries, because it's in charge of fetching them as git
;; submodules.
(defsystem "nasdf"
  :version "0.1.8"
  :author "Atlas Engineer LLC"
  :description "ASDF helpers for system setup, testing and installation."
  :license "BSD 3-Clause"
  :components ((:file "package")
               (:file "log")
               (:file "nasdf")
               (:file "install")
               (:file "submodules")
               (:file "systems")
               (:file "tests")))
