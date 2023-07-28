;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nasdf"
  :version "0.1.6"
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/ntemplate"
  :description "ASDF helpers for system setup, testing and installation."
  :license "BSD 3-Clause"
  ;; It cannot depend on anything because it's also in charge of fetching dependencies.
  :components ((:file "package")
               (:file "log")
               (:file "nasdf")
               (:file "install")
               (:file "submodules")
               (:file "systems")
               (:file "tests")
               (:file "compilation-tests")))
