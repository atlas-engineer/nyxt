;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nasdf"
  :version "0.1.8"
  :author "Atlas Engineer LLC"
  :description "ASDF helpers for system setup, testing and installation."
  :license "BSD 3-Clause"
  :components ((:file "package")
               (:file "log")
               (:file "nasdf")
               (:file "install")
               (:file "systems")
               (:file "tests")))
