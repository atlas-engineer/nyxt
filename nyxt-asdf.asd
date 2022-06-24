;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nyxt-asdf"
  :version "1.0.0"
  :author "Atlas Engineer LLC"
  :homepage "https://nyxt.atlas.engineer"
  :description "ASDF helpers for Nyxt and its extensions."
  :license "BSD 3-Clause"
  :pathname #p"libraries/nyxt-asdf/"
  :components ((:file "package")
               (:file "log")
               (:file "nyxt-asdf")
               (:file "install")
               (:file "submodules")
               (:file "systems")
               (:file "tests")
               (:file "user-systems")))
