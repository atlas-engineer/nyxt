;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/tests/renderer/electron
  (:use :lisp-unit2))
(in-package :nyxt/tests/renderer/electron)

(defmethod files:write-file ((profile nyxt:nyxt-profile) (file files:file) &key)
  "Don't persist test data."
  nil)

(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt:history-file))
  "Don't use any history."
  #p"")
