;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nyxt/tests/renderer
  (:use #:common-lisp)
  (:use #:nyxt)
  (:shadowing-import-from #:lisp-unit2 #:*debug-on-error*)
  (:use #:lisp-unit2)
  (:import-from #:class-star #:define-class))
(nyxt::use-nyxt-package-nicknames)

(class-star:define-class nyxt-user::test-profile (nyxt:nyxt-profile)
  ((files:name :initform "test"))
  (:documentation "Test profile."))

(defmethod files:write-file ((profile nyxt-user::test-profile) (file files:file) &key)
  "Don't persist test data."
  nil)

(defmethod files:resolve ((profile nyxt-user::test-profile) (file nyxt:history-file))
  "Don't use any history."
  #p"")
