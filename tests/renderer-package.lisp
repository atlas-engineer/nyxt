;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nyxt/tests/renderer
  (:use #:common-lisp)
  (:use #:nyxt)
  (:shadowing-import-from #:prove #:*debug-on-error*)
  (:use #:prove)
  (:import-from #:class-star #:define-class))

(class-star:define-class nyxt-user::test-profile (nyxt:nyxt-profile)
  ((nfiles:name :initform "test"))
  (:documentation "Test profile."))

(defmethod nfiles:write-file ((profile nyxt-user::test-profile) (file nfiles:file) &key)
  "Don't persist test data."
  nil)

(defmethod nfiles:resolve ((profile nyxt-user::test-profile) (file nyxt:history-file))
  "Don't use any history."
  #p"")
