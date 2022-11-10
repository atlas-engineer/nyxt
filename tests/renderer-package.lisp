;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nyxt/tests/renderer
  (:use #:common-lisp #:lisp-unit2)
  (:import-from #:class-star #:define-class))
(in-package :nyxt/tests/renderer)
(nyxt::use-nyxt-package-nicknames)

(class-star:define-class nyxt-user::test-profile (nyxt:nyxt-profile)
  ()
  (:documentation "Test profile."))

(defmethod files:write-file ((profile nyxt-user::test-profile) (file files:file) &key)
  "Don't persist test data."
  nil)

(defmethod files:resolve ((profile nyxt-user::test-profile) (file nyxt:history-file))
  "Don't use any history."
  #p"")

(defvar +test-root+ #p"/tmp/nyxt-tests/")

(defmethod files:resolve ((profile nyxt-user::test-profile) (file files:file))
  "Store all files in a temporary `+test-root+' directory."
  (nfiles:join +test-root+  (call-next-method)))

(defmacro with-headless (&body body)
  `(let ((old-headless-p nyxt::*headless-p*))
    (unwind-protect (progn
                      (setf nyxt::*headless-p* t)
                      ,@body)
      (setf nyxt::*headless-p* old-headless-p))))
