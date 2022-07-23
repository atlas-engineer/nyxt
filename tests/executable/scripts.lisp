;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/executable)
(use-nyxt-package-nicknames)

(defvar *executable* (asdf:system-relative-pathname :nyxt "nyxt"))

(define-test scripts ()
  (assert-error 'uiop/run-program:subprocess-error
                (uiop:run-program (list *executable*
                                        "--script"
                                        (uiop:native-namestring
                                         (asdf:system-relative-pathname :nyxt "tests/script-failure.lisp")))
                                  :error-output t
                                  :output t))
  (dolist (script (uiop:directory-files (asdf:system-relative-pathname :nyxt "examples/scripts")))
    (assert-false (uiop:run-program (list *executable*
                                          "--script"
                                          (uiop:native-namestring script))
                                    :error-output t
                                    :output t))))
