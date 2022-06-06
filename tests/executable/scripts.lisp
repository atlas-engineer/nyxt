;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)
(use-nyxt-package-nicknames)

(plan nil)

(defvar *executable* (asdf:system-relative-pathname :nyxt "nyxt"))

(subtest "Scripts"
  (prove:is-error (uiop:run-program (list *executable*
                                          "--script" (uiop:native-namestring
                                                      (asdf:system-relative-pathname :nyxt "tests/script-failure.lisp")))
                                    :error-output t
                                    :output t)
                  'uiop/run-program:subprocess-error)
  (dolist (script (uiop:directory-files (asdf:system-relative-pathname :nyxt "examples/scripts")))
    (prove:is (uiop:run-program (list *executable* "--script" (uiop:native-namestring script))
                                :error-output t
                                :output t)
              nil)))

(finalize)
