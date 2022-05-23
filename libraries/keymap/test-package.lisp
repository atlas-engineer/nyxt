;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package keymap/tests
  (:use #:common-lisp)
  (:import-from #:keymap))

(in-package :keymap/tests)

(defmacro keymap-test (name &body body)
  `(prove:subtest ,name
     (let ((keymap:*default-bound-type* '(or keymap:keymap t)))
       ,@body)))
