;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package analysis/tests
  (:use #:common-lisp #:lisp-unit2 :analysis))
(in-package :analysis/tests)

(define-test tomato-test ()
  (lisp-unit2:assert-equal "tomato" "tomato"))
