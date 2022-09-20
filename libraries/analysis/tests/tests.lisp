;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package analysis/tests
  (:use #:common-lisp #:lisp-unit2 :analysis))
(in-package :analysis/tests)

(define-test test-single-length ()
  (let ((model (make-instance 'analysis::sequence-model)))
    (analysis::add-record model '(1 2))
    (lisp-unit2:assert-equal (analysis::element (analysis::predict model '(1))) 2)))
