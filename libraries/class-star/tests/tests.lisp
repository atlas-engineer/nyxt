;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package class-star/tests
  (:use #:common-lisp #:prove)
  (:import-from #:class-star))
(in-package :class-star/tests)

(plan nil)

(subtest "Simple class"
  (is (progn
              (class-star:define-class foo ()
                ((name "fooname")))
              (let ((foo (make-instance 'foo)))
                (name-of foo)))
            "fooname"))

(subtest "Simple class with custom accessors"
  (class-star:define-class bar ()
    ((name "fooname")
     (age :accessor this-age)
     (address :accessor nil))
    (:accessor-name-transformer (lambda (name def) (declare (ignore def)) name)))
  (make-instance 'bar)
  (ok (fboundp 'name))
  (ok (fboundp 'this-age))
  (is (fboundp 'address) nil))

(subtest "Simple class default value"
  (is (progn
              (class-star:define-class foo-default ()
                ((name :type string)
                 (age :type number)))
              (let ((foo (make-instance 'foo-default)))
                (name-of foo)))
            ""))

;; TODO: Fix following test and try to make it portable.
#+nil
(subtest "No initarg"
  (is-error (let ((hu.dwim.defclass-star:*automatic-initargs-p* nil))
                    (class-star:define-class foo-no-initarg ()
                      ((name :type string)))
                    (make-instance 'foo-no-initarg :name "bar"))
                  'sb-pcl::initarg-error))

(subtest "No accessor"
  (is (progn
              (class-star:define-class foo-no-accessors ()
                ((name-no-acc :type string))
                (:automatic-accessors-p nil))
              (make-instance 'foo-no-accessors)
              (fboundp 'name-no-acc-of))
            nil))

(subtest "Initform inference"
  (class-star:define-class foo-initform-infer ()
    ((name :type string)))
  (is (name-of (make-instance 'foo-initform-infer))
            "")
  (class-star:define-class foo-initform-infer-no-unbound ()
    ((name :type function))
    (:initform-inference 'class-star:no-unbound-initform-inference))
  (is-error (make-instance 'foo-initform-infer-no-unbound)
                  'simple-error)
  (class-star:define-class foo-initform-infer-nil-fallback ()
    ((name :type (or function null)))
    (:initform-inference 'class-star:nil-fallback-initform-inference))
  (is (name-of (make-instance 'foo-initform-infer-nil-fallback))
            nil))

(defvar street-name "bar")
(subtest "Type inference"
  (class-star:define-class foo-type-infer ()
    ((name "foo")
     (nickname street-name)
     (age 1)
     (height 2.0)
     (width 2 :type number)
     (lisper nil)
     (empty-list '())
     (nonempty-list '(1 2 3))
     (mark :foo)
     (sym 'sims)
     (fun #'list)
  (is (getf (mopu:slot-properties 'foo-type-infer 'name) :type)
            'string)
  (is (getf (mopu:slot-properties 'foo-type-infer 'nickname) :type)
            'string)
  (is (getf (mopu:slot-properties 'foo-type-infer 'age) :type)
            'integer)
  (is (getf (mopu:slot-properties 'foo-type-infer 'height) :type)
            'number)
  (is (getf (mopu:slot-properties 'foo-type-infer 'width) :type)
            'number)
  (is (getf (mopu:slot-properties 'foo-type-infer 'lisper) :type)
            'boolean)
  (is (getf (mopu:slot-properties 'foo-type-infer 'empty-list) :type)
            'list)
  (is (getf (mopu:slot-properties 'foo-type-infer 'nonempty-list) :type)
            'list)
  (is (getf (mopu:slot-properties 'foo-type-infer 'sym) :type)
            'symbol)
  (is (getf (mopu:slot-properties 'foo-type-infer 'fun) :type)
            'function)
  (is (getf (mopu:slot-properties 'foo-type-infer 'composite) :type)
            nil))

(finalize)
     (composite (error "Should not eval, type should not be inferred"))))
