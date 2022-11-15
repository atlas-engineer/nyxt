;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package class-star/tests
  (:use #:common-lisp #:lisp-unit2)
  (:import-from #:class-star))
(in-package :class-star/tests)

(define-test simple-class ()
  (assert-string= "fooname"
                  (progn
                    (class-star:define-class foo ()
                      ((name "fooname")))
                    (let ((foo (make-instance 'foo)))
                      (name-of foo)))))

(define-test simple-class-with-custom-accessors ()
  (class-star:define-class bar ()
    ((name "fooname")
     (age :accessor this-age)
     (address :accessor nil))
    (:accessor-name-transformer (lambda (name def) (declare (ignore def)) name)))
  (make-instance 'bar)
  (assert-no-error t (fboundp 'name))
  (assert-no-error t (fboundp 'this-age))
  (assert-false (fboundp 'address)))

(define-test simple-class-default-value ()
  (assert-string= ""
                  (progn
                    (class-star:define-class foo-default ()
                      ((name :type string)
                       (age :type number)))
                    (let ((foo (make-instance 'foo-default)))
                      (name-of foo)))))

(define-test no-accessor ()
  (assert-false (progn
                  (class-star:define-class foo-no-accessors ()
                    ((name-no-acc :type string))
                    (:automatic-accessors-p nil))
                  (make-instance 'foo-no-accessors)
                  (fboundp 'name-no-acc-of))))

(define-test initform-inference ()
  (class-star:define-class foo-initform-infer ()
    ((name :type string)))
  (assert-string= ""
                  (name-of (make-instance 'foo-initform-infer)))
  (class-star:define-class foo-initform-infer-no-unbound ()
    ((name :type function))
    (:initform-inference 'class-star:no-unbound-initform-inference))
  (assert-error 'simple-error
                (make-instance 'foo-initform-infer-no-unbound))
  (class-star:define-class foo-initform-infer-nil-fallback ()
    ((name :type (or function null)))
    (:initform-inference 'class-star:nil-fallback-initform-inference))
  (assert-false (name-of (make-instance 'foo-initform-infer-nil-fallback))))

(defvar street-name "bar")
(define-test type-inference ()
  (class-star:define-class foo-type-infer ()
    ((name "foo")
     (nickname street-name)
     (age 1)
     (height 2.0)
     (width 2 :type number)
     (lisper t)
     (nil-is-not-bool nil)
     (empty-list '())
     (nonempty-list '(1 2 3))
     (mark :foo)
     (sym 'sims)
     (fun #'list)
     (composite (error "Should not eval, type should not be inferred"))))
  (assert-eq 'string
             (getf (mopu:slot-properties 'foo-type-infer 'name) :type))
  (assert-eq 'string
             (getf (mopu:slot-properties 'foo-type-infer 'nickname) :type))
  (assert-eq 'integer
             (getf (mopu:slot-properties 'foo-type-infer 'age) :type))
  (assert-eq 'number
             (getf (mopu:slot-properties 'foo-type-infer 'height) :type))
  (assert-eq 'number
             (getf (mopu:slot-properties 'foo-type-infer 'width) :type))
  (assert-eq 'boolean
             (getf (mopu:slot-properties 'foo-type-infer 'lisper) :type))
  (assert (not (eq 'boolean
                   (getf (mopu:slot-properties 'foo-type-infer 'nil-is-not-bool) :type))))
  (assert-eq 'list
             (getf (mopu:slot-properties 'foo-type-infer 'empty-list) :type))
  (assert-eq 'list
             (getf (mopu:slot-properties 'foo-type-infer 'nonempty-list) :type))
  (assert-eq 'symbol
             (getf (mopu:slot-properties 'foo-type-infer 'sym) :type))
  (assert-eq 'function
             (getf (mopu:slot-properties 'foo-type-infer 'fun) :type))
  (assert-eq nil
             (getf (mopu:slot-properties 'foo-type-infer 'composite) :type)))
