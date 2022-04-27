;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defclass user-class (standard-class) ())
(export-always 'user-class)

(defmethod closer-mop:validate-superclass ((class user-class)
                                           (superclass standard-class))
  t)

(defmethod closer-mop:validate-superclass ((superclass standard-class)
                                           (class user-class))
  t)

(defvar *groups* (make-hash-table))

(macrolet ((define-group-predicate (number)
             (let ((function-name (intern (format nil "MATCH-INTO-GROUP-~d" number))))
               `(defun ,function-name (qualifier)
                  (unless (member (first qualifier) (gethash ,number *groups*) :test #'equal)
                    (format t "Pushing ~s into group ~d~&" (first qualifier) ,number)
                    (push (first qualifier) (gethash ,number *groups*))
                    t)))))
  (define-group-predicate 0)
  (define-group-predicate 1)
  (define-group-predicate 2)
  (define-group-predicate 3)
  (define-group-predicate 4)
  (define-group-predicate 5)
  (define-group-predicate 6)
  (define-group-predicate 7)
  (define-group-predicate 8)
  (define-group-predicate 9))

(define-method-combination hookable ()
  ((before (:before))
   (around (:around))
   (after (:after))
   (primary ())
   (group-0 match-into-group-0)
   (group-1 match-into-group-1)
   (group-2 match-into-group-2)
   (group-3 match-into-group-3)
   (group-4 match-into-group-4)
   (group-5 match-into-group-5)
   (group-6 match-into-group-6)
   (group-7 match-into-group-7)
   (group-8 match-into-group-8)
   (group-9 match-into-group-9))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (clrhash *groups*)
    (let ((form `(prog1
                     (progn
                       ,@(call-methods
                          (append
                           before
                           group-0 group-1 group-2 group-3 group-4
                           group-5 group-6 group-7 group-8 group-9
                           primary)))
                   ,@(call-methods (reverse after)))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around)
                         (make-method ,form)))
          form))))

(export-always 'customize-instance)
(defgeneric customize-instance (object &key &allow-other-keys)
  (:method-combination hookable)
  (:method ((class t) &key) t)
  (:documentation "Specialize this method to customize the default values and
behaviour of some CLASS instance.

This method is run after the instance has been initialized (in particular, after
the `initialize-instance' :after method).

The standard method is meant to be used by users only.

Don't use it in public code, prefer `initialize-instance :after' instead to
initialize slots, and `customize-instance :after' for code that relies on
finalized slot values."))

(defmethod make-instance :around ((class user-class) &rest initargs &key &allow-other-keys)
  (sera:lret ((initialized-object (call-next-method)))
    (apply #'customize-instance initialized-object initargs)))

(defun user-class-p (class-specifier)
  (let ((metaclass (cond
                     ((symbolp  class-specifier)
                      (find-class class-specifier))
                     ((closer-mop:classp class-specifier)
                      class-specifier)
                     (t (class-of class-specifier)))))
    (typep metaclass 'user-class)))

(defclass mixin-class (standard-class) ())
(export-always 'mixin-class)

(defmethod closer-mop:validate-superclass ((class mixin-class)
                                           (superclass standard-class))
  t)
(defmethod closer-mop:validate-superclass ((superclass standard-class)
                                           (class mixin-class))
  t)
(defmethod closer-mop:validate-superclass ((class mixin-class)
                                           (superclass user-class))
  t)
(defmethod closer-mop:validate-superclass ((superclass user-class)
                                           (class mixin-class))
  t)

(defmethod initialize-instance :after ((class mixin-class) &key)
  (when (closer-mop:class-direct-slots class)
    (error "Mixin class cannot have slots."))
  class)
