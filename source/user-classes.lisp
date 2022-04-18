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

(defgeneric customize-instance (object)
  (:method ((class t)) t)
  (:documentation "Specialize this method to customize the default values and
behaviour of some CLASS instance.

This method is run after the instance has been initialized (in particular, after
the `initialize-instance' :after method).

This is meant to be used by users only.  Don't use it in public code, prefer
`initialize-instance :after' instead."))

(defmethod make-instance :around ((class user-class) &key)
  (sera:lret ((initialized-object (call-next-method)))
    (customize-instance initialized-object)))

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
