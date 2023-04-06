;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defclass user-mixin-class ()
  ((customize-hook :initform (make-instance 'hooks:hook-any)
                   :accessor customize-hook
                   :documentation "An internal hook to add customization handlers to.

Reserved for `define-configuration'.

Prefer `define-configuration' and `customize-instance' instead."))
  (:documentation "Classes using this metaclass will call `customize-instance'
on instantiation.
This is useful to expose a class configuration knob to the user.

This class is also for portable configuration management. Imagine some
implementation has structs totally separate from classes, and thus configuring
structs may be possible, but would require adding yet another class. Say,
user-structure-class. With user-mixin-class, that would be a one-liner:

(defclass user-structure-class (impl:built-in-structure-class user-mixin-class) ())

while having the customize-hook and other configuration functionality in 3+
classes (user-class, user-funcallable-class, and user-structure-class in case of
this imaginary implementation) would already be 3x+ code duplication..."))

(defclass user-class (standard-class user-mixin-class)
  ()
  (:documentation "User-configurable value class.
Can be configured using `customize-instance' and `customize-hook'."))
(export-always 'user-class)

(defmethod closer-mop:validate-superclass ((class user-class) (superclass standard-class)) t)
(defmethod closer-mop:validate-superclass ((superclass standard-class) (class user-class)) t)
(defmethod closer-mop:validate-superclass ((class user-class) (superclass user-mixin-class)) t)
(defmethod closer-mop:validate-superclass ((superclass user-mixin-class) (class user-class)) t)

(defclass user-funcallable-class (closer-mop:funcallable-standard-class user-mixin-class)
  ()
  (:documentation "User-configurable class that behaves like function.
One can use `funcall' on it, thus funcallable.
Can be configured using `customize-instance' and `customize-hook'."))
(export-always 'user-funcallable-class)

(defmethod closer-mop:validate-superclass ((class user-funcallable-class) (superclass closer-mop:funcallable-standard-class)) t)
(defmethod closer-mop:validate-superclass ((superclass closer-mop:funcallable-standard-class) (class user-funcallable-class)) t)
(defmethod closer-mop:validate-superclass ((class user-funcallable-class) (superclass user-mixin-class)) t)
(defmethod closer-mop:validate-superclass ((superclass user-mixin-class) (class user-funcallable-class)) t)

(export-always 'customize-instance)
(defgeneric customize-instance (object &key &allow-other-keys)
  (:method ((class t) &key) t)
  (:documentation "Specialize this method to customize the default values and
behavior of some CLASS instance.

This method is run after the instance has been initialized (in particular, after
the `initialize-instance' :after method).

The standard method is reserved for user configuration.

Do not specialize the standard method in public code, prefer
`initialize-instance :after' instead to initialize slots, and
`customize-instance :after' for code that relies on finalized slot values."))

(defmethod #+nyxt-debug-make-instance cl:make-instance #-nyxt-debug-make-instance make-instance
  :around ((class user-mixin-class) &rest initargs &key &allow-other-keys)
  (sera:lret ((initialized-object (call-next-method)))
    (mapcar (lambda (class)
              (hooks:run-hook (slot-value class 'customize-hook) initialized-object))
            (sera:filter #'user-class-p (cons class (mopu:superclasses class))))
    (apply #'customize-instance initialized-object initargs)))

(defun user-class-p (class-specifier)
  (let ((metaclass (cond
                     ((symbolp  class-specifier)
                      (find-class class-specifier))
                     ((closer-mop:classp class-specifier)
                      class-specifier)
                     (t (class-of class-specifier)))))
    (or (typep metaclass 'user-class)
        (typep metaclass 'user-funcallable-class))))

(export-always 'coerce-slot)
(defgeneric coerce-slot (new-value instance slot-type)
  (:method ((new-value t) (instance t) (slot-type t))
    (declare (ignore instance slot-type))
    new-value)
  (:documentation "Coerce NEW-VALUE to a return value that should satisfy SLOT-TYPE.
This applies to all slots of INSTANCE.
To coerce the value for a specific slot, see `c2mop:slot-value-using-class'."))

(defmethod coerce-slot (new-value instance (slot-type (eql 'local-time:timestamp)))
  (declare (ignorable instance slot-type))
  (if (typep new-value 'local-time:timestamp)
      new-value
      (local-time:parse-timestring new-value)))

(defmethod coerce-slot (new-value instance (slot-type (eql 'package)))
  (declare (ignorable instance slot-type))
  (if (typep new-value 'package)
      new-value
      (find-package new-value)))

(defmethod (setf c2mop:slot-value-using-class) :around (new-value
                                                        (class user-class)
                                                        instance
                                                        slot)
  (call-next-method (coerce-slot new-value instance (c2mop:slot-definition-type slot))
                    class instance slot))

(defclass interface-class (standard-class) ()
  (:documentation "An interface class exists solely for the purpose of
dereferencing other classes through its superclasses.
It cannot have direct slots.

This is useful when you do not know in advance which classes you need.

Example:

In some early file:

(defclass renderer-browser () ()
  (:metaclass interface-class))

In a later file, when you've defined `gtk-browser':

\(handler-bind ((warning #'muffle-warning))
  (defclass renderer-browser (gtk-browser)
    ()
    (:metaclass interface-class)))"))
(export-always 'interface-class)
;; TODO: Is there a way to customize the metaclass so that redefinitions do not
;; trigger a warning?

(defmethod closer-mop:validate-superclass ((class interface-class)
                                           (superclass standard-class))
  t)
(defmethod closer-mop:validate-superclass ((superclass standard-class)
                                           (class interface-class))
  t)
(defmethod closer-mop:validate-superclass ((class interface-class)
                                           (superclass user-class))
  t)
(defmethod closer-mop:validate-superclass ((superclass user-class)
                                           (class interface-class))
  t)

(defmethod initialize-instance :after ((class interface-class) &key)
  (when (closer-mop:class-direct-slots class)
    (error "Interface class cannot have direct slots."))
  class)
(defmethod reinitialize-instance :after ((class interface-class) &key)
  (when (closer-mop:class-direct-slots class)
    (error "Interface class cannot have direct slots."))
  class)

(define-method-combination cascade ()
  ((before (:before))
   (around (:around))
   (after (:after))
   (primary ()))
  "Cascade upwards in the hierarchy from the child to all parent methods.
Allows for composed object constructors/destructors, for instance."
  ;; TODO: Allow cascading down to children instead?
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (if (and (null before)
             (null around)
             (null after)
             (sera:single primary))
        `(call-method ,(first primary))
        (let ((form `(prog1
                         (progn
                           ,@(call-methods before)
                           ,@(call-methods primary))
                       ,@(call-methods (reverse after)))))
          (if around
              `(call-method ,(first around)
                            (,@(rest around)
                             (make-method ,form)))
              form)))))
