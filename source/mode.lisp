;;; mode.lisp --- Definition of the mode class and root-mode.
;;; All modes inherit from the root-mode.

(in-package :next)

(defmacro define-mode (name direct-superclasses docstring direct-slots &body body)
  "Define mode NAME.
When DIRECT-SUPERCLASSES is T, then the mode has no parents.
Otherwise, the mode's parents are ROOT-MODE and DIRECT-SUPERCLASSES.
BODY is executed during instantiation.
In the BODY, the %MODE variable refers to the mode being instantiated.
A constructor named NAME is also defined."
  `(progn
     (defclass ,name ,(unless (eq (car direct-superclasses) t)
                        (cons 'root-mode direct-superclasses))
       ,direct-slots
       (:documentation ,docstring))
     (defmethod initialize-instance :after ((%mode ,name) &key)
       ,@body)
     (defun ,name ()
       ,docstring
       (make-instance ',name
                      :name (format nil "~a-mode" ',name)))))

(define-mode root-mode (t)
    "The root of all modes."
    ((name :accessor name :initarg :name) ;; TODO: What's the use of mode's NAME slot?
     (keymap :accessor keymap :initarg :keymap :initform (make-keymap))
     (buffer :accessor buffer)))

(defmethod setup ((mode root-mode) (buffer buffer))
  (setf (buffer mode) buffer))

(defmethod did-commit-navigation ((mode root-mode) url)
  url)

(defmethod did-finish-navigation ((mode root-mode) url)
  url)
