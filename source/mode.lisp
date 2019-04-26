;;; mode.lisp --- definition of major-mode base class
;;; all major modes inherit from the mode class

(in-package :next)

(defmacro define-mode (name direct-superclasses docstring direct-slots &body body)
  `(progn
     (defclass ,name ,(unless (eq (car direct-superclasses) t)
                        (cons 'fundamental-mode direct-superclasses))
       ,direct-slots
       (:documentation ,docstring))
     (defmethod initialize-instance :after ((%mode ,name) &key)
       ,@body)
     (defun ,name ()
       ,docstring
       (make-instance ',name
                      :name (format nil "~a-mode" ',name)))))

(define-mode fundamental-mode (t)
    "The root of all modes."
    ((name :accessor name :initarg :name)
     (keymap :accessor keymap :initarg :keymap :initform (make-keymap))
     (buffer :accessor buffer)))

(defmethod setup ((mode fundamental-mode) (buffer buffer))
  (setf (buffer mode) buffer))

(defmethod did-commit-navigation ((mode fundamental-mode) url)
  url)

(defmethod did-finish-navigation ((mode fundamental-mode) url)
  url)
