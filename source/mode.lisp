;;; mode.lisp --- definition of major-mode base class
;;; all major modes inherit from the mode class

(in-package :next)

(defmacro define-mode (name direct-superclasses direct-slots &body body)
  `(progn
     (defclass ,name ,direct-superclasses ,direct-slots)
     (push (lambda () ,@body) *deferred-mode-initializations*)))

(define-mode mode ()
    ((name :accessor name :initarg :name)
     (keymap :accessor keymap :initarg :keymap)
     (buffer :accessor buffer)))

(defmethod setup ((mode mode) (buffer buffer))
  (setf (buffer mode) buffer))

(defmethod did-commit-navigation ((mode mode) url)
  url)

(defmethod did-finish-navigation ((mode mode) url)
  url)
