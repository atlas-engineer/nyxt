;;; mode.lisp --- definition of major-mode base class
;;; all major modes inherit from the mode class

(in-package :next)

(defclass mode ()
    ((name :accessor name :initarg :name)
     (keymap :accessor keymap :initarg :keymap)
     (buffer :accessor buffer)))

(defmethod setup ((mode mode) buffer)
  (setf (buffer mode) buffer))
