;;;; mode.lisp --- definition of major-mode base class
;;;; all major modes inherit from the mode class

(in-package :next)

(defclass mode ()
    ((name :accessor mode-name :initarg :name)
     (keymap :accessor mode-keymap :initarg :keymap)
     (view :accessor mode-view :initarg :view)))
