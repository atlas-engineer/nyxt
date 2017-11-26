;;; utility.lisp --- utility classes and functions

(in-package :next)

;; data node used to represent tree history
(defstruct node
  parent
  children
  data)

;; class used to represent a completion within the minibuffer
;; the display text is the text displayed and filtered in the minibuffer
;; the object represents the actual object behind the completion
(defclass completion ()
  ((display-text :accessor display-text)))

