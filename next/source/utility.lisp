;;; utility.lisp --- utility classes and functions

(in-package :next)

;; data node used to represent tree history
(defstruct node
  parent
  children
  data)
