;;;; tree.lisp --- tree representation for usage in traversable history
;;;; nodes have access to parent and children nodes, tree is non-binary

(in-package :next)

(defstruct node
  parent
  children
  data)
