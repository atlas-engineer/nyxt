;;;; tree-mode.lisp --- tree representation for usage in traversable history
;;;; nodes have access to parent and children nodes, tree is non-binary

(in-package :next)

(defstruct node
  parent
  children
  data)

;; Breadth-First-Search(Graph, root):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; add to queue, add children to model
;;
;;     create empty set S
;;     create empty queue Q
;;
;;     add root to S
;;     Q.enqueue(root)
;;
;;     while Q is not empty:
;;         current = Q.dequeue()
;;         if current is the goal:
;;             return current
;;         for each node n that is adjacent to current:
;;             if n is not in S:
;;                 add n to S
;;                 Q.enqueue(n)


(defvar tree-history-mode-map (make-hash-table :test 'equalp))

(defvar tree-history-mode-hook nil)
(defclass tree-history-mode (mode) ())

(defun tree-history-mode ()
  "Base mode for representing the history of a buffer"
  (let ((mode 
	 (make-instance 'tree-history-mode
			:name "Tree-Mode"
			:keymap tree-history-mode-map
			:view (qnew "QTreeView"))))
    ;; return instance of mode
    mode))
