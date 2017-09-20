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


(defvar tree-history-mode-hook nil)
(defvar tree-history-mode-map (make-hash-table :test 'equalp))

(defclass tree-history-mode (mode) ())

(defvar *history-tree* nil
  "A variable to store the singleton history tree")
(defparameter *tree-history-model* (qnew "QStandardItemModel")
  "A variable to store the model which updates the QTreeView")

(defun update-tree-model (root-node)
  (let ((nodey (qnew "QStandardItem(QString)" "element")))
    (|appendRow| *tree-history-model* (qnew "QStandardItem(QString)" "element"))
    (|appendRow| *tree-history-model* (qnew "QStandardItem(QString)" "element"))
    (|appendRow| *tree-history-model* nodey)
    (|appendRow| nodey (qnew "QStandardItem(QString)" "element"))))

(defun tree-history-mode ()
  "Base mode for representing the history of a buffer"
  (let ((mode 
	 (make-instance 'tree-history-mode
			:name "Tree-Mode"
			:keymap tree-history-mode-map
			:view (qnew "QTreeView"))))
    (|setHorizontalHeaderLabels| *tree-history-model* '("Name"))
    (|setModel| (mode-view mode) *tree-history-model*)
    (|setUniformRowHeights| (mode-view mode) t)
    ;; return instance of mode
    mode))
