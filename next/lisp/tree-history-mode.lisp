;;;; tree-mode.lisp --- tree representation for usage in traversable history
;;;; nodes have access to parent and children nodes, tree is non-binary

(in-package :next)

(defstruct node
  parent
  children
  data
  qitem)

(defvar tree-history-mode-map (make-hash-table :test 'equalp))

(defclass tree-history-mode (mode) ())

(defvar *history-tree* nil
  "A variable to store the singleton history tree")
(defparameter *tree-history-model* (qnew "QStandardItemModel")
  "A variable to store the model which updates the QTreeView")

(defun update-tree-model (root-node)
  (let ((queue (queues:make-queue :simple-queue)))
    (queues:qpush queue root-node)
    (setf (node-qitem root-node) *tree-history-model*)
    (loop while (>= (queues:qsize queue) 1) do
	 (let* ((node (queues:qpop queue)))
	   (when (not (node-qitem node))
	     (setf (node-qitem node) (qnew "QStandardItem(QString)" (node-data node))))
	   (loop for child in (node-children node) do
		(queues:qpush queue child)
		(when (not (node-qitem child))
		  (setf (node-qitem child) (qnew "QStandardItem(QString)" (node-data child))))
		(|appendRow| (node-qitem node) (node-qitem child))
		(print (node-data child)))))))

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
