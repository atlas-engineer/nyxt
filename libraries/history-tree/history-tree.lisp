;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :history-tree)

(define-class node ()
  ((parent nil
           :type (or null node))
   (children '()
             :documentation "List of nodes.")
   (bindings (make-hash-table)
             :documentation "The key is an owner, the value is a
`binding'.  This slot also allows us to know to which owner a node belongs.")
   (data nil
         :type t
         :documentation "Arbitrary data carried by the node."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "Internal node of the history tree."))

(defun make-node (&key data parent)
  (make-instance 'node :data data :parent parent))

(define-class binding ()
  ((parent-owner nil
                 :type t
                 :documentation "The owner in the parent node.
Unless the parent was disowned by this `parent-owner',

  (gethash PARENT-OWNER (bindings (parent NODE)))

should return non-nil.")
   (forward-child  nil
                  :type (or null node)
                  :documentation "Which of the `children' (in a `node') is the
child to go forward to for this owner.")
   (last-access (local-time:now)
                :type (or local-time:timestamp string) ; Support `string' for easier deserialization.
                :documentation "Timestamp of the last access to this node by the
owner."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "The relation ship between an owner and the current node."))

(define-class owner-header ()
  ((origin nil             ; TODO: Rename to `root'?  Not to be confused with the htree root, but maybe it's convenient to have the same method name.
           :type (or null node)
           :documentation "The first node created for this owner.")
   (current nil
            :type (or null node)
            :initform nil
            :documentation "The current node.
It changes every time a node is added or deleted.")
   (nodes (make-hash-table)
          :type hash-table
          :documentation "The set of all unique nodes visited by an owner."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "The high-level information about an owner."))

(defmethod (setf current) (value (header owner-header))
  (if value
      (setf (slot-value header 'current) value)
      (error "Attempted to set current node to NIL for owner header ~a." header)))

(define-class history-tree ()
  ((root nil
         :type (or null node)
         :documentation "The root node.
It only changes when deleted.")
   (owners (make-hash-table)
           :type hash-table
           :documentation "The key is an owner, the value is an `owner-header'.")
   (current-owner-header nil
                         :type t
                         :documentation "Must be one of the `owners' values."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "Staring point of the global history tree data structure."))

(defun make ()
  (make-instance 'history-tree))

(declaim (ftype (function (history-tree t) owner-header) set-current-owner))
(defun set-current-owner (history owner)
  (let ((header (gethash owner (owners history))))
    (unless header
      (setf (gethash owner (owners history)) (make-instance 'owner-header)))
    (setf (current-owner-header history)
          (gethash owner (owners history)))))

(declaim (ftype (function (history-tree) node) current-owner-node))
(defun current-owner-node (history)
  (and (current-owner-header history)
       (current (current-owner-header history))))

(deftype positive-integer ()            ; TODO: Remove if unused.
  `(integer 1 ,most-positive-fixnum))

(deftype non-negative-integer ()        ; TODO: Remove if unused.
  `(integer 0 ,most-positive-fixnum))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'back))
;; TODO: Can we set ftype for methods return value?
;; (declaim (ftype (function (history-tree &optional positive-integer))
;;                 back))
(defmethod back ((history history-tree) &optional (count 1))
  "Go COUNT parent up from the current node.
Return (VALUES HISTORY (CURRENT HISTORY)) so that `back' and `forward' calls can
be chained."
  (when (and (current history)
             (parent (current history)))
    ;; Put former current node back in first position if it is not already
    ;; there, e.g. if current node was set manually.
    (let ((former-current (current history)))
      (setf (current history) (parent (current history)))
      (setf (children (current history))
            (cons former-current
                  (delete former-current (children (current history))))))
    (when (< 1 count)
      (back history (1- count))))
  (values history (current history)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'forward))
;; (declaim (ftype (function (history-tree &optional positive-integer))
;;                 forward))
(defmethod forward ((history history-tree) &optional (count 1))
  "Go COUNT first-children down from the current node.
Return (VALUES HISTORY (CURRENT HISTORY)) so that `back', `forward', and
`go-to-child' calls can be chained."
  (when (and (current history)
             (children (current history)))
    (setf (current history) (first (children (current history))))
    (when (< 1 count)
      (forward history (1- count))))
  (values history (current history)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'go-to-child))
(defmethod go-to-child (data (history history-tree) &key (test #'equal))
  "Go to direct current node's child matching DATA.
Test is done with the TEST argument.
Return (VALUES HISTORY (CURRENT HISTORY)) so that `back', `forward', and
`go-to-child' calls can be chained."
  (when (current history)
    (let ((selected-child))
      (setf (children (current history))
            (delete-if (lambda (node)
                         (when (funcall test (data node) data)
                           (setf selected-child node)))
                       (children (current history))))
      (when selected-child
        (push selected-child (children (current history)))
        (forward history))))
  (values history (current history)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'delete-child))
(defmethod delete-child (data (history history-tree) &key (test #'equal))
  "Delete child matching DATA and return the child.
Test is done with the TEST argument."
  (when (current history)
    (let ((matching-node nil))
      (setf (children (current history))
            (delete-if (lambda (node)
                         (when (funcall test (data node) data)
                           (setf matching-node node)))
                       (children (current history))))
      matching-node)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'add-child))
(defmethod add-child (data (history history-tree) &key (test #'equal))
  "Create or find a node holding DATA and set current node to it.
Return (possibly new) current node.

If current node matches DATA (according to TEST), then we update its data to
DATA (since the TEST function does not necessarily mean the data is identical).

If DATA is found among the children (according to TEST), the child is moved
first among the children, its data is set to DATA and the current node is set to
this child.

If there is no current element, this creates the first element of the tree."
  (cond
    ((null (current history))
     (let ((new-node (make-node :data data)))
       (setf (root history) new-node)
       (setf (current history) (root history))))
    ((not (funcall test data (data (current history))))
     (let ((node (delete-child data history :test test)))
       (push (or (when node
                   (setf (data node) data)
                   node)
                 (make-node :data data :parent (current history)))
             (children (current history)))
       (forward history)))
    (t
     (setf (data (current history)) data)))
  (current history))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'add-children))
(defmethod add-children (children-data (history history-tree) &key (test #'equal))
  "Add CHILDREN-DATA to the HISTORY `current''s node.
Each child is added with `add-child'.
Return the (maybe new) current node, which holds the last piece of data in
`children-data'."
  (add-child (first children-data) history :test test)
  (if (rest children-data)
      (add-children (rest children-data) (back history) :test test)
      (current history)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'map-tree))
(defun map-tree (function tree &key flatten include-root (collect-function #'cons))
  "Map the FUNCTION over the TREE.
If TREE is a `htree:history-tree', start from it's root.
If TREE is a `htree:node', start from it.
Include results of applying FUNCTION over ROOT if INCLUDE-ROOT is
non-nil.
Return results as cons cells tree if FLATTEN is nil and as a flat
list otherwise.
COLLECT-FUNCTION is the function of two arguments that glues the
current node to the result of further traversal."
  (labels ((collect (node children)
             (funcall collect-function node children))
           (traverse (node)
             (when node
               (collect (funcall function node)
                 ;; This lambda here because (apply #'identity ...) fails on empty arglist.
                 (apply (if flatten #'append #'(lambda (&rest args) args))
                        (mapcar #'traverse (children node)))))))
    (let ((root (typecase tree
                  (htree:node tree)
                  (htree:history-tree (htree:root tree)))))
      (when root
        (if include-root
            (traverse root)
            (apply #'append (mapcar #'traverse (children root))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'do-tree))
(defmacro do-tree ((var tree) &body body)
  "Apply actions in BODY to all the nodes in a tree.
Nodes are bound to VAR.
If TREE is a node, it's passed right away,
if it is a tree, then the root is taken.

Always return nil, as it is an explicitly imperative macro."
  `(progn
     (map-tree (lambda (,var) ,@body) ,tree :include-root t)
     ;; Explicitly return nil
     nil))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'node-children))
(defun node-children (node)
  "Return a list of all the children of the NODE, recursively."
  (map-tree #'identity node :flatten t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'node-parents))
(defun node-parents (node)
  "Return a list of parents of NODE, recursively.
First parent comes first in the resulting list."
  (when node
    (cons node
          (node-parents (parent node)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'node-forward-children))
(defun node-forward-children (node)
  "Return a list of the first children of NODE, recursively.
First child comes first in the resulting list."
  (when node
    (cons node
          (node-forward-children (first (children node))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'parent-nodes))
(defmethod parent-nodes ((history history-tree))
  "Return a list of all parents of the current node.
First parent comes first in the resulting list."
  (when (current history)
    (node-parents (parent (current history)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'forward-children-nodes))
(defmethod forward-children-nodes ((history history-tree))
  "Return a list of the first children of the current node, recursively.
First child comes first in the resulting list."
  (when (current history)
    (node-forward-children (first (children (current history))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'children-nodes))
(defmethod children-nodes ((history history-tree))
  "Return a list of all the children of the current node.
The nodes come in depth-first order."
  (when (current history)
    (node-children (current history))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'all-nodes))
(defmethod all-nodes ((history history-tree))
  "Return a list of all nodes, in depth-first order."
  (let ((root (root history)))
    (when root
      (cons root (node-children root)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'all-nodes-data))
(defmethod all-nodes-data ((history history-tree))
  "Return a list of all nodes data, in depth-first order."
  (mapcar #'data (all-nodes history)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'parent-nodes-data))
(defmethod parent-nodes-data ((history history-tree))
  "Return a list of all nodes data.
First parent comes first."
  (mapcar #'data (parent-nodes history)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'forward-children-nodes-data))
(defmethod forward-children-nodes-data ((history history-tree))
  "Return a list of all forward children nodes data.
First child comes first."
  (mapcar #'data (forward-children-nodes history)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'children-nodes-data))
(defmethod children-nodes-data ((history history-tree))
  "Return a list of all children nodes data, in depth-first order."
  (mapcar #'data (children-nodes history)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'find-node))
(defun find-node (item tree &key (key #'identity) (test #'equal))
  "Find a tree node matching ITEM (by TEST) in TREE and return it.
TREE can be a `history' or a `node'. "
  (block search
    (do-tree (node tree)
      (when (funcall test item (funcall key node))
        (return-from search node)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'remove-node))
(defun remove-node (item tree &key (key #'identity) (test #'equal))
  "Return all the nodes from TREE that didn't match ITEM (measured by TEST).
TREE can be a `history' or a `node'."
  (let (result)
    (do-tree (node tree)
      (unless (funcall test item (funcall key node))
        (push node result)))
    result))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'find-data))
(defmethod find-data (data (history history-tree) &key (test #'equal) ensure-p)
  "Find a tree node matching DATA in HISTORY and return it.
If ENSURE-P is non-nil, create this node when not found.
Search is done with the help of TEST argument."
  (let ((match (find-node data history :test test :key #'data)))
    ;; TODO: `add-child' changes `current'. Always change `current' on match?
    (or match (when ensure-p (add-child data history :test test)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'delete-data))
(defmethod delete-data (data (history history-tree) &key (test #'equal) rebind-children-p)
  "Delete node(s) matching DATA from HISTORY and return the last deleted node.
If the node has children itself, and REBIND-CHILDREN-P is not nil, these
will become children of the node's parent. Search is done with the
help of TEST argument."
  (let ((last-deleted nil))
    (do-tree (node history)
      (when (funcall test data (data node))
        (setf (children (parent node)) (append (when rebind-children-p (children node))
                                               (remove node (children (parent node))))
              last-deleted node)))
    last-deleted))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'depth))
;; (declaim (ftype (function (history-tree) non-negative-integer)
;;                 depth))
(defmethod depth ((history history-tree))
  "Return the number of parents of the current node."
  (length (parent-nodes history)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'size))
;; (declaim (ftype (function (history-tree) non-negative-integer)
;;                 size))
(defmethod size ((history history-tree))
  "Return the number of nodes."
  ;; TODO: This could be optimized with a SIZE slot, but is it worth it?
  (length (all-nodes history)))
