;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :history-tree)

;; TODO: Unexport most (all?) slot writers.

(defmacro export-always (symbols &optional (package nil package-supplied?)) ; From serapeum.
  "Like `export', but also evaluated at compile time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export ,symbols ,@(and package-supplied? (list package)))))

(define-class node ()
  ((parent nil
           :type (or null node))
   (children '()
             :documentation "List of nodes.")
   (bindings (make-hash-table)
             :documentation "The key is an `owner', the value is a
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
  ((forward-child  nil
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
  (:documentation "The relationship between an owner and the current node."))

(define-class owner ()
  ;; TODO: Add slot pointing to history an owner belongs to?
  ((origin nil ; TODO: Rename to `root'?  Not to be confused with the htree root, but maybe it's convenient to have the same method name.
           :type (or null node)
           :documentation "The first node created for this owner.")
   (creator nil
            :type t
            :documentation "The owner in `origin's parent node that created this owner.
Unless the parent was disowned by this `creator',

  (gethash CREATOR (bindings (origin OWNER)))

should return non-nil.")
   (current nil
            :type (or null node)
            :initform nil
            :documentation "The current node.
It's updated every time a node is visited.")
   (nodes (make-hash-table)
          :type hash-table
          :documentation "The set of all unique nodes visited by an owner."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "The high-level information about an owner."))

(defmethod (setf current) (value (owner owner))
  (if value
      (setf (slot-value owner 'current) value)
      (error "Attempted to set current node to NIL for owner ~a." owner)))

(declaim (ftype (function (owner) (or null (cons node))) owned-children))
(defun owned-children (owner)
  (remove-if (lambda (child)
               (not (gethash owner (bindings child))))
             (children owner)))

(defun owned-parent (owner)
  "Return OWNER's parent if it's owned, nil otherwise."
  (let ((parent (parent (current owner))))
    (when (gethash owner (bindings parent))
      parent)))

(declaim (ftype (function (owner) binding) current-binding))
(defun current-binding (owner)
  (and (current owner)
       (gethash owner (bindings (current owner)))))

(declaim (ftype (function (owner node) (or null binding)) owned-p))
(defun owned-p (owner node)
  (and (bindings node)
       (gethash owner (bindings node))))

(define-class history-tree ()
  ((root nil
         :type (or null node)
         :documentation "The root node.
It only changes when deleted.")
   (owners (make-hash-table)
           :type hash-table
           :documentation "The key is an owner identifier (an artitrary balue),
the value is an `owner'.")
   (current-owner nil
                  :type t
                  :documentation "Must be one of the `owners' values."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "Staring point of the global history tree data structure."))

(defun make ()
  (make-instance 'history-tree))

(export-always 'owner)
(defun owner (history owner-identifier)
  "Return the `owner' object identified by OWNER-IDENTIFIER in HISTORY."
  (gethash owner-identifier (owners history)))

(export-always 'set-current-owner)
(declaim (ftype (function (history-tree t) owner) set-current-owner))
(defun set-current-owner (history owner-identifier)
  "OWNER-IDENTIFIER is arbitrary data representing an `owner'."
  (let ((owner (owner history owner-identifier)))
    (unless owner
      (setf owner
            (setf (gethash owner-identifier (owners history)) (make-instance 'owner))))
    (setf (current-owner history) owner)))

(export-always 'current-owner-node)
(declaim (ftype (function (history-tree) node) current-owner-node))
(defun current-owner-node (history)
  (and (current-owner history)
       (current (current-owner history))))

;; TODO: Add `gethash*' to set default value when not found?

(defmethod visit ((owner owner) node)
  "Return (values HEADER NODE) so that calls to `visit' can be chained."
  (setf (gethash node (nodes owner)) t)
  (setf (owner current) node)
  (let ((binding (gethash owner (bindings node))))
    (if binding
        (setf (last-access binding) (local-time:now))
        (setf (gethash owner (bindings node))
              (make-instance 'binding))))
  ;; TODO: If node is among the children, should we set all the forward children
  ;; to ensure that calling `forward` from CURRENT would lead to NODE?
  ;; What if there is no path owned by OWNER?
  (values owner node))

(defmethod visit ((history history-tree) node)
  "Visit NODE with HISTORY's current owner.
Return (values HISTORY NODE) so that calls to `visit' can be chained."
  (values history
          (when (current-owner history)
            (nth-value 1 (back (current-owner history))))))

(deftype positive-integer ()
  `(integer 1 ,most-positive-fixnum))

(deftype non-negative-integer ()
  `(integer 0 ,most-positive-fixnum))

(export-always 'back)
(defmethod back ((owner owner) &optional (count 1))
  "Go COUNT parent up from the current OWNER node.
Return (values OWNER (current OWNER)) so that `back' and `forward' calls can
be chained."
  (check-type count 'positive-integer)
  (when (and (current owner)
             (parent (current history)))
    (let ((former-current (current owner)))
      (visit owner (parent (current owner)))
      ;; Put former current node back as forward-child if it is not already
      ;; the case, e.g. if current node was set manually.
      (setf (forward-child (current-binding owner))
            former-current))
    (when (< 1 count)
      (back owner (1- count))))
  (values owner (current owner)))

(defmethod back ((history history-tree) &optional (count 1))
  "Go COUNT parent up from the current owner node.
Return (VALUES HISTORY CURRENT-NODE) so that `back' and `forward' calls can
be chained."
  (when (current-owner history)
    (values history (nth-value 1 (back (current-owner history) count)))))

(export-always 'forward)
(defmethod forward ((owner owner) &optional (count 1))
  "Go COUNT first-children down from the current OWNER node.
Return (values OWNER (CURRENT OWNER)) so that `back' and `forward' calls can be
chained."
  (check-type count 'positive-integer)
  (when (and (current owner)
             (children (current owner)))
    (visit (forward-child (current-binding owner)))
    (when (< 1 count)
      (forward owner (1- count))))
  (values owner (current owner)))

(defmethod forward ((history history-tree) &optional (count 1))
  "Go COUNT first-children down from the current owner node.
Return (values HISTORY CURRENT-NODE)) so that `back' and `forward' calls can be
chained."
  (when (current-owner history)
    (values history (nth-value 1 (forward (current-owner history) count)))))

(declaim (ftype (function (t owner &key (:test function)) (or null node)) find-child))
(defun find-child (data owner &key (test #'equal)) ; TODO: Generalize?
  "Return the direct child node of OWNER which matches DATA.
Test is done with the TEST argument."
  (find data
        (children (current owner))
        :key #'data
        :test test))

(declaim (ftype (function (t owner &key (:test function)) (or null node))
                find-owned-child))
(defun find-owned-child (data owner &key (test #'equal)) ; TODO: Generalize?
  "Return the direct child node owned by OWNER which matches DATA.
Test is done with the TEST argument."
  (find data
        (owned-children owner)
        :key #'data
        :test test))

(export-always 'go-to-child)
(defmethod go-to-child (data (owner owner) &key (test #'equal)
                                             (child-finder #'find-child))
  "Go to current OWNER node's direct child matching DATA.
Test is done with the TEST argument.
Return (values OWNER (current OWNER))."
  (when (current owner)
    (let ((match (find-child data owner :test test)))
      (when match
        (visit owner match))))
  (values history (current history)))

(export-always 'go-to-owned-child)
(defmethod go-to-owned-child (data (owner owner) &key (test #'equal))
  "Go to current OWNER node's direct owned child matching DATA.
A child is owned if it has a binding with OWNER.
Test is done with the TEST argument.
Return (values OWNER (current OWNER))."
  (go-to-child data owner :test test :child-finder #'find-owned-child))

(defmethod go-to-child (data (history history-tree) &key (test #'equal))
  "Go to direct current node's child matching DATA.
Test is done with the TEST argument. "
  (when (current-owner history)
    (go-to-child data (current-owner history) :test test)))

(defmethod go-to-owned-child (data (history history-tree) &key (test #'equal))
  "Go to current node's direct owned child matching DATA.
A child is owned if it has a binding with current owner.
Test is done with the TEST argument.
Return (values OWNER (current OWNER))."
  (when (current-owner history)
    (go-to-owned-child data (current-owner history) :test test)))

;; (export-always 'delete-child)
;; (defmethod delete-child (data (history history-tree) &key (test #'equal))
;;   "Delete child matching DATA and return the child.
;; Test is done with the TEST argument."
;;   (when (current history)
;;     (let ((matching-node nil))
;;       (setf (children (current history))
;;             (delete-if (lambda (node)
;;                          (when (funcall test (data node) data)
;;                            (setf matching-node node)))
;;                        (children (current history))))
;;       matching-node)))

(export-always 'add-child)
(defmethod add-child (data (owner owner) &key (test #'equal) creator)
  "Create or find a node holding DATA and set current node to it.
Return the (possibly new) current node.

If current node matches DATA (according to TEST), then we update its data to
DATA (since the TEST function does not necessarily mean the data is identical).

If DATA is found among the children (according to TEST), the OWNER
`forward-child' is set to the matching child, the child data is set to DATA and
the OWNER current node is set to this child.

If there is no current element, this creates the first element of the tree.
If CREATOR is provided, set the `creator' slot of OWNER."
  (cond
    ((null (current owner))
     (let ((new-node (make-node :data data)))
       (setf (origin owner) new-node
             (creator owner) creator)
       (visit owner new-node)))
    ((not (funcall test data (data (current owner))))
     (let ((node (find-child data owner :test test)))
       (if node
           (setf (data node) data)
           (push (setf node (make-node :data data :parent (current owner)))
                 (children (current owner))))
       (let ((binding (gethash owner (bindings (current owner)))))
         (setf (forward-child binding) node))
       (forward history)))
    (t
     (setf (data (current owner)) data)))
  (current owner))

(defmethod add-child (data (history history-tree) &key (test #'equal))
  "Like `add-child' for the current `owner'.
Return (possibly new) current node, or nil if there is no current owner."
  (when (current-owner history)
    (add-child data (current-owner history) :test test)))

(export 'add-children)
(defmethod add-children (children-data (owner owner) &key (test #'equal))
  "Add CHILDREN-DATA to the `owner''s node children.
Each child is added with `add-child'.
Return the (maybe new) current node, which holds the last piece of data in
`children-data'."
  (add-child (first children-data) owner :test test)
  (if (rest children-data)
      (add-children (rest children-data) (back owner) :test test)
      (current owner)))

(defmethod add-children (children-data (history history-tree) &key (test #'equal))
  "Add CHILDREN-DATA to the HISTORY `current-owner''s node children.
Each child is added with `add-child'.
Return the (maybe new) current node, which holds the last piece of data in
`children-data'.
Return nil if there is no `current-owner'."
  (when (current-owner history)
    (add-children children-data (current-owner history) :test test)))

(export-always 'map-tree)
(defun map-tree (function tree &key flatten include-root (collect-function #'cons)) ; TODO: Edit?  Unexport?
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
                 ;; This lambda is here because (apply #'identity ...) fails on empty arglist.
                 (apply (if flatten #'append #'(lambda (&rest args) args))
                        (mapcar #'traverse (children node)))))))
    (let ((root (typecase tree
                  (htree:node tree)
                  (htree:history-tree (htree:root tree)))))
      (when root
        (if include-root
            (traverse root)
            (apply #'append (mapcar #'traverse (children root))))))))

(export-always 'do-tree)
(defmacro do-tree ((var tree) &body body) ; TODO: Edit? Unexport?
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
