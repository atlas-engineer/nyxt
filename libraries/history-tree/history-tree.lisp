;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :history-tree)

;; TODO: Unexport most (all?) slot writers.
;; TODO: Review docstrings.
;; TODO: Is "Shared history tree" a better name than "Global history tree"?

;; TODO: Add forward and back functions to unowned nodes.
;; TODO: Add `with-owner' macro.
;; TODO: Thread safe?

(defmacro export-always (symbols &optional (package nil package-supplied?)) ; From serapeum.
  "Like `export', but also evaluated at compile time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export ,symbols ,@(and package-supplied? (list package)))))

(define-class entry ()
  ((key nil
        :type (or null function)
        :documentation "Function call over `value', the result is `equalp'ed to
identify the data uniquely.")
   (value nil
          :type t
          :documentation "List of nodes."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "Wrapped data as stored in `history-tree''s `entries'."))

(define-class node ()
  ((parent nil
           :type (or null node))
   (children '()
             :documentation "List of nodes.")
   (bindings (make-hash-table)
             :documentation "The key is an `owner', the value is a
`binding'.  This slot also allows us to know to which owner a node belongs.")
   (entry nil
          :type (or null entry)
          :documentation "Arbitrary data carried by the node.  This entry is
mirrored as a key in `history-tree''s `entries' slot, and the node is added to
the value list."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "Internal node of the history tree."))

(defun make-node (&key parent entry) ; TODO: Useless?
  (make-instance 'node :parent parent :entry entry))

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
   (creator nil                         ; TODO: Should it be an `owner' or an
                                        ; identifier?  An identifier would allow
                                        ; us to keep this information even afer
                                        ; the creator `owner' is deleted.
            :type t
            :documentation "The owner in `origin's parent node that created this owner.
Unless the parent was disowned by this `creator',

  (gethash CREATOR (bindings (origin OWNER)))

should return non-nil.")
   (current nil
            :type (or null node)
            :reader current
            :documentation "The current node.
It's updated every time a node is visited.")
   (nodes '()
          :type (or null (cons node))
          :documentation "The list of owned nodes."))
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

(declaim (ftype (function (owner) (or null binding)) current-binding))
(defun current-binding (owner)
  (and (current owner)
       (the binding
            (gethash owner (bindings (current owner))))))

(declaim (ftype (function (owner node) (or null binding)) owned-p))
(defun owned-p (owner node)
  (and (bindings node)
       (gethash owner (bindings node))))

(declaim (ftype (function (owner node) boolean) disown))
(defun disown (owner node)
  "Remove binding between OWNER and NODE.
Return true if NODE was owned by OWNER, false otherwise."
  (remhash owner (bindings node)))

(defun entry-equal-p (a b)
  (if (key a)
      (equalp (funcall (key a) (value a))
              (funcall (key b) (value b)))
      (equalp (value a)
              (value b))))

(defun data-equal-entry-p (data entry)
  (if (key entry)
      (equalp (funcall (key entry) (value entry))
              (funcall (key entry) data))
      (equalp (value entry)
              data)))

(defun entry-hash (a)
  (if (key a)
      (sxhash (funcall (key a) (value a)))
      (sxhash (value a))))

(cl-custom-hash-table:define-custom-hash-table-constructor make-entry-hash-table
  :test entry-equal-p
  :hash-function entry-hash)

(defun add-entry (history data)
  "Add DATA to an `entry' in HISTORY `entries'.
If DATA is already there, reset the `entry' value to DATA anyways.
Return the new or existing `entry'."
  (cl-custom-hash-table:with-custom-hash-table
    (let ((new-entry (make-instance 'entry :value data :key (entry-key history))))
      (multiple-value-bind (existing-entry found?)
          (gethash new-entry (entries history))
        (if found?
            (progn
              (setf (value existing-entry) data)
              existing-entry)
            (progn
              (setf (gethash new-entry (entries history)) '())
              new-entry))))))

(define-class history-tree ()           ; TODO: Rename `history'?
  ((root nil
         :type (or null node)
         :documentation "The root node.
It only changes when deleted.")
   (owners (error "Owners need be initialized")
           :type hash-table
           :documentation "The key is an owner identifier (an artitrary balue),
the value is an `owner'.")
   (current-owner (error "Owner required")
                  :type owner
                  :documentation "Must be one of the `owners' values.")
   (entries (make-entry-hash-table)
            :type hash-table
            :documentation "The key is an `entry', the value is the list of
nodes that hold this data.")
   ;; TODO: Add `hash-function' and `test'.
   (entry-key nil                       ; TODO: Rename to `key'?
              :type (or null function)
              :documentation "The function used to access entries.")) ; TODO: Clarify this docstring.
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "Staring point of the global history tree data structure."))

(export 'make)
(defun make (&key entry-key (owner-identifier "default-owner"))
  "Return a new `history-tree'."
  (let ((owners (make-hash-table :test #'equalp))
        (initial-owner (make-instance 'owner)))
    (setf (gethash owner-identifier owners) initial-owner)
    (make-instance 'history-tree :entry-key entry-key
                                 :owners owners
                                 :current-owner initial-owner)))

(export-always 'owner)
(declaim (ftype (function (history-tree t) (or null owner)) owner))
(defun owner (history owner-identifier)
  "Return the `owner' object identified by OWNER-IDENTIFIER in HISTORY."
  (gethash owner-identifier (owners history)))

(export-always 'set-current-owner)
;; (declaim (ftype (function (history-tree t) owner) set-current-owner)) ; TODO: Fix type.
(defun set-current-owner (history owner-identifier)
  "OWNER-IDENTIFIER is arbitrary data representing an `owner'."
  (let ((owner (owner history owner-identifier)))
    (unless owner
      (setf owner
            (setf (gethash owner-identifier (owners history)) (make-instance 'owner))))
    (setf (current-owner history) owner)))

(export-always 'current-owner-node)
;; (declaim (ftype (function (history-tree) (or null node)) current-owner-node)) ; TODO: Fix type.
(defun current-owner-node (history)
  (when (current-owner history)
    (current (the owner (current-owner history)))))

;; TODO: Add `gethash*' to set default value when not found?

(defmethod visit ((history history-tree) node)
  "Visit NODE with HISTORY's current owner.
Return (values HISTORY NODE) so that calls to `visit' can be chained."
  (let ((owner (current-owner history)))
    (pushnew node (nodes owner))        ; TODO: Replace with hash-table to speedup?
    (setf (current owner) node)
    (let ((binding (gethash owner (bindings node))))
      (if binding
          (setf (last-access binding) (local-time:now))
          (setf (gethash owner (bindings node))
                (make-instance 'binding))))
    (cl-custom-hash-table:with-custom-hash-table
      (pushnew owner (gethash (entry node) (entries history))))
    ;; TODO: If node is among the children, should we set all the forward children
    ;; to ensure that calling `forward` from CURRENT would lead to NODE?
    ;; What if there is no path owned by OWNER?
    (values history node)))

(deftype positive-integer ()
  `(integer 1 ,most-positive-fixnum))

(export-always 'back)
(defmethod back ((history history-tree) &optional (count 1))
  "Go COUNT parent up from the current owner node.
Return (VALUES HISTORY CURRENT-NODE) so that `back' and `forward' calls can
be chained."
  (let ((owner (current-owner history)))
    (check-type count positive-integer)
    (when (and owner
               (current owner)
               (parent (current owner)))
      (let ((former-current (current owner)))
        (visit history (parent (current owner)))
        ;; Put former current node back as forward-child if it is not already
        ;; the case, e.g. if current node was set manually.
        (setf (forward-child (current-binding owner))
              former-current))
      (when (< 1 count)
        (back history (1- count))))
    (values history (current owner))))

(export-always 'forward)
(defmethod forward ((history history-tree) &optional (count 1))
  "Go COUNT first-children down from the current owner node.
Return (values HISTORY CURRENT-NODE)) so that `back' and `forward' calls can be
chained."
  (check-type count positive-integer)
  (let ((owner (current-owner history)))
    (when (and owner
               (current owner)
               (children (current owner)))
      (visit history (forward-child (current-binding owner)))
      (when (< 1 count)
        (forward history (1- count))))
    (values history (current owner))))

(declaim (ftype (function (t owner) (or null node)) find-child))
(defun find-child (data owner) ; TODO: Generalize?
  "Return the direct child node of OWNER which matches DATA. "
  (find data
        (children (current owner))
        :key #'entry
        :test #'data-equal-entry-p))

(declaim (ftype (function (t owner) (or null node))
                find-owned-child))
(defun find-owned-child (data owner) ; TODO: Generalize?
  "Return the direct child node owned by OWNER which matches DATA.
Test is done with the TEST argument."
  (find data
        (owned-children owner)
        :key #'entry
        :test #'data-equal-entry-p))

(export-always 'go-to-child)
(defmethod go-to-child (data (owner owner) &key (child-finder #'find-child))
  "Go to current OWNER node's direct child matching DATA.
Return (values OWNER (current OWNER))."
  (when (current owner)
    (let ((match (funcall child-finder data owner)))
      (when match
        (visit owner match))))
  (values owner (current owner)))

(export-always 'go-to-owned-child)
(defmethod go-to-owned-child (data (owner owner))
  "Go to current OWNER node's direct owned child matching DATA.
A child is owned if it has a binding with OWNER.
Return (values OWNER (current OWNER))."
  (go-to-child data owner :child-finder #'find-owned-child))

(defmethod go-to-child (data (history history-tree) &key (child-finder #'find-child))
  "Go to direct current node's child matching DATA."
  (when (current-owner history)
    (go-to-child data (current-owner history)
                 :child-finder child-finder)))

(defmethod go-to-owned-child (data (history history-tree))
  "Go to current node's direct owned child matching DATA.
A child is owned if it has a binding with current owner.
Return (values OWNER (current OWNER))."
  (when (current-owner history)
    (go-to-owned-child data (current-owner history)
                       :child-finder #'find-owned-child)))

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
(defmethod add-child (data (history history-tree) &key creator)
  "Create or find a node holding DATA and set current node to it.
Return the (possibly new) current node.

If current node matches DATA, then we update its data to DATA (since the
`history-tree''s `entry-key' function does not necessarily mean the data is
identical).

If DATA is found among the children, the OWNER `forward-child' is set to the
matching child, the child data is set to DATA and the OWNER current node is set
to this child.

If there is no current element, this creates the first element of the tree.
If CREATOR is provided, set the `creator' slot of OWNER."
  (let ((owner (current-owner history)))
    (cond
      ((null (current owner))
       (let ((new-node (make-node :entry (add-entry history data))))
         (setf (origin owner) new-node
               (creator owner) creator)
         (visit history new-node)))
      ((not (data-equal-entry-p data (entry (current owner))))
       (let ((node (find-child data owner)))
         (if node
             (setf (value (entry node)) data)
             (let ((maybe-new-entry (add-entry history data)))
               (push (setf node (make-node :entry maybe-new-entry
                                           :parent (current owner)))
                     (children (current owner)))))
         (let ((binding (gethash owner (bindings (current owner)))))
           (setf (forward-child binding) node))
         (forward history)))
      (t
       (setf (value (entry (current owner))) data)))
    (current owner)))

(export 'add-children)
(defmethod add-children (children-data (owner owner))
  "Add CHILDREN-DATA to the `owner''s node children.
Each child is added with `add-child'.
Return the (maybe new) current node, which holds the last piece of data in
`children-data'."
  (add-child (first children-data) owner)
  (if (rest children-data)
      (add-children (rest children-data) (back owner))
      (current owner)))

(defmethod add-children (children-data (history history-tree))
  "Add CHILDREN-DATA to the HISTORY `current-owner''s node children.
Each child is added with `add-child'.
Return the (maybe new) current node, which holds the last piece of data in
`children-data'.
Return nil if there is no `current-owner'."
  (when (current-owner history)
    (add-children children-data (current-owner history))))

(export-always 'map-tree)
(defun map-tree (function tree &key flatten include-root ; TODO: Edit?  Unexport?
                                 (collect-function #'cons)
                                 (children-function #'children))
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
                        (mapcar #'traverse (funcall children-function node)))))))
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



(export-always 'all-children)
(defmethod all-children ((node node))
  "Return a list of all the children of the NODE, recursively."
  (map-tree #'identity node :flatten t))

(defmethod all-children ((owner owner))
  "Return a list of all the children of OWNER, recursively."
  (all-children (current owner)))

(defmethod all-children ((history history-tree))
  "Return a list of all the children of HISTORY's current owner, recursively."
  (when (current-owner history)
    (all-children (current-owner-node history))))

(export-always 'all-contiguous-owned-children)
(defmethod all-contiguous-owned-children ((owner owner))
  "Return a list of all the children owned by OWNER, recursively."
  (map-tree #'identity (current owner) :flatten t
                                       :children-function #'owned-children))

(defmethod all-contiguous-owned-children ((history history-tree))
  "Return a list of all the children owned by HISTORY's current owner,
recursively."
  (when (current-owner history)
    (all-contiguous-owned-children (current-owner history))))

(export-always 'all-parents)
(defmethod all-parents ((node node))
  "Return a list of parents of NODE, recursively.
First parent comes first in the resulting list."
  (when (parent node)
    (cons (parent node)
          (all-parents (parent node)))))

(defmethod all-parents ((owner owner))
  "Return a list of parents of OWNER current node, recursively.
First parent comes first in the resulting list."
  (when (current owner)
    (all-parents (current owner))))

(defmethod all-parents ((history history-tree))
  "Return a list of all parents of the current node.
First parent comes first in the resulting list."
  (when (current-owner history)
    (all-parents (current-owner-node history))))

(export-always 'all-contiguous-owned-parents)
(defmethod all-contiguous-owned-parents ((owner owner))
  "Return a list of parents of owned by OWNER, starting from its current node,
recursively.
First parent comes first in the resulting list."
  (when (and (parent owner)
             (owned-p owner (parent owner)))
    (cons (parent owner)
          (all-contiguous-owned-parents (current owner)))))

(defmethod all-contiguous-owned-parents ((history history-tree))
  "Return a list of parents of owned by HISTORY current owner, starting from its
current node, recursively.  First parent comes first in the resulting list."
  (when (current-owner history)
    (all-contiguous-owned-parents (current-owner history))))

(export-always 'all-forward-children)
(defmethod all-forward-children ((owner owner))
  "Return a list of the first children of OWNER, recursively.
First child comes first in the resulting list."
  (when (and (current-binding owner)
             (forward-child (current-binding owner)))
    (cons (forward-child (current-binding owner))
          (all-forward-children (forward-child (current-binding owner))))))

(defmethod all-forward-children ((history history-tree))
  "Return a list of the first children of NODE, recursively.
First child comes first in the resulting list."
  (when (current-owner history)
    (all-forward-children (current-owner history))))

(export 'all-nodes)
(defmethod all-nodes ((history history-tree))
  "Return a list of all nodes, in depth-first order."
  (let ((root (root history)))
    (when root
      (cons root (all-children root)))))

(defmethod all-nodes ((owner owner))
  "Return a list of all OWNER nodes, in no particular order."
  (nodes owner))

(export 'all-contiguous-owned-nodes)
(defmethod all-contiguous-owned-nodes ((owner owner))
  "Return a list of all owner nodes, in depth-first order."
  (let ((owner-root (first (all-contiguous-owned-parents owner))))
    (when owner-root
      (cons owner-root (all-contiguous-owned-children owner-root)))))


(defun node-value (node)
  (value (entry node)))

(export-always 'all-nodes-data)
(defmethod all-nodes-data ((history history-tree))
  "Return a list of all nodes data, in depth-first order."
  (mapcar #'node-value (all-nodes history)))

(export-always 'parent-nodes-data)
(defmethod parent-nodes-data ((history history-tree))
  "Return a list of all parent nodes data.
First parent comes first."
  (mapcar #'node-value (all-parents history)))

(export-always 'forward-children-nodes-data)
(defmethod forward-children-nodes-data ((history history-tree))
  "Return a list of all forward children nodes data.
First child comes first."
  (mapcar #'node-value (all-forward-children history)))

(export-always 'children-nodes-data)
(defmethod children-nodes-data ((history history-tree))
  "Return a list of all children nodes data, in depth-first order."
  (mapcar #'node-value (all-children history)))


(defun first-hash-table-value (hash-table)
  (with-hash-table-iterator (next-entry hash-table)
    (nth-value 2 (next-entry))))

(declaim (ftype (function (history-tree t) (or null owner)) delete-owner))
(defun delete-owner (history owner-identifier)
  "Return owner, or nil if there is no owner corresponding to OWNER-IDENTIFIER."
  (let ((owner (owner owner-identifier history)))
    (remhash owner-identifier (owners history))
    (when (eq owner (current-owner history))
      (setf (current-owner history)
            (first-hash-table-value (owners history))))
    (when owner
      (mapc
       (lambda (node)
         (disown owner node))
       (nodes owner)))
    ;; TODO: Garbage-collect owner-less branch.
    ;;
    ;; Delete nodes when whole branch is owner-less.  Indeed, otherwise we would
    ;; lose information.  It's better to be as "immutable" as possible.
    ;;
    ;; If we want to "free" disowned nodes or edit a given owner's history, the
    ;; best approach is to copy its owned branches to new branches that are
    ;; directly connected to the root.
    owner))

(export-always 'find-node)
(defun find-node (item tree &key (key #'identity))
  "Find a tree node matching ITEM in TREE and return it.
TREE can be a `history' or a `node'. "
  (block search
    (do-tree (node tree)
      (when (funcall (entry-key tree) item (funcall key node))
        (return-from search node)))))

(export-always 'remove-node)
(defun remove-node (item tree &key (key #'identity))
  "Return all the nodes from TREE that didn't match ITEM.
TREE can be a `history' or a `node'."
  (let (result)
    (do-tree (node tree)
      (unless (funcall (entry-key tree) item (funcall key node))
        (push node result)))
    result))


;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (export 'find-data))
;; (defmethod find-data (data (history history-tree) &key (test #'equal) ensure-p)
;;   "Find a tree node matching DATA in HISTORY and return it.
;; If ENSURE-P is non-nil, create this node when not found.
;; Search is done with the help of TEST argument."
;;   (let ((match (find-node data history :test test :key #'entry)))
;;     ;; TODO: `add-child' changes `current'. Always change `current' on match?
;;     (or match (when ensure-p (add-child data history :test test)))))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (export 'delete-data))
;; (defmethod delete-data (data (history history-tree) &key (test #'equal) rebind-children-p)
;;   "Delete node(s) matching DATA from HISTORY and return the last deleted node.
;; If the node has children itself, and REBIND-CHILDREN-P is not nil, these
;; will become children of the node's parent. Search is done with the
;; help of TEST argument."
;;   (let ((last-deleted nil))
;;     (do-tree (node history)
;;       (when (funcall test data (data node))
;;         (setf (children (parent node)) (append (when rebind-children-p (children node))
;;                                                (remove node (children (parent node))))
;;               last-deleted node)))
;;     last-deleted))


(deftype non-negative-integer ()
  `(integer 0 ,most-positive-fixnum))

(export-always 'depth)
(declaim (ftype (function ((or history-tree owner)) non-negative-integer) depth))
(defun depth (history-or-owner)
  "Return the number of parents of the current owner node."
  (length (all-parents history-or-owner)))

(export-always 'size)
(defmethod size ((owner owner))
  "Return the total number of nodes owned by OWNER."
  (length (nodes owner)))

(defmethod connected-size ((owner owner)) ; TODO: Prefer "connected" over "contiguous"?
  "Return the total number of owned nodes connect to the current OWNER node."
  (length (all-contiguous-owned-nodes owner)))

(defmethod size ((history history-tree))
  "Return the total number of nodes."
  ;; TODO: This could be optimized with a SIZE slot, but is it worth it?
  (length (all-nodes history)))
