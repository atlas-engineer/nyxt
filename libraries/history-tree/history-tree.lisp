;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :history-tree)

;; TODO: Unexport most (all?) slot writers.
;; TODO: Review docstrings.
;; TODO: Is "Shared history tree" a better name than "Global history tree"?

;; TODO: Turn unique defmethod to defuns.
;; TODO: Thread safe?

;; TODO: Add function to reparent all the branches of an owner.  Test owner
;; deletion first.

;; TODO: Add forward and back functions to unowned nodes, maybe leveraging `visit-all'?
;; TODO: Should we have different functions for finding nodes vs. "owned nodes",
;; or pass an option as key argument?

(defmacro export-always (symbols &optional (package nil package-supplied?)) ; From serapeum.
  "Like `export', but also evaluated at compile time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export ,symbols ,@(and package-supplied? (list package)))))

(define-class entry ()
  ((key (error "Key function required")
        :type function
        :documentation "See `history-tree''s slot of the same name.
This is duplicated here so that it can be accessed from the `entry-equal-p' and
`entry-hash' functions.")
   (test (error "Test function required")
        :type function
        :documentation "See `history-tree''s slot of the same name.
This is duplicated here so that it can be accessed from the `entry-equal-p' and
`entry-hash' functions.")
   (hash-function (error "Hash function required")
        :type function
        :documentation "See `history-tree''s slot of the same name.
This is duplicated here so that it can be accessed from the `entry-equal-p' and
`entry-hash' functions.")
   (value nil
          :type t
          :documentation "Arbitrary data."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "Wrapped data as stored in `history-tree''s `entries'."))

(defun make-entry (history data)        ; Flet in `add-entry'?
  "Return an `entry' wrapping DATA and suitable for HISTORY."
  (make-instance 'entry :value data
                        :key (key history)
                        :test (test history)
                        :hash-function (hash-function history)))

(define-class node ()
  ((parent nil
           :type (or null node))
   (children '()
             :documentation "List of nodes.")
   (bindings (make-hash-table)
             :documentation "The key is an `owner', the value is a
`binding'.  This slot also allows us to know to which owner a node belongs.")
   (entry (error "Entry required")
          :type entry
          :documentation "Arbitrary data (wrapped in an `entry' object) carried
by the node.  `history-tree''s `entries' holds this `entry'-`node' association."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "Internal node of the history tree."))

(defmethod value ((node node))
  (value (entry node)))

(defmethod root ((node node))
  (if (parent node)
      (root (parent node))
      node))

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
  ;; TODO: Add slot pointing to history an owner belongs to?  Unnecessary if we never expose the `owner' to the caller.
  ((origin nil ; TODO: Rename to `root'?  Not to be confused with the htree root, but maybe it's convenient to have the same method name.
           :type (or null node)
           :documentation "The first node created for this owner.")
   (creator nil
            :type t
            :documentation "The owner in `origin's parent node that created this owner.
Unless the parent was disowned by this `creator',

  (gethash (owner history CREATOR) (bindings (parent (origin OWNER))))

should return non-nil.

We store the owner-identifier instead of the `owner' object so that we keep the
information of who created this owner even after the creator object has been
deleted.")
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

(declaim (ftype (function (owner) function) owned-children-lister))
(defun owned-children-lister (owner)
  "Return a function which lists the OWNER's owned children of the node argument."
  (lambda (node)
    (remove-if (complement (alexandria:curry #'owned-p owner))
               (children node))))

(defun owned-children (owner)
  "Return the OWNER's owned children for the current node."
  (funcall (owned-children-lister owner) (current owner) ))

(defun owned-parent (owner)
  "Return OWNER's parent if it's owned, nil otherwise."
  (let ((parent (parent (current owner))))
    (when (gethash owner (bindings parent))
      parent)))

(declaim (ftype (function (owner &optional node) (or null binding)) current-binding))
(defun current-binding (owner &optional (node (current owner)))
  (gethash owner (bindings node)))

(declaim (ftype (function (owner node) (or null binding)) owned-p))
(defun owned-p (owner node)
  (and (bindings node)
       (gethash owner (bindings node))))

(declaim (ftype (function (node) boolean) disowned-p))
(defun disowned-p (node)
  (null (bindings node)))

(declaim (ftype (function (owner node) boolean) disown))
(defun disown (owner node)
  "Remove binding between OWNER and NODE.
Return true if NODE was owned by OWNER, false otherwise."
  (remhash owner (bindings node)))

(defun entry-equal-p (a b)
  (funcall (test a)                     ; `test' of A and B should be the same.
           (funcall (key a) (value a))
           (funcall (key b) (value b))))

(defun entry-hash (a)
  (funcall (hash-function a) (funcall (key a) (value a))))

(cl-custom-hash-table:define-custom-hash-table-constructor make-entry-hash-table
  :test entry-equal-p
  :hash-function entry-hash)

(defun data-equal-entry-p (data entry)
  (if (key entry)
      (equalp (funcall (key entry) (value entry))
              (funcall (key entry) data))
      (equalp (value entry)
              data)))

(defun add-entry (history data)
  "Add DATA to an `entry' in HISTORY `entries'.
If DATA is already there, reset the `entry' value to DATA anyways.
Return the new or existing `entry'."
  (cl-custom-hash-table:with-custom-hash-table
    (let ((new-entry (make-entry history data)))
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
  ((owners (error "Owners need be initialized")
           :type hash-table
           :documentation "The key is an owner identifier (an artitrary balue),
the value is an `owner'.")
   (current-owner-identifier (error "Owner identifier required")
                  :reader current-owner-identifier
                  :type t
                  :documentation "Must be one of the `owners' keys.")
   (entries (make-entry-hash-table)
            :type hash-table
            :documentation "The key is an `entry', the value is the list of
nodes that hold this data.")
   (key #'identity
              :type function
              :documentation "The result of this function is passed to `test'
and `hash-function'.  It is useful to uniquely identify (i.e. avoid
duplications) objects from one of their slots.")
   (test #'equalp
         :type function
         :documentation "Function that tests if the two results of `key' called
over two entries are equal.
Also see `hash-function'.")
   (hash-function #'sxhash
         :type function
         :documentation "Function that returns the hash of the result of `key'
called over an `entry'.
Also see `test'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "Starting point of the global history tree data structure."))

(export 'make)
(defun make (&rest args
             &key key
               test
               hash-function
               (current-owner-identifier "default-owner" explicit-p))
  "Return a new `history-tree'."
  (declare (ignore key test hash-function))
  (let ((owners (make-hash-table :test #'equalp))
        (initial-owner (make-instance 'owner)))
    (setf (gethash current-owner-identifier owners) initial-owner)
    (unless explicit-p
      (setf args (append
                  (list :current-owner-identifier current-owner-identifier)
                  args)))
    (apply #'make-instance 'history-tree
           :owners owners
           args)))

(export-always 'owner)
(declaim (ftype (function (history-tree t) (or null owner)) owner))
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
    (setf (slot-value history 'current-owner-identifier) owner-identifier)
    (the owner owner)))

(export-always 'current-owner)
(declaim (ftype (function (history-tree) owner) current-owner))
(defun current-owner (history)
  (the (values owner &optional) (owner history (current-owner-identifier history))))

(export-always 'current-owner-node)
(declaim (ftype (function (history-tree) node) current-owner-node))
(defun current-owner-node (history)
  (the (values node &optional)
       (current (current-owner history))))

(export-always 'with-current-owner)
(defmacro with-current-owner ((history owner-identifier) &body body)
  (let  ((old-owner-identifier (gensym)))
    `(let ((,old-owner-identifier (current-owner-identifier ,history)))
       (unwind-protect (progn (set-current-owner ,history ,owner-identifier)
                              ,@body)
         (set-current-owner ,history ,old-owner-identifier)))))

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
    (values history node)))

(defmethod visit-all ((history history-tree) node)
  "Like `visit' but on all nodes between the current node and NODE.
This is only possible if the current node and NODE are on the same branch.
If they are not, an error is raised.
Return (values HISTORY NODE) so that calls to `visit' can be chained."
  (labels ((nodes-with-common-parent (node current-node-parents)
             (unless node
               (error "NODE and current owner node must be on the same branch"))
             (if (or (null node)
                     (find node current-node-parents))
                 node
                 (cons node (nodes-with-common-parent (parent node) current-node-parents)))))
    (let* ((current-node (current-owner-node history))
           (current-node-parents (all-parents current-node))
           (node-parents (nreverse (nodes-with-common-parent node current-node-parents)))
           (common-parent (first node-parents)))
      (dolist (node (nreverse (member common-parent (nreverse current-node-parents))))
        (visit history node))
      (dolist (node (rest node-parents)) ; Skip the first node since it's the common-parent and it's already visited.
        (visit history node))
      (values history node))))

(deftype positive-integer ()
  `(integer 1 ,most-positive-fixnum))

(export-always 'back)
(defmethod back ((history history-tree) &optional (count 1))
  "Go COUNT parent up from the current owner node.
Return (VALUES HISTORY CURRENT-NODE) so that `back' and `forward' calls can
be chained."
  (let ((owner (current-owner history)))
    (check-type count positive-integer)
    (when (parent (current owner))
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
    (when (children (current owner))
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
(defmethod go-to-child (data (history history-tree) &key (child-finder #'find-child))
  "Go to direct current node's child matching DATA.
Return (values HISTORY (current-owner-node HISTORY))."
  (let* ((owner (current-owner history))
         (match (funcall child-finder data owner)))
    (when match
      (visit history match))))

(export-always 'go-to-owned-child)
(defmethod go-to-owned-child (data (history history-tree))
  "Go to current node's direct owned child matching DATA.
A child is owned if it has a binding with current owner.
Return (values OWNER (current OWNER))."
  (go-to-child data (current-owner history) :child-finder #'find-owned-child))



(export-always 'add-child)
(defmethod add-child (data (history history-tree) &key creator)
  "Create or find a node holding DATA and set current node to it.
Return the (possibly new) current node.

If current node matches DATA, then we update its data to DATA (since the
`history-tree''s `key' and `test' functions may identify two non-identical datum
as equal).

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
(defmethod add-children (children-data (history history-tree))
  "Add CHILDREN-DATA to the HISTORY `current-owner''s node children.
Each child is added with `add-child'.
Return the (maybe new) current node, which holds the last piece of data in
`children-data'.
Return nil if there is no `current-owner'."
  (let ((owner (current-owner history)))
    (add-child (first children-data) owner)
    (if (rest children-data)
        (add-children (rest children-data) (back owner))
        (current owner))))

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
                  (node tree)
                  (history-tree (root tree)))))
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

(defmethod all-children ((history history-tree))
  "Return a list of all the children of HISTORY's current owner, recursively."
  (all-children (current-owner-node history)))

(export-always 'all-contiguous-owned-children)
(defmethod all-contiguous-owned-children ((history history-tree) &optional node)
  "Return a list of all the children owned by HISTORY's current owner,
recursively."
  (let ((owner (current-owner history)))
    (map-tree #'identity (or node (current owner))
              :flatten t
              :children-function (owned-children-lister owner))))

(export-always 'all-parents)
(defmethod all-parents ((node node))
  "Return a list of parents of NODE, recursively.
First parent comes first in the resulting list."
  (when (parent node)
    (cons (parent node)
          (all-parents (parent node)))))

(defmethod all-parents ((history history-tree))
  "Return a list of all parents of the current node.
First parent comes first in the resulting list."
  (all-parents (current-owner-node history)))

(export-always 'all-contiguous-owned-parents)
(defmethod all-contiguous-owned-parents ((history history-tree))
  "Return a list of parents of owned by HISTORY current owner, starting from its
current node, recursively.  First parent comes first in the resulting list."
  (labels ((node-contiguous-owned-parents (node)
             (when (and (parent node)
                        (owned-p (current-owner history) (parent node)))
               (cons (parent node)
                     (node-contiguous-owned-parents (parent node))))))
    (node-contiguous-owned-parents (current-owner-node history))))

(export-always 'all-forward-children)
(defmethod all-forward-children ((history history-tree)
                                 &optional (node (current-owner-node history)))
  "Return a list of the forward children of NODE, recursively.
First child comes first in the resulting list."
  (let* ((owner (current-owner history))
         (binding (current-binding owner node)))
    (when (and binding (forward-child binding))
      (cons (forward-child (current-binding owner))
            (all-forward-children history
                                  (forward-child binding))))))

(export 'all-nodes)
(defmethod all-nodes ((history history-tree))
  "Return a list of all current owner nodes, in unspecified order."
  (nodes (current-owner history)))

(export 'all-contiguous-owned-nodes)
(defmethod all-contiguous-owned-nodes ((history history-tree))
  "Return a list of all owner nodes, in depth-first order."
  (let ((owner-root (first (last (all-contiguous-owned-parents history)))))
    (when owner-root
      (cons owner-root (all-contiguous-owned-children history owner-root)))))



;; TODO: Delete these functions?
(export-always 'all-data)
(defmethod all-data ((history history-tree))
  "Return a list of all entries data, in unspecified order."
  (mapcar #'value (alexandria:hash-table-keys (entries history))))

(export-always 'all-nodes-data)
(defmethod all-nodes-data ((history history-tree))
  "Return a list of all nodes data, in depth-first order."
  (mapcar #'value (all-nodes history)))

(export-always 'all-contiguous-owned-nodes-data)
(defmethod all-contiguous-owned-nodes-data ((history history-tree))
  "Return a list of all nodes data, in depth-first order."
  (mapcar #'value (all-contiguous-owned-nodes history)))

(export-always 'parent-nodes-data)
(defmethod parent-nodes-data ((history history-tree))
  "Return a list of all parent nodes data.
First parent comes first."
  (mapcar #'value (all-parents history)))

(export-always 'forward-children-nodes-data)
(defmethod forward-children-nodes-data ((history history-tree))
  "Return a list of all forward children nodes data.
First child comes first."
  (mapcar #'value (all-forward-children history)))

(export-always 'children-nodes-data)
(defmethod children-nodes-data ((history history-tree))
  "Return a list of all children nodes data, in depth-first order."
  (mapcar #'value (all-children history)))



(defun disowned-branch-nodes (node)
  "Return list of all NODE's children (including NODE) if they are all disowned.
Return nil otherwise."
  (let ((disowned? t)
        (children '()))
    (block nil
      (do-tree (child-node node)
        (unless (disowned-p child-node)
          (setf disowned? nil)
          (return))
        (push child-node children)))
    (when disowned?
      children)))

(defun delete-node (history node)
  (cl-custom-hash-table:with-custom-hash-table
    (alexandria:deletef (gethash (entry node) (entries history)) node)))

(defun delete-disowned-branch-nodes (history owner)
  ;; We memoize the deleted nodes to avoid retraversing the branch for all the
  ;; nodes that belong to the same branch.
  (let ((deleted-nodes '()))
    (labels ((garbage-collect (list-of-nodes)
               (when list-of-nodes
                 (let* ((node (first list-of-nodes))
                        (root (root node)))
                   (unless (find node deleted-nodes)
                     (let ((disowned-nodes (disowned-branch-nodes root)))
                       (when disowned-nodes
                         (dolist (node deleted-nodes)
                           (delete-node history node))
                         (setf deleted-nodes
                               (append disowned-nodes
                                       deleted-nodes))))))
                 (garbage-collect (rest list-of-nodes)))))
      (garbage-collect (nodes owner)))))

(defun first-hash-table-value (hash-table)
  (with-hash-table-iterator (next-entry hash-table)
    (nth-value 2 (next-entry))))

(declaim (ftype (function (history-tree t) (or null owner)) delete-owner))
(defun delete-owner (history owner-identifier)
  "Delete `owner' corresponding to OWNER-IDENTIFIER from HISTORY.
For every branch `owner' has nodes on, remove all its nodes if the branch is
without any owner.
Return owner, or nil if there is no owner corresponding to OWNER-IDENTIFIER."
  (let ((owner (owner owner-identifier history)))
    (remhash owner-identifier (owners history))
    (when (eq owner (current-owner history))
      (setf (slot-value history 'current-owner)
            (first-hash-table-value (owners history))))
    (when owner
      (mapc
       (lambda (node)
         (disown owner node))
       (nodes owner)))
    ;; Delete nodes only when whole branch is owner-less.  Indeed, otherwise we would
    ;; lose information for other owners.  It's better to be as "immutable" as possible.
    ;;
    ;; If we want to "free" disowned nodes or edit a given owner's history, the
    ;; best approach is to copy its owned branches to new branches that are
    ;; directly connected to the root.
    (delete-disowned-branch-nodes history owner)
    owner))



(deftype non-negative-integer ()
  `(integer 0 ,most-positive-fixnum))

(export-always 'depth)
(declaim (ftype (function (history-tree) non-negative-integer) depth))
(defun depth (history)
  "Return the number of parents of the current owner node."
  (length (all-parents history)))

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
