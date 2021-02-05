;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :history-tree)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria))

;; TODO: Thread safe?

;; TODO: Use fast sets for unique lookups?  Turns out hash-tables are overkill
;; with SBCL on modern hardware, for less than 10.000.000 entries.
;; Always use lists then?
;; See TODO notes below mentioning "fast sets".
;; See https://old.reddit.com/r/Common_Lisp/comments/l1z7ei/fast_set_library_or_gethash_vs_findmember/.

;; TODO: Should we have different functions for finding nodes vs. "owned nodes",
;; or pass an option as key argument?

;; TODO: Is "Shared history tree" a better name than "Global history tree"?
;; TODO: Turn unique defmethod to defuns.

(defmacro export-always (symbols &optional (package nil package-supplied?)) ; From serapeum.
  "Like `export', but also evaluated at compile time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export ,symbols ,@(and package-supplied? (list package)))))

(deftype function-symbol ()
  `(and symbol (satisfies fboundp)))

(define-class entry ()
  ((history nil
            :type (or null history-tree)
            :documentation "Required.
This is gives access to the custom hash functions, see the corresponding
`history-tree' slots.
We allow null values for easier deserialization.")
   (data nil
         :type t
         :documentation "Arbitrary data.")
   (last-access (local-time:now)
                :type (or local-time:timestamp string) ; Support `string' for easier deserialization.
                :documentation "The last access to the corresponding entry by
any owner.  It's useful to keep this access stored here so that when an entry
goes owner-less, we can still consult the last time it was accessed.")
   (nodes '()
          :type list
          :documentation "The list of nodes that access an entry."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Wrapped data as stored in `history-tree''s `entries'.
Each entry has a unique datum.  Each `node' points to one entry.  Multiple nodes
may point to the same entry.  Entries may also be node-less; they are kept
around so that we can remember the data that was visited since the beginning of
time.  Node-less entries are available for manual deletion with
`delete-data'."))

(defun ensure-timestamp (string-or-timestamp)
  (if (stringp string-or-timestamp)
      (or (ignore-errors (local-time:parse-timestring string-or-timestamp))
          (local-time:now))
      string-or-timestamp))

(defmethod last-access ((entry entry))
  "Ensure we return last-access as a timestamp, in case it was a string."
  (setf (slot-value entry 'last-access) (ensure-timestamp
                                         (slot-value entry 'last-access))))

(defun make-entry (history data &optional last-access)
  "Return an `entry' wrapping DATA and suitable for HISTORY."
  (make-instance 'entry :data data
                        :history history
                        :last-access (or last-access (local-time:now))))

(define-class node ()
  ((parent nil
           :type (or null node)
           :documentation "If nil, it means the node is a root node.
(The first of the parents.)")
   (children '()
             :type (list node)
             :documentation "Order does not matter.")
   (bindings (make-hash-table)
             :documentation "The key is an `owner', the value is a
`binding'.  This slot also allows us to know to which owner a node belongs.")
   (entry nil
          :type (or null entry)
          :documentation "Required.
(Null entry is accepted only to ease deserialization.)
Arbitrary data (wrapped in an `entry' object) carried
by the node.  `history-tree''s `entries' holds `entry'-`node' associations."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Node structure of the history tree.
Each node has one parent (unless it's a root node) and zero or multiple
children.  Nodes may have zero or multiple owners."))

(export 'data)
(defmethod data ((node node))
  (data (entry node)))

(defmethod root ((node node))
  (if (parent node)
      (root (parent node))
      node))

(defun make-node (&key parent entry)
  (let ((node (make-instance 'node :parent parent :entry entry)))
    (cl-custom-hash-table:with-custom-hash-table
      (pushnew node (nodes entry)))
    node))

(define-class binding ()
  ((forward-child  nil
                  :type (or null node)
                  :documentation "Which of the `children' (in the bound `node')
is the child to go forward to for the bound owner.")
   (last-access (local-time:now)
                :type (or local-time:timestamp string) ; Support `string' for easier deserialization.
                :documentation "Timestamp of the last access to this node by the
owner."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "The relationship between an owner and one of its nodes.
In particular, it encodes the forward child nad the date of last access to the
node for a given owner."))

(export-always 'last-access)
(defmethod last-access ((binding binding))
  "Ensure we return last-access as a timestamp, in case it was a string."
  (setf (slot-value binding 'last-access) (ensure-timestamp
                                           (slot-value binding 'last-access))))

(defmethod last-access ((node node))
  "Return node's last access across all its owners.
If the node has no owner, return Epoch."
  (if (< 0 (hash-table-count (bindings node)))
      (apply #'local-time:timestamp-maximum
             (mapcar #'last-access
                     (alex:hash-table-values (bindings node))))
      (local-time:unix-to-timestamp 0)))

(export-always 'data-last-access)
(declaim (ftype (function (history-tree t) local-time:timestamp) data-last-access))
(defun data-last-access (history data)
  "Return data last access across all its nodes, regardless of the owner.
Return Epoch if DATA is not found or if entry has no timestamp."
  (let* ((entry (find-entry history data))
         (nodes (when entry (nodes entry))))
    (the (values local-time:timestamp &optional)
         (if nodes
             (let ((new-last-access
                     (apply #'local-time:timestamp-maximum
                            (mapcar #'last-access nodes))))
               (setf (last-access entry) new-last-access)
               new-last-access)
             (if entry
                 (last-access entry)
                 (local-time:unix-to-timestamp 0))))))

(define-class owner ()
  ;; REVIEW: Add slot pointing to history an owner belongs to?  As long as the
  ;; owner has at least one node, the history can be accessed via the entry.
  ((origin nil
           :type (or null node)
           :documentation "The first node created for this owner.
Not to be confused with the root, since the owner be go back to a parent of `origin'.")
   (creator-id nil
            :type t
            :documentation "The owner-id in `origin's parent node that
created this owner.  May be nil, in which case `origin' is a root node.

Unless the parent was disowned by this `creator-id',

  (gethash (owner history CREATOR-ID) (bindings (parent (origin OWNER))))

should return non-nil.

We store the owner-id instead of the `owner' object so that we keep the
information of who created this owner even after the creator object has been
deleted.")
   (creator-node nil
                 :export nil
                 :type (or null node)
                 :documentation "The current node of the creator when this owner
is created.  This is useful since the owner corresponding to `creator-id' may be
deleted before the `origin' node is added.")
   (current nil
            :type (or null node)
            :reader current
            :export t
            :documentation "The current node.
It's updated every time a node is visited.")
   (nodes '()
          :type (or null (cons node))
          :documentation "The list of all owned nodes."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "The high-level information about an owner.
Each owner is identified by a unique identifier, which is arbitrary data (may
even be NIL)."))

(defmethod (setf current) (value (owner owner))
  "This setter protects against setting OWNER's `current' slot to an invalid object."
  (if (node-p value)
      (setf (slot-value owner 'current) value)
      (error "Attempted to set current node to a non-node for owner ~a." owner)))

(defmethod last-access ((owner owner))
  "Return owner current node last access."
  (last-access (gethash owner (bindings (current owner)))))

(declaim (ftype (function (owner) function) owned-children-lister))
(defun owned-children-lister (owner)
  "Return a function which lists the OWNER's owned children of the node argument."
  (lambda (node)
    (remove-if (complement (alex:curry #'owned-p owner))
               (children node))))

(export-always 'owned-children)
(defun owned-children (owner)
  "Return the OWNER's owned children for the current node."
  (funcall (owned-children-lister owner) (current owner) ))

(export-always 'owned-parent)
(defun owned-parent (owner node)
  "Return OWNER's parent if it's owned, nil otherwise."
  (let ((parent (and node (parent node))))
    (when (owned-p owner parent)
      parent)))

(export-always 'current-binding)
(declaim (ftype (function (owner &optional node) (or null binding)) current-binding))
(defun current-binding (owner &optional (node (current owner)))
  (and node
       (gethash owner (bindings node))))

(export-always 'owned-p)
(declaim (ftype (function (owner (or null node)) (or null binding)) owned-p))
(defun owned-p (owner node)
  (and node
       (bindings node)
       (gethash owner (bindings node))))

(declaim (ftype (function (node) boolean) disowned-p))
(defun disowned-p (node)
  (= 0 (hash-table-count (bindings node))))

(declaim (ftype (function (owner node) boolean) disown))
(defun disown (owner node)
  "Remove binding between OWNER and NODE.
Return true if NODE was owned by OWNER, nil otherwise."
  (alex:deletef (nodes owner) node)
  (remhash owner (bindings node)))

(defun entry-equal-p (a b)
  (let ((h (history a)))
    (funcall (test h)
             (funcall (key h) (data a))
             (funcall (key h) (data b)))))

(defun entry-hash (a)
  (let ((h (history a)))
    (funcall (hash-function h) (funcall (key h) (data a)))))

(cl-custom-hash-table:define-custom-hash-table-constructor make-entry-hash-table
  :test entry-equal-p
  :hash-function entry-hash)

(defun data-equal-entry-p (data entry)
  (let ((h (history entry)))
    (funcall (test h)
             (funcall (key h) (data entry))
             (funcall (key h) data))))

(export-always 'add-entry)
(defun add-entry (history data &optional last-access)
  "Add DATA to an `entry' in HISTORY `entries'.
If DATA is already there, reset the `entry' data to DATA anyways.
Return the new or existing `entry'.

The higher-level functions take care of adding entries for you, so you normally
need not call this function.  See `add-child' instead.
One case in which this function might be useful is when you want to import flat
history data, e.g. a list of visited URLs that's not bound to any owner."
  (cl-custom-hash-table:with-custom-hash-table
    (let ((new-entry (make-entry history data last-access)))
      (multiple-value-bind (existing-entry found?)
          (gethash new-entry (entries history))
        (if found?
            (progn
              (setf (data existing-entry) data)
              (when last-access
                (setf (last-access existing-entry) last-access))
              existing-entry)
            (progn
              (setf (gethash new-entry (entries history))
                    new-entry)
              new-entry))))))

(define-class history-tree ()           ; TODO: Rename `history'?
  ((owners (error "Owners need be initialized")
           :type hash-table
           :documentation "The key is an owner identifier (an artitrary value),
the value is an `owner'.")
   (current-owner-id (error "Owner identifier required")
                     :reader current-owner-id
                     :type t
                     :export t
                     :documentation "Must be one of the `owners' keys.")
   (entries (make-entry-hash-table)
            :type hash-table
            :documentation "Both the key and the value are an `entry', so that
we can access the actual object from a given piece of data.
Indeed, with custom hash table the key that is store (here the entry) is not
necessarily identical to the one used in `gethash'.  So storing the entry as a
value gives us access to to the actual object.")
   (key 'identity
        :type function-symbol
        :documentation "The result of this function is passed to `test'
and `hash-function'.  It is useful to uniquely identify (i.e. avoid
duplications) objects from one of their slots.
It is a `function-symbol' so that the history can be more easily serialized than
if if were a function.")
   (test 'equalp
         :type function-symbol
         :documentation "Function that tests if the two results of `key' called
over two entries are equal.
Also see `hash-function'.
It is a `function-symbol' so that the history can be more easily serialized than
if if were a function.")
   (hash-function 'sxhash
                  :type function-symbol
                  :documentation "Function that returns the hash of the result
of `key' called over an `entry'.
Also see `test'.
It is a `function-symbol' so that the history can be more easily serialized than
if if were a function."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Starting point of the global history tree data structure."))

(export-always '+default-owner+)
(defparameter +default-owner+ "default-owner")

(export 'make)
(defun make (&rest args
             &key key
               test
               hash-function
               (current-owner-id +default-owner+ explicit-p))
  "Return a new `history-tree'."
  (declare (ignore key test hash-function))
  (let ((owners (make-hash-table :test #'equalp))
        (initial-owner (make-instance 'owner)))
    (setf (gethash current-owner-id owners) initial-owner)
    (unless explicit-p
      (setf args (append
                  (list :current-owner-id current-owner-id)
                  args)))
    (apply #'make-instance 'history-tree
           :owners owners
           args)))

(export-always 'owner)
(declaim (ftype (function (history-tree t) (or null owner)) owner))
(defun owner (history owner-id)
  "Return the `owner' object identified by OWNER-IDENTIFIER in HISTORY."
  (gethash owner-id (owners history)))

(export-always 'add-owner)
(declaim (ftype (function (history-tree t &key (:creator-id t))
                          (values owner &optional))
                add-owner))
(defun add-owner (history owner-id &key creator-id)
  "Create and register owner object for OWNER-IDENTIFIER.
CREATOR-ID is the optional identifier of the parent owner.
Return the newly created owner.  If the owner with such identifier already
exists, return it and raise a warning."
  (let ((owner (owner history owner-id)))
    (if owner
        (progn
          (warn "Owner with identifier ~s already exists" owner-id)
          owner)
        (let ((creator-owner (owner history creator-id)))
          (when (and creator-id
                     (or (not creator-owner)
                         (not (current creator-owner))))
            (error "Cannot make owner a child of the node-less parent ~s"
                   creator-id))
          (let ((owner (make-instance 'owner
                                      :creator-id creator-id
                                      :creator-node (when creator-id
                                                      (current creator-owner)))))
            (setf (gethash owner-id (owners history))
                  owner)
            owner)))))

(export-always 'set-current-owner)
(defun set-current-owner (history owner-id)
  "Persistently switch owner for HISTORY.
OWNER-IDENTIFIER is arbitrary data representing an `owner'.
Raise an error when no matching owner exists.
See `add-owner' to create an owner.
See `with-current-owner' to set the owner locally.
See `delete-owner' to remove it from HISTORY."
  (if (owner history owner-id)
      (setf (slot-value history 'current-owner-id) owner-id)
      (error "Owner with identifier ~s does not exist" owner-id)))

(export-always 'current-owner)
(defun current-owner (history)
  (owner history (current-owner-id history)))

(export-always 'current-owner-node)
(defun current-owner-node (history)
  (current (current-owner history)))

(export-always 'with-current-owner)
(defmacro with-current-owner ((history owner-id) &body body)
  "Locally switch owner for HISTORY.
See `set-current-owner' to set the owner persistently.
OWNER-IDENTIFIER can be any value, even NIL."
  (let ((old-owner-id (gensym)))
    `(let ((,old-owner-id (current-owner-id ,history)))
       (unwind-protect (progn (set-current-owner ,history ,owner-id)
                              ,@body)
         (set-current-owner ,history ,old-owner-id)))))

(defmethod visit ((history history-tree) node)
  "Visit NODE with HISTORY's current owner.
Return (values HISTORY NODE) so that calls to `visit' can be chained."
  (let ((owner (current-owner history)))
    (pushnew node (nodes owner))        ; TODO: See TODO note on "fast sets".
    (setf (current owner) node)
    (let ((binding (gethash owner (bindings node))))
      (if binding
          (setf (last-access binding) (local-time:now))
          (setf (gethash owner (bindings node))
                (make-instance 'binding))))
    (values history node)))

(export-always 'visit-all)
(defmethod visit-all ((history history-tree) node)
  "Like `visit' but on all nodes between the current node and NODE.
This is only possible if the current node and NODE are on the same branch.
If they are not, an error is raised.
Return (values HISTORY NODE) so that calls to `visit' can be chained."
  (if (eq node (current-owner-node history))
      (values history node)
      (labels ((nodes-with-common-parent (node current-node-parents)
                 (unless node
                   (error "Node ~s and current owner node must be on the same branch" node))
                 (if (find node current-node-parents)
                     (list node)
                     (cons node (nodes-with-common-parent (parent node) current-node-parents)))))
        (let* ((current-node (current-owner-node history))
               (current-node-with-parents (cons current-node (all-parents current-node)))
               (node-parents-until-common-parent (nreverse (nodes-with-common-parent
                                                            node
                                                            current-node-with-parents)))
               (common-parent (first node-parents-until-common-parent)))
          (loop :until (eq common-parent (current-owner-node history))
                :do (backward history))
          (loop :until (eq node (current-owner-node history))
                ;; Skip the first node since it's the common-parent and it's already visited.
                :do (setf node-parents-until-common-parent (rest node-parents-until-common-parent))
                :do (go-to-child (data (first node-parents-until-common-parent)) history))
          (values history node)))))

(deftype positive-integer ()
  `(integer 1 ,most-positive-fixnum))

(export-always 'backward)
(defmethod backward ((history history-tree) &optional (count 1))
  "Go COUNT parent up from the current owner node, if possible.
Return (VALUES HISTORY CURRENT-NODE) so that `backward' and `forward' calls can
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
        (backward history (1- count))))
    (values history (current owner))))

(export-always 'backward-owned-parents)
(defmethod backward-owned-parents ((history history-tree) &optional (count 1))
  "Go COUNT parent up from the current owner node, if possible.
Only contiguous owned parents are considered.
Return (VALUES HISTORY CURRENT-NODE) so that `backward' and `forward' calls can
be chained."
  (let ((owner (current-owner history)))
    (when (owned-parent owner (current owner))
      (backward history count))
    (values history (current owner))))

(export-always 'forward)
(defmethod forward ((history history-tree) &optional (count 1))
  "Go COUNT forward-children down from the current owner node, if possible.
Return (values HISTORY CURRENT-NODE)) so that `backward' and `forward' calls can be
chained."
  (check-type count positive-integer)
  (let ((owner (current-owner history)))
    (when (and (current-binding owner)
               (forward-child (current-binding owner)))
      (visit history (forward-child (current-binding owner)))
      (when (< 1 count)
        (forward history (1- count))))
    (values history (current owner))))

(defun find-entry (history data)
  "Return the nodes matching DATA."
  (cl-custom-hash-table:with-custom-hash-table
    (let ((new-entry (make-entry history data)))
      (gethash new-entry (entries history)))))

(export 'find-nodes)
(defun find-nodes (history data)
  "Return the nodes matching DATA."
  (cl-custom-hash-table:with-custom-hash-table
    (let ((entry (find-entry history data)))
      (when entry
        (nodes entry)))))

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
(defmethod go-to-child (data (history history-tree) &key (child-finder #'find-child)) ; TODO: Should take a node instead?
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


(defun make-origin-node (history owner-id data)
  (let* ((owner (owner history owner-id))
         (new-node (make-node :entry (add-entry history data)
                              :parent (creator-node owner))))
    (when (creator-node owner)
      (push new-node (children (creator-node owner))))
    (setf (origin owner) new-node)
    (with-current-owner (history owner-id)
      (visit history new-node))))

(export-always 'add-child)
(defmethod add-child (data (history history-tree))
  "Create or find a node holding DATA and set current node to it.
Return the (possibly new) current node.

If current node matches DATA, then we update its data to DATA (since the
`history-tree''s `key' and `test' functions may identify two non-identical datum
as equal).

If DATA is found among the children, the current owner node `forward-child' is
set to the matching child, the child data is set to DATA and the owner current
node is set to this child.

If there is no current node, this creates the `origin' node of the current owner
and also sets `current' to it.  If the owner has a `creator-id' set,
the new node is added to the children of the current node of the creator."
  (let* ((owner (current-owner history)))
    (cond
      ((null (current owner))
       (make-origin-node history (current-owner-id history) data))

      ((not (data-equal-entry-p data (entry (current owner))))
       (let ((node (find-child data owner)))
         (if node
             (setf (data (entry node)) data)
             (let ((maybe-new-entry (add-entry history data)))
               (push (setf node (make-node :entry maybe-new-entry
                                           :parent (current owner)))
                     (children (current owner)))))
         (let ((binding (gethash owner (bindings (current owner)))))
           (setf (forward-child binding) node))
         (forward history)))

      (t
       (setf (data (entry (current owner))) data)))

    (current owner)))

(export 'add-children)
(defmethod add-children (children-data (history history-tree))
  "Add CHILDREN-DATA to the HISTORY `current-owner''s.
Each child is added with `add-child'.
Return the (maybe new) current node, which holds the last piece of data in
`children-data'."
  (let ((owner (current-owner history)))
    (add-child (first children-data) owner)
    (if (rest children-data)
        (add-children (rest children-data) (backward owner))
        (current owner))))

(export-always 'map-tree)
(defun map-tree (function tree &key flatten include-root
                                 (collect-function #'cons)
                                 (children-function #'children))
  "Map the FUNCTION over the TREE.
If TREE is a `htree:history-tree', start from its current branch root.
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
                  (history-tree (root (current-owner-node tree))))))
      (when root
        (if include-root
            (traverse root)
            (apply #'append (mapcar #'traverse (children root))))))))

(export-always 'map-owned-tree)
(defun map-owned-tree (function tree &key flatten include-root
                                       (collect-function #'cons)
                                       (owner (if (history-tree-p tree)
                                                  (current-owner tree)
                                                  (current-owner (history (entry tree))))))
  "Like `map-tree' but restrict traversal to OWNER's nodes."
  (map-tree function (if (history-tree-p tree)
                         (owned-root owner)
                         tree)
            :flatten flatten
            :include-root include-root
            :collect-function collect-function
            :children-function (owned-children-lister owner)))

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
  "Return a list of all the children of NODE, recursively."
  (map-tree #'identity node :flatten t))

(defmethod all-children ((history history-tree))
  "Return a list of all the children of HISTORY's current owner node.
Children may not all be owned by the current owner."
  (all-children (current-owner-node history)))

(export-always 'all-contiguous-owned-children)
(defmethod all-contiguous-owned-children ((history history-tree) &optional node)
  "Return a list of all the children of HISTORY's current owner node,
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
Parents may not be owned by the current owner.
First parent comes first in the resulting list."
  (all-parents (current-owner-node history)))

(defun node-contiguous-owned-parents (owner node)
  "Return a list of parents of owned by NODE, recursively.
First parent comes first in the resulting list."
  (labels ((contiguous-owned-parents (node)
             (when (owned-parent owner node)
               (cons (parent node)
                     (contiguous-owned-parents (parent node))))))
    (contiguous-owned-parents node)))

(export-always 'all-contiguous-owned-parents)
(defmethod all-contiguous-owned-parents ((history history-tree))
  "Return a list of parents of owned by HISTORY current owner node, recursively.
First parent comes first in the resulting list."
  (node-contiguous-owned-parents (current-owner history)
                                 (current-owner-node history)))

(export-always 'all-forward-children)
(defmethod all-forward-children ((history history-tree)
                                 &optional (node (current-owner-node history)))
  "Return a list of the forward children of NODE, recursively.
First child comes first in the resulting list."
  (let* ((owner (current-owner history))
         (binding (current-binding owner node)))
    (when (and binding (forward-child binding))
      (cons (forward-child binding)
            (all-forward-children history
                                  (forward-child binding))))))

(export 'all-current-owner-nodes)
(defmethod all-current-owner-nodes ((history history-tree))
  "Return a list of all current owner nodes, in unspecified order."
  (nodes (current-owner history)))

(export 'all-current-branch-nodes)
(defmethod all-current-branch-nodes ((history history-tree))
  "Return a list of all nodes that belong to the branch the current owner node is on.
These nodes do not necessarily belong to the current owner.
See `all-contiguous-owned-nodes'."
  (let ((current-node (current-owner-node history)))
    (when current-node
      (let ((root (root (current-owner-node history))))
        (cons root (all-children root))))))

(export 'owned-root)
(defun owned-root (owner)
  "Return the first parent among the contiguous owned parents of NODE."
  (or
   (first (last (node-contiguous-owned-parents owner (current owner))))
   (current owner)))

(export 'all-contiguous-owned-nodes)
(defmethod all-contiguous-owned-nodes ((history history-tree))
  "Return a list of all nodes contiguous to the current owner node, starting
from the top-most parent, in depth-first order."
  (let ((owned-root (owned-root (current-owner history))))
    (when owned-root
      (cons owned-root (all-contiguous-owned-children history owned-root)))))

(export-always 'all-data)
(defmethod all-data ((history history-tree))
  "Return a list of all entries data, in unspecified order."
  (mapcar #'data (alex:hash-table-keys (entries history))))

(defun map-data (arg)
  (mapcar #'data arg))



(defun branch-owners (node)
  "Return the list of all NODE's children (including NODE) owners."
  (let ((owners '()))
    (do-tree (child-node node)
      (alexandria:appendf owners (alexandria:hash-table-keys (bindings child-node))))
    (delete-duplicates owners)))

(defun disowned-branch-nodes (node)
  "Return true if all NODE's children (including NODE) are disowned.
Return nil otherwise.
As a second value, return the list of all NODE's children, including NODE."
  (let ((disowned? t)
        (children '()))
    (do-tree (child-node node)
      (unless (disowned-p child-node)
        (setf disowned? nil))
      (push child-node children))
    (values disowned? children)))

(defun delete-node (history node)
  (cl-custom-hash-table:with-custom-hash-table
    (let ((entry (gethash (entry node) (entries history))))
      (when entry
        (alex:deletef (nodes entry) node)))))

(defun delete-disowned-branch-nodes (history nodes)
  (labels ((garbage-collect (list-of-roots)
             (when list-of-roots
               (let ((node (first list-of-roots)))
                 (multiple-value-bind (disowned-branch? nodes)
                     (disowned-branch-nodes node)
                   (when disowned-branch?
                     (mapc (alex:curry #'delete-node history) nodes))))
               (garbage-collect (rest list-of-roots)))))
    ;; `delete-duplicates' ensures that no node is processed twice.
    (garbage-collect (delete-duplicates (mapcar #'root nodes)))))

(defun first-hash-table-key (hash-table)
  (with-hash-table-iterator (next-entry hash-table)
    (nth-value 1 (next-entry))))

(defun first-hash-table-value (hash-table) ; TODO: Unused
  (with-hash-table-iterator (next-entry hash-table)
    (nth-value 2 (next-entry))))

(defun disown-all (history owner)
  (when (owner-p owner)
    (let ((nodes (nodes owner)))
      (mapc (alex:curry #'disown owner) (nodes owner))
      ;; Delete nodes only when whole branch is owner-less.  Indeed, otherwise
      ;; we would lose information for other owners.  It's better to be as
      ;; "immutable" as possible.
      ;;
      ;; If we want to "free" disowned nodes from a branch with still owned
      ;; nodes, the less confusing approach (at least from a user perspective)
      ;; is delete all remaining owners, possibly by duplicating elsewhere
      ;; beforehand.
      (delete-disowned-branch-nodes history nodes))
    (setf (creator-node owner) nil)))

(export-always 'delete-owner)
(declaim (ftype (function (history-tree t) (or null owner)) delete-owner))
(defun delete-owner (history owner-id)
  "Delete `owner' corresponding to OWNER-IDENTIFIER from HISTORY.
For every branch `owner' has nodes on, remove all its nodes if the branch is
without any owner.
Return owner, or nil if there is no owner corresponding to OWNER-IDENTIFIER."
  (let ((owner (owner history owner-id)))
    (remhash owner-id (owners history))
    (when (equal owner-id (current-owner-id history))
      (setf (slot-value history 'current-owner-id)
            (first-hash-table-key (owners history))))
    (disown-all history owner)
    owner))

(export-always 'reset-owner)
(defun reset-owner (history owner-id)
  "Disown all OWNER's nodes and create a new root node with the previous current
node entry."
  (let* ((owner (owner history owner-id))
         (old-current-entry (entry (current owner))))
    (disown-all history owner)
    (make-origin-node history owner-id (data old-current-entry))
    owner))

(export-always 'delete-data)
(defun delete-data (history data)
  "Delete entry matching DATA from HISTORY.
If nodes are still associated to entry, do nothing."
  (cl-custom-hash-table:with-custom-hash-table
    (let ((nodes (find-nodes history data)))
      (unless nodes
        (let ((matching-entry (make-entry history data)))
          (remhash matching-entry (entries history)))))))



(deftype non-negative-integer ()
  `(integer 0 ,most-positive-fixnum))

(declaim (ftype (function (history-tree) non-negative-integer) depth))
(defun depth (history)
  "Return the number of (possibly unowned) parents of the current owner node."
  (length (all-parents history)))

(defmethod size ((owner owner))
  "Return the total number of nodes owned by OWNER."
  (length (nodes owner)))

(defmethod contiguous-size ((owner owner))
  "Return the total number of owned nodes contiguous to the current OWNER node."
  (length (all-contiguous-owned-nodes owner)))

(defmethod size ((history history-tree))
  "Return the total number of nodes for the current branch."
  ;; TODO: This could be optimized with a SIZE slot, but is it worth it?
  (length (all-current-branch-nodes history)))
