;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package history-tree
  (:nicknames #:htree)
  (:use #:common-lisp)
  (:import-from #:class-star #:define-class)
  ;; TODO: Update documentation to reflect the Global History Tree structure.
  (:documentation "Library for the history-tree data structure.

This data structure can be used to store the history of visited path with a file
or web browser, in a way that no \"forward\" element is ever forgotten.

A history tree is a general tree that keeps a pointer to the \"current node\".
All nodes have values.

When the current node is moved to point to the parent, the previous current node
is placed first in the children list.  This allows us to backtrack to down the
previously visited children.

Considered the following tree:

(A
  (B1
     (C1 C2))
  (B2
     (D1 D2)))

If the current node is moved from D2 to B2, the tree becomes

(A
  (B1
     (C1 C2))
  (B2
     (D2 D1)))

Then from B2 to A:

(A
  (B2
     (D2 D1))
  (B1
     (C1 C2)))

This way, by follow the first children recursively, we can go back to B2, then
C2.

The direct children of a given node are systematically deduplicated: when adding
a child that already exists, it is moved to the front.

The creation of a child whose data is the same as a current node is a no-op."))
