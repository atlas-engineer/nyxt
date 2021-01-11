;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package history-tree
  (:nicknames #:htree)
  (:use #:common-lisp)
  (:import-from #:class-star #:define-class)
  (:documentation "Library for the global history tree data structure.

This data structure can be used to store the history of visited paths with a file
or web browser, in a way that no \"forward\" element is ever forgotten.

A history tree is a general tree that keeps a pointer to the \"current node\".
All nodes have values.

When the current node is moved to point to the parent, the previous current node
is marked as the \"forward child\".  This allows us to know to which node to go
forward to, when a node has multiple children.

When creating a child, if its data is the same as the current one then we do
nothing since this would not add any meaningful information, it would only
clutter the history tree.

The history tree is \"global\" in the sense that multiple owners (e.g. tabs,
buffers) can have overlapping histories.
On top of that, an owner can spawn another one, _starting from one of its nodes_
(typically when you open a URL in a new tab).  The global history tree reifies
all this.

Owners can jump to non-contiguous nodes, even to nodes that belong to other
unrelated owners.  In that sense, owners may own disjoint branches.



Consider the following tree:

(X-A
  (X-B1
     (X-C1 X-C2))
  (X-B2
     (X-D1 Y-D2)))

The X-prefixed nodes belong to owner X, while the Y-ones belong to Y.
X current node may be X-B2, while Y current node may be Y-D2.

X is said to be the `creator' of Y, and Y-D2 is the `origin' node of Y.
Y owns only 1 node, while X owns 6 nodes.
None of them overlaps, but wait.

With current owner being X, if we go forward to Y-D2, then we own it and it
becomes the `current' node.  Now Y-D2 is owned both by X and Y.

Similarly, Y can \"go back\" to X-B2, which becomes its `current' node, while
Y-D2 becomes the `forward-child' of X-B2 _for_ X.

Now if Y goes to X-D1, it owns it as well and X-D1 becomes the `forward-child'
of X-B2 for Y.  Observe that each node may have different `forward-child'ren for
each of their owners."))
