;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package history-tree-tests
  (:use #:common-lisp)
  (:import-from #:class-star #:define-class))
(in-package :history-tree-tests)

(prove:plan nil)

(defun make-tree1 ()
  (let ((tree (htree:make)))
    (dolist (url '(
                   "http://example.root"
                   "http://example.root/A"
                   "http://example.root/A1"))
      (htree:add-child url tree))
    (htree:back tree)
    (htree:add-child "http://example.root/A2" tree)
    (htree:back tree 2)
    (htree:add-child "http://example.root/B" tree)
    (htree:add-child "http://example.root/B1" tree)
    (htree:back tree)
    (htree:add-child "http://example.root/B2" tree)
    tree))

(defun make-tree2 ()
  (let ((tree (htree:make)))
    (htree:add-child "http://example.root" tree)
    (htree:add-child "http://example.root/A" tree)
    (htree:back tree)
    (htree:add-child "http://example.root/B" tree)
    tree))

(prove:subtest "Single entry"
  (let ((history (htree:make))
        (url "http://example.org" ))
    (htree:add-child url history)
    (prove:is (htree:value (htree:current-owner-node history))
              url)))

(prove:subtest "Multiple entry"
  (let ((history (htree:make))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org"))
    (htree:add-child url1 history)
    (htree:add-child url2 history)
    (htree:back history)
    (htree:add-child url3 history)
    (prove:is (htree:value (htree:current-owner-node history))
              url3)
    (prove:is (htree:value (htree:parent (htree:current-owner-node history)))
              url1)
    ;; TODO: Go to other child.
    ))

(prove:subtest "Simple branching tree tests."
  (prove:is (htree:value (htree:current-owner-node (make-tree1)))
            "http://example.root/B2"))

(prove:subtest "History depth."
  (prove:is (htree:depth (make-tree1))
            2))

(prove:subtest "History size."
  (prove:is (htree:size (make-tree1))
            7))

(prove:subtest "All contiguous history nodes for current owner."
  (prove:is (htree:all-contiguous-owned-nodes-data (make-tree1))
            '("http://example.root"
              "http://example.root/B"
              "http://example.root/B2" "http://example.root/B1"
              "http://example.root/A"
              "http://example.root/A2" "http://example.root/A1")))

(prove:subtest "Traverse all history."
  (prove:is (htree:all-contiguous-owned-nodes-data
             (htree:back (make-tree1)))
            '("http://example.root"
              "http://example.root/B"
              "http://example.root/B2" "http://example.root/B1"
              "http://example.root/A"
              "http://example.root/A2" "http://example.root/A1")))

(prove:subtest "Visiting other branches should not reorder the nodes."
  (prove:is (htree:all-contiguous-owned-nodes-data
             (htree:go-to-child
               "http://example.root/A2"
              (htree:go-to-child
               "http://example.root/A"
               (htree:back (make-tree1) 2))))
            '("http://example.root"
              "http://example.root/B"
              "http://example.root/B2" "http://example.root/B1"
              "http://example.root/A"
              "http://example.root/A2" "http://example.root/A1")))

(prove:subtest "Traverse parents."
  (prove:is (htree:parent-nodes-data
             (htree:back (make-tree1)))
            '("http://example.root")))

(prove:subtest "Traverse forward children."
  (prove:is (htree:forward-children-nodes-data
             (htree:back (make-tree1)))
            '("http://example.root/B2")))

(prove:subtest
    "Traverse all children."
  (prove:is (htree:children-nodes-data
             (htree:back (make-tree1)))
            '("http://example.root/B2" "http://example.root/B1")))

(prove:subtest
    "Move node to forward-child on add."
  (let ((tree (make-tree2)))
    (prove:is (htree:value (htree:current-owner-node tree))
              "http://example.root/B")
    (htree:back tree)
    (prove:is (htree:value (htree:current-owner-node tree))
              "http://example.root")
    (htree:add-child "http://example.root/A" tree)
    (prove:is (htree:value (htree:current-owner-node tree))
              "http://example.root/A")))

(define-class web-page ()
  ((url "")
   (title ""))
  (:accessor-name-transformer #'class*:name-identity))

(defun first-hash-table-key (hash-table)
  (with-hash-table-iterator (next-entry hash-table)
    (nth-value 1 (next-entry))))

(prove:subtest "Compound entry uniqueness"
  (let ((web-page1 (make-instance 'web-page :url "http://example.org"
                                            :title "Example page"))
        (web-page2 (make-instance 'web-page :url "http://example.org"
                                            :title "Same page, another title")))
    (let ((history (htree:make :key #'url)))
      (htree:add-child web-page1 history)
      (htree:add-child web-page2 history)
      (prove:is (hash-table-count (htree:entries history))
                1)
      (prove:is (title (htree:value (first-hash-table-key (htree:entries history))))
                "Same page, another title"))
    (let ((history (htree:make)))
      (htree:add-child web-page1 history)
      (htree:add-child web-page2 history)
      (prove:is (hash-table-count (htree:entries history))
                2)
      (prove:is (sort (loop for key being the hash-keys in (htree:entries history)
                            collect (title (htree:value key)))
                      #'string<)
                (sort (mapcar #'title (list web-page1 web-page2)) #'string<)))))

(prove:subtest "Multiple owners"
  (let ((history (htree:make :current-owner-identifier "a"))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org"))
    (htree:add-child url1 history)
    (htree:set-current-owner history "b")
    (htree:add-child url2 history)
    (htree:add-child url3 history)
    (prove:is (hash-table-count (htree:entries history))
              3)
    (prove:is (htree:value (htree:current-owner-node history))
              url3)
    (prove:is (htree:value (htree:parent (htree:current-owner-node history)))
              url2)
    (prove:is (htree:parent (htree:parent (htree:current-owner-node history)))
              nil)
    (prove:is (length (htree:nodes (htree:current-owner history)))
              2)
    (prove:is (htree:with-current-owner (history "a")
                (htree:value (htree:current-owner-node history)))
              url1)))

(prove:finalize)
