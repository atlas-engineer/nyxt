;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package history-tree-tests
  (:use #:common-lisp)
  (:import-from #:class-star #:define-class))
(in-package :history-tree-tests)

(prove:plan nil)

(defun make-history1 ()
  (let ((history (htree:make)))
    (dolist (url '(
                   "http://example.root"
                   "http://example.root/A"
                   "http://example.root/A1"))
      (htree:add-child url history))
    (htree:backward history)
    (htree:add-child "http://example.root/A2" history)
    (htree:backward history 2)
    (htree:add-child "http://example.root/B" history)
    (htree:add-child "http://example.root/B1" history)
    (htree:backward history)
    (htree:add-child "http://example.root/B2" history)
    history))

(defun make-history2 ()
  (let ((history (htree:make)))
    (htree:add-child "http://example.root" history)
    (htree:add-child "http://example.root/A" history)
    (htree:backward history)
    (htree:add-child "http://example.root/B" history)
    history))

(prove:subtest "Single entry"
  (let ((history (htree:make))
        (url "http://example.org" ))
    (htree:add-child url history)
    (prove:is (htree:data (htree:current-owner-node history))
              url)))

(prove:subtest "Multiple entry"
  (let ((history (htree:make))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org"))
    (htree:add-child url1 history)
    (htree:add-child url2 history)
    (htree:backward history)
    (htree:add-child url3 history)
    (prove:is (htree:data (htree:current-owner-node history))
              url3)
    (prove:is (htree:data (htree:parent (htree:current-owner-node history)))
              url1)
    (htree:backward history)
    (htree:go-to-child url2 history)
    (prove:is (htree:data (htree:current-owner-node history))
              url2)))

(prove:subtest "Simple branching tree tests."
  (prove:is (htree:data (htree:current-owner-node (make-history1)))
            "http://example.root/B2"))

(prove:subtest "History depth."
  (prove:is (htree::depth (make-history1))
            2))

(prove:subtest "History size."
  (prove:is (htree::size (make-history1))
            7))

(prove:subtest "All forward children"
  (let ((history (make-history1)))
    (htree:backward history 2)
    (prove:is (htree::map-data (htree:all-forward-children history))
              '("http://example.root/B"
                "http://example.root/B2"))))

(prove:subtest "All contiguous history nodes for current owner."
  (prove:is (htree::map-data (htree:all-contiguous-owned-nodes (make-history1)))
            '("http://example.root"
              "http://example.root/B"
              "http://example.root/B2" "http://example.root/B1"
              "http://example.root/A"
              "http://example.root/A2" "http://example.root/A1")))

(prove:subtest "Traverse all history."
  (prove:is (htree::map-data (htree:all-contiguous-owned-nodes
                              (htree:backward (make-history1))))
            '("http://example.root"
              "http://example.root/B"
              "http://example.root/B2" "http://example.root/B1"
              "http://example.root/A"
              "http://example.root/A2" "http://example.root/A1")))

(prove:subtest "Visiting other branches should not reorder the nodes."
  (prove:is (htree::map-data (htree:all-contiguous-owned-nodes
                              (htree:go-to-child
                               "http://example.root/A2"
                               (htree:go-to-child
                                "http://example.root/A"
                                (htree:backward (make-history1) 2)))))
            '("http://example.root"
              "http://example.root/B"
              "http://example.root/B2" "http://example.root/B1"
              "http://example.root/A"
              "http://example.root/A2" "http://example.root/A1")))

(prove:subtest "Traverse parents."
  (prove:is (htree::map-data (htree:all-parents
                              (htree:backward (make-history1))))
            '("http://example.root")))

(prove:subtest "Traverse forward children."
  (prove:is (htree::map-data (htree:all-forward-children
                              (htree:backward (make-history1))))
            '("http://example.root/B2")))

(prove:subtest "Traverse all children."
  (prove:is (htree::map-data (htree:all-children
                              (htree:backward (make-history1))))
            '("http://example.root/B2" "http://example.root/B1")))

(prove:subtest "Move node to forward-child on add."
  (let ((history (make-history2)))
    (prove:is (htree:data (htree:current-owner-node history))
              "http://example.root/B")
    (htree:backward history)
    (prove:is (htree:data (htree:current-owner-node history))
              "http://example.root")
    (htree:add-child "http://example.root/A" history)
    (prove:is (htree:data (htree:current-owner-node history))
              "http://example.root/A")))

(define-class web-page ()
  ((url "")
   (title ""))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(prove:subtest "Compound entry uniqueness"
  (let ((web-page1 (make-instance 'web-page :url "http://example.org"
                                            :title "Example page"))
        (web-page2 (make-instance 'web-page :url "http://example.org"
                                            :title "Same page, another title")))
    (let ((history (htree:make :key 'url)))
      (htree:add-child web-page1 history)
      (htree:add-child web-page2 history)
      (prove:is (hash-table-count (htree:entries history))
                1)
      (prove:is (title (htree:data (htree::first-hash-table-key (htree:entries history))))
                "Same page, another title"))
    (let ((history (htree:make :key 'url)))
      (htree:add-child web-page1 history)
      (htree:add-owner history "b")
      (htree:set-current-owner history "b")
      (htree:add-child web-page2 history)
      (prove:is (hash-table-count (htree:entries history))
                1)
      (prove:is (title (htree:data (htree::first-hash-table-key (htree:entries history))))
                "Same page, another title"))
    (let ((history (htree:make)))
      (htree:add-child web-page1 history)
      (htree:add-child web-page2 history)
      (prove:is (hash-table-count (htree:entries history))
                2)
      (prove:is (sort (loop for key being the hash-keys in (htree:entries history)
                            collect (title (htree:data key)))
                      #'string<)
                (sort (mapcar #'title (list web-page1 web-page2)) #'string<)))))

(prove:subtest "Single owners"
  (let ((history (htree:make))
        (url1 "http://example.org"))
    (htree:add-child url1 history)
    (prove:is (htree:current-owner-id history)
              htree:+default-owner+)
    (prove:is (hash-table-count (htree:owners history))
              1)))

(prove:subtest "Multiple owners"
  (let ((history (htree:make :current-owner-id "a"))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org"))
    (htree:add-child url1 history)
    (htree:add-owner history "b")
    (htree:set-current-owner history "b")
    (htree:add-child url2 history)
    (htree:add-child url3 history)
    (prove:is (hash-table-count (htree:entries history))
              3)
    (prove:is (htree:data (htree:current-owner-node history))
              url3)
    (prove:is (htree:data (htree:parent (htree:current-owner-node history)))
              url2)
    (prove:is (htree:parent (htree:parent (htree:current-owner-node history)))
              nil)
    (prove:is (length (htree:nodes (htree:current-owner history)))
              2)
    (prove:is (htree:with-current-owner (history "a")
                (htree:data (htree:current-owner-node history)))
              url1)))

(prove:subtest "Backward and forward"
  (let ((history (htree:make :current-owner-id "a"))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org"))
    (htree:add-child url1 history)
    (htree:add-child url2 history)
    (htree:add-owner history "b" :creator-id "a")
    (htree:set-current-owner history "b")
    (htree:add-child url3 history)
    (htree:set-current-owner history "a")
    (prove:is (htree:data (htree:current-owner-node history))
              url2)
    (htree:backward history)
    (htree:backward history)
    (prove:is (htree:data (htree:current-owner-node history))
              url1)
    (htree:forward history)
    (prove:is (htree:data (htree:current-owner-node history))
              url2)
    (htree:forward history)
    (prove:is (htree:data (htree:current-owner-node history))
              url2)))

(prove:subtest "Inter-owner relationships"
  (let ((history (htree:make :current-owner-id "a"))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer"))
    (htree:add-child url1 history)
    (htree:add-owner history "b" :creator-id "a")
    (htree:set-current-owner history "b")
    (htree:add-child url2 history)
    (prove:is (length (htree:nodes (htree:owner history "a")))
              1)
    (prove:is (length (htree:nodes (htree:owner history "b")))
              1)
    (prove:is (htree:parent (htree:current (htree:owner history "b")))
              (htree:current (htree:owner history "a")))))

;; Default owner has its own branch, then we make 2 owners on a 1 separate branch.
;; 1. Remove 1 owner from non-default branch.  Test if all nodes are still there.
;; 2. Remove 2nd owner.  Test if all these nodes got garbage collected, but not
;; the default branch nodes.
(prove:subtest "Owner deletion"
  (let ((history (htree:make)))
    (dolist (url '("http://example.root"
                   "http://example.root/R"
                   "http://example.root/R1"
                   "http://example.root/R2"))
      (htree:add-child url history))
    (htree:add-owner history "parent-owner")
    (htree:set-current-owner history "parent-owner")
    (htree:add-child "http://parent/A" history)
    (htree:add-child "http://parent/A1" history)
    (htree:backward history)
    (htree:add-child "http://parent-child/A2" history)

    (htree:add-owner history "child-owner" :creator-id "parent-owner")
    (htree:set-current-owner history "child-owner")
    (htree:add-child "http://child/A3a" history)
    (htree:backward history)
    (htree:add-child "http://child/A3b" history)

    (prove:is (length (htree:nodes (htree:owner history "parent-owner")))
              3)
    (prove:is (length (htree:all-current-branch-nodes history))
              5)
    (prove:is (length (htree:nodes (htree:owner history htree:+default-owner+)))
              4)

    (htree:delete-owner history "child-owner")

    (prove:isnt (htree:current-owner-id history)
                "child-owner")
    (prove:is (htree:owner history "child-owner")
              nil)
    (prove:is (length (htree:nodes (htree:owner history "parent-owner")))
              3)
    (htree:set-current-owner history "parent-owner")
    (prove:is (length (htree:all-current-branch-nodes history))
              5)
    (prove:is (length (htree:nodes (htree:owner history htree:+default-owner+)))
              4)

    (dolist (url-owner (list '("http://parent/A" "parent-owner")
                             '("http://parent/A1" "parent-owner")
                             '("http://parent-child/A2" "parent-owner")
                             '("http://child/A3a" nil)
                             '("http://child/A3b" nil)))
      (prove:is (alexandria:hash-table-keys
                 (htree:bindings (first (htree:find-nodes history (first url-owner)))))
                (if (second url-owner)
                    (list (htree:owner history (second url-owner)))
                    nil)))

    (htree:delete-owner history "parent-owner")
    (prove:is (htree:current-owner-id history)
              htree:+default-owner+)
    (prove:is (htree:owner history "parent-owner")
              nil)
    (prove:is (hash-table-count (htree:entries history))
              9)

    (maphash (lambda (entry entry-accessors)
               (prove:is (length (htree:nodes entry-accessors))
                         (if (str:contains? "example.root" (htree:data entry))
                             1
                             0)
                         (format nil "~a entry has ~a remaining nodes"
                                 (htree:data entry)
                                 (length (htree:nodes entry-accessors)))))

             (htree:entries history))))

(prove:subtest "Visit all nodes until distant node"
  (let* ((history (make-history1))
         (creator (htree:current-owner-id history))
         (distant-node-value "http://example.root/A1")
         (distant-node (first (htree:find-nodes history distant-node-value))))
    (htree:add-owner history "b" :creator-id creator)
    (htree:set-current-owner history "b")
    (htree:add-child "b-data" history)

    (htree:visit-all history distant-node)

    (prove:is (htree:data (htree:current-owner-node history))
              distant-node-value)
    (prove:is (sort (mapcar #'htree:data (htree:nodes (htree:owner history "b")))
                    #'string<)
              (sort (copy-seq '("http://example.root/A1" "http://example.root/A" "http://example.root"
                                "http://example.root/B" "http://example.root/B2" "b-data"))
                    #'string<)))
  (let* ((history (make-history1))
         (creator (htree:current-owner-id history))
         (distant-node-value "b-data"))
    (htree:add-owner history "b" :creator-id creator)
    (htree:set-current-owner history "b")
    (htree:add-child distant-node-value history)

    (htree:set-current-owner history htree:+default-owner+)
    (htree:backward history 2)

    (let ((distant-node (first (htree:find-nodes history distant-node-value))))
      (htree:visit-all history distant-node))

    (prove:is (htree:data (htree:current-owner-node history))
              distant-node-value)
    (prove:is (sort (mapcar #'htree:data (htree:nodes (htree:owner history htree:+default-owner+)))
                    #'string<)
              (sort (copy-seq '("http://example.root/A"
                                "http://example.root/A1"
                                "http://example.root/A2"
                                "http://example.root"
                                "http://example.root/B"
                                "http://example.root/B1"
                                "http://example.root/B2"
                                "b-data"))
                    #'string<))))

(prove:subtest "Last access test"
  (let ((history (htree:make :current-owner-id "a"))
        (url1 "http://example.org"))
    (htree:add-child url1 history)
    (sleep 0.1)
    (htree:add-owner history "b")
    (htree:set-current-owner history "b")
    (htree::visit history (first (htree:find-nodes history url1)))

    (let ((a-access (htree:last-access (htree:current-binding
                                        (htree:owner history "a"))))
          (b-access (htree:last-access (htree:current-binding
                                        (htree:current-owner history)))))
      (prove:ok (local-time:timestamp/= a-access b-access))
      (prove:ok (local-time:timestamp= b-access
                                       (htree:last-access (htree:current-owner-node history))))
      (htree:set-current-owner history "a")
      (prove:ok (local-time:timestamp= b-access
                                       (htree:last-access (htree:current-owner-node history))))
      (prove:ok (local-time:timestamp= b-access
                                       (htree:data-last-access history url1))))))

(prove:subtest "Entry deletion"
  (let ((history (htree:make :current-owner-id "a"))
        (url1 "http://example.org")
        (url2 "http://other.example.org"))
    (htree:add-child url1 history)
    (htree:add-owner history "b")
    (htree:set-current-owner history "b")
    (htree:add-child url2 history)
    (htree:delete-owner history "a")
    (prove:is (hash-table-count (htree:entries history))
              2)
    (htree:delete-data history url1)
    (prove:is (hash-table-count (htree:entries history))
              1)
    (htree:delete-data history url2)
    (prove:is (hash-table-count (htree:entries history))
              1)))

(prove:subtest "Reset owner"
  (let ((history (htree:make :current-owner-id "a"))
        (url1 "http://example.org")
        (url2 "http://other.example.org")
        (url3 "http://alt.example.org")
        (url4 "http://more.example.org")
        (url5 "http://final.example.org"))
    (htree:add-child url1 history)
    (htree:add-child url2 history)
    (htree:reset-owner history "a")
    (prove:is (mapcar #'htree:data (htree:all-contiguous-owned-nodes history))
              (list url2))
    (prove:is (htree:parent (htree:current-owner-node history))
              nil)

    (htree:add-owner history "b")
    (htree:set-current-owner history "b")
    (htree:add-child url3 history)
    (htree:add-child url4 history)
    (htree:add-child url5 history)
    (htree:backward history)
    (htree:set-current-owner history "a")
    (htree:reset-owner history "b")

    (htree:set-current-owner history "b")
    (prove:is (mapcar #'htree:data (htree:all-contiguous-owned-nodes history))
              (list url4))
    (prove:is (htree:parent (htree:current-owner-node history))
              nil)))

(prove:finalize)
