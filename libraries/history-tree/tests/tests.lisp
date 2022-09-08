;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package history-tree/tests
  (:use #:common-lisp #:lisp-unit2)
  (:import-from #:class-star #:define-class))
(in-package :history-tree/tests)

(defvar *owner* "test-owner")

(defun make (&key (owner *owner*) (key 'identity))
  (htree:make :initial-owners (list owner) :key key))

(defun make-history1 ()
  (let ((history (make)))
    (dolist (url '("http://example.root"
                   "http://example.root/A"
                   "http://example.root/A1"))
      (htree:add-child url history *owner*))
    (htree:backward history *owner*)
    (htree:add-child "http://example.root/A2" history *owner*)
    (htree:backward history *owner* 2)
    (htree:add-child "http://example.root/B" history *owner*)
    (htree:add-child "http://example.root/B1" history *owner*)
    (htree:backward history *owner*)
    (htree:add-child "http://example.root/B2" history *owner*)
    history))

(defun make-history2 ()
  (let ((history (make)))
    (htree:add-child "http://example.root" history *owner*)
    (htree:add-child "http://example.root/A" history *owner*)
    (htree:backward history *owner*)
    (htree:add-child "http://example.root/B" history *owner*)
    history))

(define-test single-entry ()
  (let ((history (make))
        (url "http://example.org"))
    (htree:add-child url history *owner*)
    (assert-string= url
                    (htree:data (htree:owner-node history *owner*)))))

(define-test multiple-entry ()
  (let ((history (make))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org"))
    (htree:add-child url1 history *owner*)
    (htree:add-child url2 history *owner*)
    (htree:backward history *owner*)
    (htree:add-child url3 history *owner*)
    (assert-string= url3
                    (htree:data (htree:owner-node history *owner*)))
    (assert-string= url1
                    (htree:data (htree:parent (htree:owner-node history *owner*))))
    (htree:backward history *owner*)
    (htree:go-to-child url2 history *owner*)
    (assert-string= url2
                    (htree:data (htree:owner-node history *owner*)))))

(define-test simple-branching-tree-tests ()
  (assert-string= "http://example.root/B2"
                  (htree:data (htree:owner-node (make-history1) *owner*))))

(define-test history-depth ()
  (assert-eq 2
             (htree::depth (make-history1) *owner*)))

(define-test history-size ()
  (assert-eq 7
             (htree::size (make-history1) :owner *owner*)))

(define-test all-forward-children ()
  (let ((history (make-history1)))
    (htree:backward history *owner* 2)
    (assert-equal '("http://example.root/B"
                    "http://example.root/B2")
                  (htree::map-data (htree:all-forward-children history *owner*)))))

(define-test all-contiguous-history-nodes-for-current-owner ()
  (assert-equal '("http://example.root"
                  "http://example.root/B"
                  "http://example.root/B2" "http://example.root/B1"
                  "http://example.root/A"
                  "http://example.root/A2" "http://example.root/A1")
                (htree::map-data (htree:all-contiguous-owned-nodes (make-history1)
                                                                   *owner*))))

(define-test traverse-all-history ()
  (assert-equal '("http://example.root"
                  "http://example.root/B"
                  "http://example.root/B2" "http://example.root/B1"
                  "http://example.root/A"
                  "http://example.root/A2" "http://example.root/A1")
                (htree::map-data (htree:all-contiguous-owned-nodes
                                  (htree:backward (make-history1) *owner*)
                                  *owner*))))

(define-test visiting-other-branches-should-not-reorder-the-nodes ()
  (assert-equal '("http://example.root"
                  "http://example.root/B"
                  "http://example.root/B2" "http://example.root/B1"
                  "http://example.root/A"
                  "http://example.root/A2" "http://example.root/A1")
                (htree::map-data (htree:all-contiguous-owned-nodes
                                  (apply #'htree:go-to-child
                                         "http://example.root/A2"
                                         (multiple-value-list
                                          (htree:go-to-child
                                           "http://example.root/A"
                                           (htree:backward (make-history1) *owner* 2)
                                           *owner*)))
                                  *owner*))))

(define-test traverse-parents ()
  (assert-equal '("http://example.root")
                (htree::map-data (htree:all-parents
                                  (htree:backward (make-history1) *owner*)
                                  :owner *owner*))))

(define-test traverse-forward-children ()
  (assert-equal '("http://example.root/B2")
                (htree::map-data (htree:all-forward-children
                                  (htree:backward (make-history1) *owner*)
                                  *owner*))))

(define-test traverse-all-children ()
  (assert-equal '("http://example.root/B2" "http://example.root/B1")
                (htree::map-data (htree:all-children
                                  (htree:backward (make-history1) *owner*)
                                  :owner *owner*))))

(define-test move-node-to-forward-child-on-add ()
  (let ((history (make-history2)))
    (assert-string= "http://example.root/B"
                    (htree:data (htree:owner-node history *owner*)))
    (htree:backward history *owner*)
    (assert-string= "http://example.root"
                    (htree:data (htree:owner-node history *owner*)))
    (htree:add-child "http://example.root/A" history *owner*)
    (assert-string= "http://example.root/A"
                    (htree:data (htree:owner-node history *owner*)))))

(define-class web-page ()
  ((url "")
   (title ""))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-test compound-entry-uniqueness ()
  (let ((web-page1 (make-instance 'web-page :url "http://example.org"
                                            :title "Example page"))
        (web-page2 (make-instance 'web-page :url "http://example.org"
                                            :title "Same page, another title")))
    (let ((history (make :key 'url)))
      (htree:add-child web-page1 history *owner*)
      (htree:add-child web-page2 history *owner*)
      (assert-eq 1
                 (hash-table-count (htree:entries history)))
      (assert-string= "Example page"
                      (title (htree:data (htree::first-hash-table-key (htree:entries history))))))
    (let ((history (make :key 'url)))
      (htree:add-child web-page1 history *owner*)
      (htree:add-owner history "b")
      (htree:add-child web-page2 history "b")
      (assert-eq 1
                 (hash-table-count (htree:entries history)))
      (assert-string= "Example page"
                      (title (htree:data (htree::first-hash-table-key (htree:entries history))))))
    (let ((history (make)))
      (htree:add-child web-page1 history *owner*)
      (htree:add-child web-page2 history *owner*)
      (assert-eq 2
                 (hash-table-count (htree:entries history)))
      (assert-equal (sort (mapcar #'title (list web-page1 web-page2)) #'string<)
                    (sort (loop for key being the hash-keys in (htree:entries history)
                                collect (title (htree:data key)))
                          #'string<)))))

(define-test single-owners ()
  (let ((history (make))
        (url1 "http://example.org"))
    (htree:add-child url1 history *owner*)
    (assert-string= *owner*
                    (first (alexandria:hash-table-keys (htree:owners history))))
    (assert-eq 1
               (hash-table-count (htree:owners history)))))

(define-test multiple-owners ()
  (let ((history (make :owner "a"))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org"))
    (htree:add-child url1 history "a")
    (htree:add-owner history "b")
    (htree:add-child url2 history "b")
    (htree:add-child url3 history "b")
    (assert-eq 3
               (hash-table-count (htree:entries history)))
    (assert-string= url3
                    (htree:data (htree:owner-node history "b")))
    (assert-string= url2
                    (htree:data (htree:parent (htree:owner-node history "b"))))
    (assert-false (htree:parent (htree:parent (htree:owner-node history "b"))))
    ;; Following tests are useless now that we don't have with-current-owner anymore.
    (assert-eq 2
               (length (htree:nodes (htree:owner history "b"))))
    (assert-string= url1
                    (htree:data (htree:owner-node history "a")))))

(define-test backward-and-forward ()
  (let ((history (make :owner "a"))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org"))
    (htree:add-child url1 history "a")
    (htree:add-child url2 history "a")
    (htree:add-owner history "b" :creator-id "a")
    (htree:add-child url3 history "b")
    (assert-string= url2
                    (htree:data (htree:owner-node history "a")))
    (htree:backward history "a")
    (htree:backward history "a")
    (assert-string= url1
                    (htree:data (htree:owner-node history "a")))
    (htree:forward history "a")
    (assert-string= url2
                    (htree:data (htree:owner-node history "a")))
    (htree:forward history "a")
    (assert-string= url2
                    (htree:data (htree:owner-node history "a")))))

(define-test inter-owner-relationships ()
  (let ((history (make :owner "a"))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer"))
    (htree:add-child url1 history "a")
    (htree:add-owner history "b" :creator-id "a")
    (htree:add-child url2 history "b")
    (assert-eq 1
               (length (htree:nodes (htree:owner history "a"))))
    (assert-eq 1
               (length (htree:nodes (htree:owner history "b"))))
    (assert-eq (htree:current (htree:owner history "a"))
               (htree:parent (htree:current (htree:owner history "b"))))))

;; Default owner has its own branch, then we make 2 owners on a 1 separate branch.
;; 1. Remove 1 owner from non-default branch.  Test if all nodes are still there.
;; 2. Remove 2nd owner.  Test if all these nodes got garbage collected, but not
;; the default branch nodes.
(define-test owner-deletion ()
  (let ((history (make)))
    (dolist (url '("http://example.root"
                   "http://example.root/R"
                   "http://example.root/R1"
                   "http://example.root/R2"))
      (htree:add-child url history *owner*))
    (htree:add-owner history "parent-owner")
    (htree:add-child "http://parent/A" history "parent-owner")
    (htree:add-child "http://parent/A1" history "parent-owner")
    (htree:backward history "parent-owner")
    (htree:add-child "http://parent-child/A2" history "parent-owner")

    (htree:add-owner history "child-owner" :creator-id "parent-owner")
    (htree:add-child "http://child/A3a" history "child-owner")
    (htree:backward history "child-owner")
    (htree:add-child "http://child/A3b" history "child-owner")

    (assert-eq 3
               (length (htree:nodes (htree:owner history "parent-owner"))))
    (assert-eq 5
               (length (htree:all-branch-nodes history "child-owner")))
    (assert-eq 4
               (length (htree:nodes (htree:owner history *owner*))))

    (htree:delete-owner history "child-owner")

    (assert-false (htree:owner history "child-owner"))
    (assert-eq 3
               (length (htree:nodes (htree:owner history "parent-owner"))))
    (assert-eq 5
               (length (htree:all-branch-nodes history "parent-owner")))
    (assert-eq 4
               (length (htree:nodes (htree:owner history *owner*))))

    (dolist (url-owner (list '("http://parent/A" "parent-owner")
                             '("http://parent/A1" "parent-owner")
                             '("http://parent-child/A2" "parent-owner")
                             '("http://child/A3a" nil)
                             '("http://child/A3b" nil)))
      (if (second url-owner)
          (assert-equal (list (htree:owner history (second url-owner)))
                        (alexandria:hash-table-keys
                         (htree:bindings (first (htree:find-nodes history (first url-owner))))))
          (assert-false (alexandria:hash-table-keys
                         (htree:bindings (first (htree:find-nodes history (first url-owner))))))))

    (htree:delete-owner history "parent-owner")
    (assert-false (htree:owner history "parent-owner"))
    (assert-eq 9
               (hash-table-count (htree:entries history)))

    (maphash (lambda (entry entry-accessors)
               (assert-eq (if (str:contains? "example.root" (htree:data entry))
                              1
                              0)
                          (length (htree:nodes entry-accessors))))
             (htree:entries history))))

(define-test visit-all-nodes-until-distant-node ()
  (let* ((history (make-history1))
         (creator *owner*)
         (distant-node-value "http://example.root/A1")
         (distant-node (first (htree:find-nodes history distant-node-value))))
    (htree:add-owner history "b" :creator-id creator)
    (htree:add-child "b-data" history "b")

    (htree:visit-all history "b" distant-node)

    (assert-string= distant-node-value
                    (htree:data (htree:owner-node history "b")))
    (assert-equal (sort (copy-seq '("http://example.root/A1" "http://example.root/A" "http://example.root"
                                    "http://example.root/B" "http://example.root/B2" "b-data"))
                        #'string<)
                  (sort (mapcar #'htree:data (htree:nodes (htree:owner history "b")))
                        #'string<)))
  (let* ((history (make-history1))
         (creator *owner*)
         (distant-node-value "b-data"))
    (htree:add-owner history "b" :creator-id creator)
    (htree:add-child distant-node-value history "b")

    (htree:backward history *owner* 2)

    (let ((distant-node (first (htree:find-nodes history distant-node-value))))
      (htree:visit-all history *owner* distant-node))

    (assert-string= distant-node-value
                    (htree:data (htree:owner-node history *owner*)))
    (assert-equal (sort (copy-seq '("http://example.root/A"
                                    "http://example.root/A1"
                                    "http://example.root/A2"
                                    "http://example.root"
                                    "http://example.root/B"
                                    "http://example.root/B1"
                                    "http://example.root/B2"
                                    "b-data"))
                        #'string<)
                  (sort (mapcar #'htree:data (htree:nodes (htree:owner history *owner*)))
                        #'string<))))

(define-test last-access-test ()
  (let ((history (make :owner "a"))
        (url1 "http://example.org"))
    (htree:add-child url1 history "a")
    (sleep 0.1)
    (htree:add-owner history "b")
    (htree::visit history "b" (first (htree:find-nodes history url1)))

    (let ((a-access (htree:last-access (htree:current-binding
                                        (htree:owner history "a"))))
          (b-access (htree:last-access (htree:current-binding
                                        (htree:owner history "b")))))
      (assert-true (local-time:timestamp/= a-access b-access))
      (assert-true (local-time:timestamp= b-access
                                          (htree:last-access (htree:owner-node history
                                                                               "b"))))
      (assert-true (local-time:timestamp= b-access
                                          (htree:last-access (htree:owner-node history
                                                                               "a"))))
      (assert-true (local-time:timestamp= b-access
                                          (htree:data-last-access history url1))))))

(define-test entry-deletion ()
  (let ((history (make :owner "a"))
        (url1 "http://example.org")
        (url2 "http://other.example.org"))
    (htree:add-child url1 history "a")
    (htree:add-owner history "b")
    (htree:add-child url2 history "b")
    (htree:delete-owner history "a")
    (assert-eq 2
               (hash-table-count (htree:entries history)))
    (htree:delete-data history url1)
    (assert-eq 1
               (hash-table-count (htree:entries history)))
    (htree:delete-data history url2)
    (assert-eq 1
               (hash-table-count (htree:entries history)))))

(define-test reset-owner ()
  (let ((history (make :owner "a"))
        (url1 "http://example.org")
        (url2 "http://other.example.org")
        (url3 "http://alt.example.org")
        (url4 "http://more.example.org")
        (url5 "http://final.example.org"))
    (htree:add-child url1 history "a")
    (htree:add-child url2 history "a")
    (htree:reset-owner history "a")
    (assert-equal (list url2)
                  (mapcar #'htree:data (htree:all-contiguous-owned-nodes history "a")))
    (assert-false (htree:parent (htree:owner-node history "a")))

    (htree:add-owner history "b")
    (htree:add-child url3 history "b")
    (htree:add-child url4 history "b")
    (htree:add-child url5 history "b")
    (htree:backward history "b")
    (htree:reset-owner history "b")

    (assert-equal (list url4)
                  (mapcar #'htree:data (htree:all-contiguous-owned-nodes history "b")))
    (assert-false (htree:parent (htree:owner-node history "b")))))

(define-test go-to-owned-child ()
  (let ((history (make))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org"))
    (htree:add-child url1 history *owner*)
    (htree:add-child url2 history *owner*)
    (htree:backward history *owner*)
    (htree:add-owner history "a" :creator-id *owner*)
    (htree:add-child url3 history "a")
    (htree:backward history "a")

    (assert-string= url1
                    (htree:data (htree:owner-node history *owner*)))
    (htree:go-to-owned-child url3 history *owner*)
    (assert-string= url1
                    (htree:data (htree:owner-node history *owner*)))
    (htree:go-to-owned-child url2 history *owner*)
    (assert-string= url2
                    (htree:data (htree:owner-node history *owner*)))

    (assert-string= url1
                    (htree:data (htree:owner-node history "a")))
    (htree:go-to-owned-child url2 history "a")
    (assert-string= url1
                    (htree:data (htree:owner-node history "a")))
    (htree:go-to-owned-child url3 history "a")
    (assert-string= url3
                    (htree:data (htree:owner-node history "a")))))

(define-test add-children ()
  (let ((history (make))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org"))
    (htree:add-children (list url1 url2 url3) history *owner*)
    (assert-string= url3
                    (htree:data (htree:owner-node history *owner*)))
    (assert-false (htree::map-data (htree:children (htree:owner-node history *owner*))))
    (htree:backward history *owner*)
    (assert-string= url1
                    (htree:data (htree:owner-node history *owner*)))

    (assert-eq 2
               (length (htree:children (htree:owner-node history *owner*))))))

(define-test find-owner-node ()
  (let ((history (make))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org")
        (url4 "http://en.wikipedia.org/wiki/web")
        (url5 "http://other.example.org"))
    (htree:add-child url1 history *owner*)
    (htree:add-child url2 history *owner*)
    (htree:add-child url3 history *owner*)
    (htree:backward history *owner*)
    (htree:backward history *owner*)
    (htree:add-child url4 history *owner*)
    (htree:backward history *owner*)

    (let ((owner (htree:owner history *owner*)))
      (assert-true (htree::find-owned-child url4 owner))
      (assert-true (htree::find-owned-child url2 owner))

      (assert-false (htree::find-owned-child url1 owner))
      (assert-false (htree::find-owned-child url3 owner))
      (let ((owner2-spec "other-owner"))
        (htree:add-owner history owner2-spec :creator-id *owner*)
        (htree:add-child url5 history owner2-spec)
        (htree:backward history owner2-spec)
        (let ((owner (htree:owner history *owner*)))
          (assert-false (htree::find-owned-child url5 owner))
          (assert-true (htree::find-child url5 owner))
          (assert-true (htree::find-owned-child url5 (htree:owner history owner2-spec))))))))
