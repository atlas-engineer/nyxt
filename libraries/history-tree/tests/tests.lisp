;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)
(uiop:define-package history-tree-tests
  (:use #:common-lisp)
  (:import-from #:class-star #:define-class))
(in-package :history-tree-tests)

(prove:plan nil)

(prove:subtest "Single entry"
  (let ((history (htree:make))
        (url "http://example.org" ))
    (htree:set-current-owner history "a")
    (htree:add-child url history)
    (prove:is (htree:value (htree:entry (htree:current-owner-node history)))
              url)))

(prove:subtest "Multiple entry"
  (let ((history (htree:make))
        (url1 "http://example.org")
        (url2 "https://nyxt.atlas.engineer")
        (url3 "http://en.wikipedia.org"))
    (htree:set-current-owner history "a")
    (htree:add-child url1 history)
    (htree:add-child url2 history)
    (htree:back history)
    (htree:add-child url3 history)
    (prove:is (htree:value (htree:entry (htree:current-owner-node history)))
              url3)
    (prove:is (htree:value (htree:entry (htree:parent (htree:current-owner-node history))))
              url1)
    ;; TODO: Go to other child.
    ))

;; (defun make-tree1 ()
;;   (let ((tree (htree:make)))
;;     (dolist (url '(
;;                    "http://example.root"
;;                    "http://example.root/A"
;;                    "http://example.root/A1"))
;;       (htree:add-child url tree))
;;     (htree:back tree)
;;     (htree:add-child "http://example.root/A2" tree)
;;     (htree:back tree 2)
;;     (htree:add-child "http://example.root/B" tree)
;;     (htree:add-child "http://example.root/B1" tree)
;;     (htree:back tree)
;;     (htree:add-child "http://example.root/B2" tree)
;;     tree))

;; (defun make-tree2 ()
;;   (let ((tree (htree:make)))
;;     (htree:add-child "http://example.root" tree)
;;     (htree:add-child "http://example.root/A" tree)
;;     (htree:back tree)
;;     (htree:add-child "http://example.root/B" tree)
;;     tree))

;; (prove:subtest
;;     "Simple tree tests."
;;   (prove:is (htree:data (htree:current (make-tree1)))
;;             "http://example.root/B2"))

;; (prove:subtest
;;     "History depth."
;;   (prove:is (htree:depth (make-tree1))
;;             2))

;; (prove:subtest
;;     "History size."
;;   (prove:is (htree:size (make-tree1))
;;             7))

;; (prove:subtest
;;     "All history nodes."
;;   (prove:is (htree:all-nodes-data (make-tree1))
;;             '("http://example.root"
;;               "http://example.root/B"
;;               "http://example.root/B2" "http://example.root/B1"
;;               "http://example.root/A"
;;               "http://example.root/A2" "http://example.root/A1")))

;; (prove:subtest
;;     "Reorder the nodes."
;;   (prove:is (htree:all-nodes-data
;;              (htree:go-to-child
;;                "http://example.root/A2"
;;               (htree:go-to-child
;;                "http://example.root/A"
;;                (htree:back (make-tree1) 2))))
;;             '("http://example.root"
;;               "http://example.root/A"
;;               "http://example.root/A2" "http://example.root/A1"
;;               "http://example.root/B"
;;               "http://example.root/B2" "http://example.root/B1")))

;; (prove:subtest
;;     "Reorder manually set current node."
;;   (prove:is (htree:all-nodes-data
;;              (let* ((history (make-tree2))
;;                     (second-child (second (htree:children
;;                                            (htree:current (htree:back history))))))
;;                (setf (htree:current history) second-child)
;;                (htree:back history)
;;                history))
;;             '("http://example.root"
;;               "http://example.root/A"
;;               "http://example.root/B")))

;; (prove:subtest
;;     "Traverse all history."
;;   (prove:is (htree:all-nodes-data
;;              (htree:back (make-tree1)))
;;             '("http://example.root"
;;               "http://example.root/B"
;;               "http://example.root/B2" "http://example.root/B1"
;;               "http://example.root/A"
;;               "http://example.root/A2" "http://example.root/A1")))

;; (prove:subtest
;;     "Traverse parents."
;;   (prove:is (htree:parent-nodes-data
;;              (htree:back (make-tree1)))
;;             '("http://example.root")))

;; (prove:subtest
;;     "Traverse forward children."
;;   (prove:is (htree:forward-children-nodes-data
;;              (htree:back (make-tree1)))
;;             '("http://example.root/B2")))

;; (prove:subtest
;;     "Traverse all children."
;;   (prove:is (htree:children-nodes-data
;;              (htree:back (make-tree1)))
;;             '("http://example.root/B2" "http://example.root/B1")))

;; (prove:subtest
;;     "Move existing child to first position on add."
;;   (let ((tree (make-tree2)))
;;     (prove:is (htree:data (htree:current tree))
;;               "http://example.root/B")
;;     (htree:back tree)
;;     (prove:is (htree:data (htree:current tree))
;;               "http://example.root")
;;     (htree:add-child "http://example.root/A" tree)
;;     (prove:is (htree:data (htree:current tree))
;;               "http://example.root/A")))

;; (defun trim-scheme (url)
;;   (let ((delimiter "://"))
;;     (subseq url (+ (length delimiter) (search delimiter url)))))

;; (defun scheme-less-url= (url1 url2)
;;   (string= (trim-scheme url1)
;;            (trim-scheme url2)))

;; (prove:subtest
;;     "Update current node data."
;;   (let ((tree (make-tree2)))
;;     (htree:add-child "https://example.root/B" tree :test #'scheme-less-url=)
;;     (prove:is (htree:data (htree:current tree))
;;               "https://example.root/B")
;;     (htree:back tree)
;;     (prove:is (htree:data (htree:current tree))
;;               "http://example.root")))

(define-class web-page ()
  ((url "")
   (title ""))
  (:accessor-name-transformer #'class*:name-identity))

(defun first-hash-table-key (hash-table)
  (with-hash-table-iterator (next-entry hash-table)
    (nth-value 1 (next-entry))))

(prove:subtest "Compound entry uniqueness"
  (let ((web-page1 (make-instance 'web-page :url "http://example.org"
                                            :title "Example page") )
        (web-page2 (make-instance 'web-page :url "http://example.org"
                                            :title "Same page, another title")))
    (let ((history (htree:make :entry-key #'url)))
      (htree:set-current-owner history "a")
      (htree:add-child web-page1 history)
      (htree:add-child web-page2 history)
      (prove:is (hash-table-count (htree:entries history))
                1)
      (prove:is (title (htree:value (first-hash-table-key (htree:entries history))))
                "Same page, another title"))
    (let ((history (htree:make)))
      (htree:set-current-owner history "a")
      (htree:add-child web-page1 history)
      (htree:add-child web-page2 history)
      (prove:is (hash-table-count (htree:entries history))
                2)
      (prove:is (sort (loop for key being the hash-keys in (htree:entries history)
                                      collect (title (htree:value key)))
                      #'string<)
                (sort (mapcar #'title (list web-page1 web-page2)) #'string<)))))

(prove:finalize)
