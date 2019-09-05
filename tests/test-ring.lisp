(defpackage :next.tests
  (:use :common-lisp
   :next
        :prove))

(in-package :next.tests)

(plan nil)

(defvar test-ring1 (make-instance 'next::ring))

(loop for i from 1 to 1000
      do (next::ring-insert test-ring1 (make-string i :initial-element #/a)))

(subtest "Add 1000 items, test head-index, item-count, car of recent-list length."
  (is 0
      (next::head-index test-ring1))
  (is 1000
      (next::item-count test-ring1))
  (is 1000
      (length (car (next::ring-recent-list next.tests::test-ring1)))))


(loop for i from 1 to 5
      do (next::ring-insert test-ring1 (make-string i :initial-element #/b)))

(subtest "Add 5 more items, test head-index again, and item-count again, then car recent-list."
  (is 5
      (next::head-index test-ring1))
  (is 1000
      (next::item-count test-ring1))
  (is "bbbbb"
      (car (next::ring-recent-list test-ring1))))



(finalize)
