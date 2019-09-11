(in-package :cl-user)

(prove:plan nil)

(defvar test-ring1 (ring:make-ring))
(setf test-ring1 (ring:make-ring))

(loop for i from 1 to 1000
      do (ring::insert test-ring1 (make-string i :initial-element #/a)))

(prove:subtest "Add 1000 items, test head-index, item-count, first of recent-list length."
  (prove:is 0
      (ring::head-index test-ring1))
  (prove:is 1000
      (ring::item-count test-ring1))
  (prove:is 1000
      (length (first (ring::recent-list test-ring1)))))


(loop for i from 1 to 5
      do (ring::insert test-ring1 (make-string i :initial-element #/b)))

(prove:subtest "Add 5 more items; test head-index, item-count, first recent-list, value at head-index, and (1- head-index)."
  (prove:is 5
      (ring::head-index test-ring1))
  (prove:is 1000
      (ring::item-count test-ring1))
  (prove:is "bbbbb"
      (first (ring::recent-list test-ring1)))
  (prove:is "aaaaaa"
      (aref (ring::items test-ring1) (ring::head-index test-ring1)))
  (prove:is "bbbbb"
            (aref (ring::items test-ring1) (1- (ring::head-index test-ring1)))))

(defvar copy-test nil)
(setf copy-test (ring:copy test-ring1))

(prove:subtest "Test ring copy."
  (prove:is 5
            (ring::head-index test-ring1))
  (prove:is 1000
            (ring::item-count test-ring1))
  (prove:is "bbbbb"
            (first (ring::recent-list test-ring1)))
  (prove:is "aaaaaa"
            (aref (ring::items test-ring1) (ring::head-index test-ring1)))
  (prove:is "bbbbb"
            (aref (ring::items test-ring1) (1- (ring::head-index test-ring1)))))


(prove:finalize)
