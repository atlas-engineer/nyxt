(in-package :cl-user)

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

(prove:subtest
    "Simple tree tests."
  (prove:is (htree:data (htree:current (make-tree1)))
            "http://example.root/B2"))

(prove:subtest
    "History depth."
  (prove:is (htree:depth (make-tree1))
            2))

(prove:subtest
    "History size."
  (prove:is (htree:size (make-tree1))
            7))

(prove:subtest
    "All history nodes."
  (prove:is (htree:all-nodes-data (make-tree1))
            '("http://example.root"
              "http://example.root/B"
              "http://example.root/B2" "http://example.root/B1"
              "http://example.root/A"
              "http://example.root/A2" "http://example.root/A1")))

(prove:subtest
    "Reorder the nodes."
  (prove:is (htree:all-nodes-data
             (htree:find-child
               "http://example.root/A2"
              (htree:find-child
               "http://example.root/A"
               (htree:back (make-tree1) 2))))
            '("http://example.root"
              "http://example.root/A"
              "http://example.root/A2" "http://example.root/A1"
              "http://example.root/B"
              "http://example.root/B2" "http://example.root/B1")))

(prove:subtest
    "Reorder manually set current node."
  (prove:is (htree:all-nodes-data
             (let* ((history (make-tree2))
                    (second-child (second (htree:children
                                           (htree:current (htree:back history))))))
               (setf (htree:current history) second-child)
               (htree:back history)
               history))
            '("http://example.root"
              "http://example.root/A"
              "http://example.root/B")))

(prove:subtest
    "Traverse all history."
  (prove:is (htree:all-nodes-data
             (htree:back (make-tree1)))
            '("http://example.root"
              "http://example.root/B"
              "http://example.root/B2" "http://example.root/B1"
              "http://example.root/A"
              "http://example.root/A2" "http://example.root/A1")))

(prove:subtest
    "Traverse parents."
  (prove:is (htree:parent-nodes-data
             (htree:back (make-tree1)))
            '("http://example.root")))

(prove:subtest
    "Traverse forward children."
  (prove:is (htree:forward-children-nodes-data
             (htree:back (make-tree1)))
            '("http://example.root/B2")))

(prove:subtest
    "Traverse all children."
  (prove:is (htree:children-nodes-data
             (htree:back (make-tree1)))
            '("http://example.root/B2" "http://example.root/B1")))

(prove:finalize)
