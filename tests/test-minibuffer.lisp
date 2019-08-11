(defpackage :next.tests
  (:use :common-lisp
        :next
        :prove))

(in-package :next.tests)

(plan nil)

(subtest "Backwards word"
  (is 0
      (next::backwards-word-position "hello" 4))
  (is 0
      (next::backwards-word-position "a b/c d:hello" 1))
  (is 1
      (next::backwards-word-position "a/hello" 5))
  (is 3
      (next::backwards-word-position "a/b/hello" 7))
  (is 3
      (next::backwards-word-position "a:b hello" 7)))

(finalize)
