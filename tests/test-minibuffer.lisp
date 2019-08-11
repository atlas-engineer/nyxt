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
  (is 0
      (next::backwards-word-position "a////" 4)
      "move past many word delimiters when we are on it.")
  (is 0
      (next::backwards-word-position "a////" 5)
      "move past many word delimiters when we are ahead of it (real world).")
  (is 4
      (next::backwards-word-position "a///b:: " 8)
      "watch out single letters between delimiters.")
  (is 0
      (next::backwards-word-position "a///b:: " 4)
      "watch out single letters between delimiters.")
  (is 2
      (next::backwards-word-position "a/hello" 5))
  (is 2
      (next::backwards-word-position "a/hello" 100))
  (is 4
      (next::backwards-word-position "a/b/hello" 7))
  (is 4
      (next::backwards-word-position "a:b hello" 7)))

(finalize)
