(defpackage :next.tests
  (:use :common-lisp
        :next
        :prove))

(in-package :next.tests)

(plan nil)

(setf *browser* (make-instance *browser-class*))

(subtest "parse-url"
  (is "https://duckduckgo.com/?q=%2Aspurious%2A" (next::parse-url "*spurious*")
      "ignore wildcards"))

(finalize)
