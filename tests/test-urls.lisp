(defpackage :nyxt.tests
  (:use :common-lisp
        :nyxt
        :prove))

(in-package :nyxt.tests)

(plan nil)

(setf *browser* (make-instance *browser-class*))

(subtest "parse-url"
  (is "https://duckduckgo.com/?q=%2Aspurious%2A" (nyxt::parse-url "*spurious*")
      "ignore wildcards"))

(finalize)
