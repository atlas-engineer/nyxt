(defpackage :nyxt.tests
  (:use :common-lisp
        :nyxt
        :prove))

(in-package :nyxt.tests)

(plan nil)

(subtest "Global history"
  (progn
    (let ((*browser* (make-instance 'browser)))
      (setf (history-restore-function *browser*) nil)
      (history-add (quri:uri "http://example.org"))
      (is (hash-table-count (nyxt::history-data *browser*))
          1
          "history has 1 entry")
      (let ((entry (first (alexandria:hash-table-alist
                           (nyxt::history-data *browser*)))))
        (is (first entry)
            "http://example.org"
            "key is URL")
        (is (url (rest entry))
            (quri:uri "http://example.org")
            :test #'quri:uri=
            "value has quri:uri")
        (is (title (rest entry))
            nil
            "value has no title"))
      (history-add (quri:uri "http://example.org") :title "foo")
      (is (hash-table-count (nyxt::history-data *browser*))
          1
          "history has still 1 entry after adding same URI")
      (let ((entry (first (alexandria:hash-table-alist
                           (nyxt::history-data *browser*)))))
        (is (title (rest entry))
            "foo"
            "value now has title"))
      (history-add (quri:uri "http://example.org/sub"))
      (is (hash-table-count (nyxt::history-data *browser*))
          2
          "history now has 2 entries"))))

(finalize)
