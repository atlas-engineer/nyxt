(defpackage :nyxt.tests
  (:use :common-lisp
        :nyxt
        :prove))

(in-package :nyxt.tests)

(plan nil)

(subtest "Global history"
  (let* ((*browser* (make-instance 'user-browser))
         (buffer (nyxt::make-internal-buffer)))
    ;; We need to make sure nothing will be restored.
    (nyxt:with-current-buffer buffer
      (let ((path (setf (history-path buffer)
                        (make-instance 'history-data-path
                                       :basename "history"
                                       :dirname "/tmp/nyxt-history-test"))))
        (history-add (quri:uri "http://example.org"))
        (is (hash-table-count (nyxt:get-data path))
            1
            "history has 1 entry")
        (let ((entry (first (alexandria:hash-table-alist
                             (nyxt:get-data path)))))
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
        (is (hash-table-count (nyxt:get-data path))
            1
            "history has still 1 entry after adding same URI")
        (let ((entry (first (alexandria:hash-table-alist (nyxt:get-data path)))))
          (is (title (rest entry))
              "foo"
              "value now has title"))
        (history-add (quri:uri "http://example.org/sub"))
        (is (hash-table-count (nyxt:get-data path))
            2
            "history now has 2 entries")
        (uiop:delete-file-if-exists (expand-path (history-path buffer)))))))

(finalize)
