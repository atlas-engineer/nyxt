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
        (nyxt::history-add (quri:uri "http://example.org"))
        (is (htree:size (nyxt:get-data path))
            1
            "history has 1 entry")
        (let ((entry (first (htree:all-nodes-data (nyxt:get-data path)))))
          (is (url entry)
              (quri:uri "http://example.org")
              :test #'quri:uri=
              "value has quri:uri")
          (is (title entry)
              ""
              "value has no title"))
        (nyxt::history-add (quri:uri "http://example.org") :title "foo")
        (is (htree:size (nyxt:get-data path))
            1
            "history has still 1 entry after adding same URI")
        (let ((entry (first (htree:all-nodes-data (nyxt:get-data path)))))
          (is (title entry)
              "foo"
              "value now has title"))
        (nyxt::history-add (quri:uri "http://example.org/sub"))
        (is (htree:size (nyxt:get-data path))
            2
            "history now has 2 entries")
        (uiop:delete-file-if-exists (expand-path (history-path buffer)))))))

(finalize)
