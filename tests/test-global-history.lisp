(defpackage :nyxt.tests
  (:use :common-lisp
        :nyxt
        :prove))

(in-package :nyxt.tests)

(plan nil)

(subtest "Global history"
  (let* ((*browser* (make-instance 'user-browser)))
    ;; Set profile to nosave to inhibit serialization / deserialization.
    ;; TODO: We should still test serialization and deserialization.
    (setf *global-data-profile* (make-instance 'nosave-data-profile))
    (let ((buffer (nyxt::make-internal-buffer)))
      (nyxt:with-current-buffer buffer
        (let ((path (history-path buffer)))
          (nyxt::history-add (quri:uri "http://example.org"))
          (is (length (htree:all-data (nyxt:get-data path)))
              1
              "history has 1 entry")
          (let ((entry (first (htree:all-data (nyxt:get-data path)))))
            (is (url entry)
                (quri:uri "http://example.org")
                :test #'quri:uri=
                "value has quri:uri")
            (is (title entry)
                ""
                "value has no title"))
          (nyxt::history-add (quri:uri "http://example.org") :title "foo")
          (is (length (htree:all-data (nyxt:get-data path)))
              1
              "history has still 1 entry after adding same URI")
          (let ((entry (first (htree:all-data (nyxt:get-data path)))))
            (is (title entry)
                "foo"
                "value now has title"))
          (nyxt::history-add (quri:uri "http://example.org/sub"))
          (is (length (htree:all-data (nyxt:get-data path)))
              2
              "history now has 2 entries")
          (uiop:delete-file-if-exists (expand-path (history-path buffer))))))))

(finalize)
