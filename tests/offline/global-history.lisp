;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(plan nil)

(define-class test-data-profile (nosave-data-profile) ; TODO: Use nfiles profile.
  ((name :initform "test"))
  (:documentation "Test profile that does not read nor write to disk."))

(defmethod nyxt:restore ((profile test-data-profile) (path data-path) &key &allow-other-keys)
  "This method guarantees PATH will not be loaded from disk in TEST-DATA-PROFILE."
  nil)

(subtest "Global history"
  (let* ((*browser* (make-instance 'user-browser)))
    ;; Set profile to nosave to inhibit serialization / deserialization.
    ;; TODO: We should still test serialization and deserialization.
    (setf *global-data-profile* (make-instance 'test-data-profile))
    (let ((buffer (nyxt::make-buffer)))
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
              "history has still 1 entry after adding same URL")
          (is (nyxt::implicit-visits (first (htree:all-data (nyxt:get-data path))))
              2)
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
