;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-class test-profile (nosave-profile)
  ((files:name :initform "test"))
  (:documentation "Test profile that does not read nor write to disk."))

(defmethod files:read-file ((profile test-profile) (file nyxt-file) &key &allow-other-keys)
  "This method guarantees FILE will not be loaded from disk in PROFILE."
  nil)

(define-test global-history ()
  ;; Set profile to nosave to inhibit serialization / deserialization.
  ;; TODO: We should still test serialization and deserialization.
  (let* ((*browser* (make-instance 'browser :profile (make-instance 'test-profile)))
         (buffer (nyxt::make-buffer)))
    (nyxt:with-current-buffer buffer
      (let ((file (history-file buffer)))
        (nyxt::history-add (quri:uri "http://example.org"))
        ;; history has 1 entry
        (assert-eq 1
                   (length (htree:all-data (files:content file))))
        (let ((entry (first (htree:all-data (files:content file)))))
          ;; "value has quri:uri"
          (assert-equality #'quri:uri=
                           (quri:uri "http://example.org")
                           (url entry))
          ;; "value has no title"
          (assert-string= ""
                          (title entry)))
        (nyxt::history-add (quri:uri "http://example.org") :title "foo")
        ;; "history has still 1 entry after adding same URL"
        (assert-eq 1
                   (length (htree:all-data (files:content file))))
        (assert-eq 2
                   (nyxt::implicit-visits (first (htree:all-data (files:content file)))))
        (let ((entry (first (htree:all-data (files:content file)))))
          ;; "value now has title"
          (assert-string= "foo"
                          (title entry)))
        (nyxt::history-add (quri:uri "http://example.org/sub"))
        ;; "history now has 2 entries"
        (assert-eq 2
                   (length (htree:all-data (files:content file))))
        (uiop:delete-file-if-exists (files:expand (history-file buffer)))))))

(define-test history-restoration ()
  (let* ((history-path (make-instance 'history-file
                                      :base-path (asdf:system-relative-pathname
                                                  :nyxt "tests/test-data/broken-history.lisp")))
         (history (files:read-file (global-profile) history-path)))
    (assert-eq 2
               (hash-table-count (htree:owners history)))
    (assert-eq 3
               (hash-table-count (htree:entries history)))))
