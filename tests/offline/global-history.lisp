;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test global-history ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (nyxt::make-buffer)))
    (nyxt:with-current-buffer buffer
      (enable-modes* 'nyxt/mode/history:history-mode buffer)
      (let ((file (history-file buffer))
            (mode (find-submode 'nyxt/mode/history:history-mode buffer)))
        (assert-true mode)
        (nyxt/mode/history::history-add (quri:uri "http://example.org"))
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
        (nyxt/mode/history::history-add (quri:uri "http://example.org") :title "foo")
        ;; "history has still 1 entry after adding same URL"
        (assert-eq 1
                   (length (htree:all-data (files:content file))))
        (assert-eq 2
                   (nyxt::implicit-visits (first (htree:all-data (files:content file)))))
        (let ((entry (first (htree:all-data (files:content file)))))
          ;; "value now has title"
          (assert-string= "foo"
                          (title entry)))
        (nyxt/mode/history::history-add (quri:uri "http://example.org/sub"))
        ;; "history now has 2 entries"
        (assert-eq 2
                   (length (htree:all-data (files:content file))))
        (sleep 0.2)
        ;; Enable hub backtracking.
        (setf (nyxt/mode/history:backtrack-to-hubs-p mode) t)
        ;; Add the same URL and test if backtracking worked.
        (nyxt/mode/history::history-add (quri:uri "http://example.org"))
        (assert-eq 2
                   (length (htree:all-data (files:content file))))
        (let* ((current (htree:current (htree:owner (files:content file) (id buffer))))
               (children (htree:children current)))
          (assert-eq 1
                     (length children))
          (assert-false (htree:parent current))
          (assert-equality #'quri:uri=
                           (quri:uri "http://example.org/sub")
                           (url (htree:data (first children)))))
        (uiop:delete-file-if-exists (files:expand (history-file buffer)))))))

(define-test history-restoration ()
  (let* ((path (asdf:system-relative-pathname :nyxt
                                              "tests/test-data/broken-history.lisp"))
         (history (files:read-file nyxt::*nyxt-profile*
                                   (make-instance 'history-file :base-path path))))
    (assert-eq 2
               (hash-table-count (htree:owners history)))
    (assert-eq 3
               (hash-table-count (htree:entries history)))))
