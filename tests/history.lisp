;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test history-entry-url-and-equality ()
  (let ((from-string (make-instance 'history-entry :url "https://example.com/a"))
        (from-uri (make-instance 'history-entry :url (quri:uri "https://example.com/a")))
        (other (make-instance 'history-entry :url "https://example.com/b")))
    (assert-true (quri:uri-p (url from-string)))
    (assert-equality #'quri:uri=
                     (quri:uri "https://example.com/a")
                     (url from-string))
    (assert-true (equals from-string from-uri))
    (assert-false (equals from-string other))))

(define-test serialize-history-entry-skips-empty-url ()
  (assert-string= ""
                  (with-output-to-string (out)
                    (nyxt::serialize-history-entry (make-instance 'history-entry) out))))

(define-test history-entry-round-trip ()
  (let* ((entry (make-instance 'history-entry
                               :url "https://example.com/a"
                               :title "Example"))
         (text (with-output-to-string (out)
                 (nyxt::serialize-history-entry entry out))))
    (with-input-from-string (in (format nil "(~a)" text))
      (let ((entries (files:deserialize (make-instance 'nyxt-profile)
                                        (make-instance 'history-file)
                                        in)))
        (assert-eq 1 (length entries))
        (assert-equality #'quri:uri=
                         (quri:uri "https://example.com/a")
                         (url (first entries)))
        (assert-string= "Example" (title (first entries)))))))

(define-test recent-history-entry-order ()
  (let* ((browser (make-instance 'browser))
         (older (make-instance 'history-entry :url "https://a.example" :title "a"))
         (middle (make-instance 'history-entry :url "https://b.example" :title "b"))
         (newer (make-instance 'history-entry :url "https://a.example" :title "a2")))
    (setf (history-vector browser)
          (make-array 3 :fill-pointer t :adjustable t
                        :initial-contents (list older middle newer)))
    (let ((recent (recent-history-entries 2 browser)))
      (assert-eq 2 (length recent))
      (assert-string= "a2" (title (first recent)))
      (assert-string= "b" (title (second recent))))
    (let ((deduped (recent-history-entries 3 browser :deduplicate-p t)))
      ;; `remove-duplicates' on the history vector keeps the later visit.
      ;; The result is then newest-first.
      (assert-eq 2 (length deduped))
      (assert-string= "a2" (title (first deduped)))
      (assert-string= "b" (title (second deduped))))))
