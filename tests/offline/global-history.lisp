;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test global-history ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (nyxt::make-buffer)))
    (enable-modes* 'nyxt/mode/history:history-mode buffer)
    (let ((file (history-file *browser*))
          (mode (find-submode 'nyxt/mode/history:history-mode buffer)))
      (assert-true mode)
      (nyxt/mode/history::add-url-to-history (quri:uri "http://example.org")
                                             buffer mode)
      ;; History has 1 entry.
      (assert-eq 1
                 (length (files:content file)))
      (let ((entry (aref (files:content file) 0)))
        (assert-equality #'quri:uri=
                         (quri:uri "http://example.org")
                         (url entry))
        (assert-string= ""
                        (title entry)))
      (nyxt/mode/history::add-url-to-history (quri:uri "http://example.org")
                                             buffer mode)
      ;; History has 2 entries after adding same URL
      (assert-eq 2
                 (length (files:content file)))
      (nyxt/mode/history::add-url-to-history (quri:uri "http://example.org/sub")
                                             buffer mode)
      ;; History now has 3 entries.
      (assert-eq 3
                 (length (files:content file)))

      ;; Clean-up history test file.
      (uiop:delete-file-if-exists (files:expand (history-file *browser*))))))

(define-test history-restoration ()
  (let* ((path (asdf:system-relative-pathname
                :nyxt
                "tests/test-data/history.lisp"))
         (history (files:read-file nyxt::*nyxt-profile*
                                   (make-instance 'history-file
                                                  :base-path path))))
    (assert-eq 3
               (length history))))
