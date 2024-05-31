;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-reduce-tracking-mode ()
  (let ((buffer (make-instance 'network-and-modable-buffer))
        (url-pre (quri:uri "https://example.com/query?foo=bar&twclid=1&redirect=https://example.org/foo&s_cid=123"))
        (url-post (quri:uri "https://example.com/query?foo=bar&redirect=https://example.org/foo")))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/reduce-tracking:reduce-tracking-mode
                                  buffer))
      (assert-equality #'quri:uri=
                       url-post
                       (url (nhooks:run-hook (request-resource-hook buffer)
                                             (make-instance 'request-data
                                                            :url url-pre))))
      (assert-true (disable-modes* 'nyxt/mode/reduce-tracking:reduce-tracking-mode
                                   buffer)))))
