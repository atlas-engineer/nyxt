;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-force-https-mode ()
  (let ((buffer (make-instance 'network-and-modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/force-https:force-https-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/force-https:force-https-mode buffer)))))

(define-test force-https-rewrites-http-and-stops-loops ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'network-and-modable-buffer)))
    (with-current-buffer buffer
      (enable-modes* 'nyxt/mode/force-https:force-https-mode buffer)
      (let ((mode (find-submode 'nyxt/mode/force-https:force-https-mode buffer))
            (https-request (make-instance 'request-data
                                          :buffer buffer
                                          :url (quri:uri "https://example.com/a")))
            (http-request (make-instance 'request-data
                                         :buffer buffer
                                         :url (quri:uri "http://example.com/a"))))
        (assert-eq https-request
                   (nyxt/mode/force-https::force-https-handler https-request))
        (assert-string= "https" (quri:uri-scheme (url https-request)))
        (assert-eq http-request
                   (nyxt/mode/force-https::force-https-handler http-request))
        (assert-string= "https" (quri:uri-scheme (url http-request)))
        (assert-eql 443 (quri:uri-port (url http-request)))
        (let ((loop-request (make-instance 'request-data
                                           :buffer buffer
                                           :url (quri:copy-uri
                                                 (nyxt/mode/force-https:previous-url mode)))))
          (assert-false (nyxt/mode/force-https::force-https-handler loop-request)))))))
