;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test simple-configuration ()
  (let ((test-url (quri:uri "about:blank")))
    (nyxt:define-configuration nyxt:browser
      ((nyxt:default-new-buffer-url test-url)))
    (assert-equality #'quri:uri=
                     test-url
                     (nyxt:default-new-buffer-url (make-instance 'browser)))
    (nyxt:clean-configuration)
    (assert-equality #'quri:uri=
                     (quri:uri (nyxt-url 'new))
                     (nyxt:default-new-buffer-url (make-instance 'browser)))
    (nyxt:clean-configuration)))

(define-test overwritten-configuration ()
  (let ((test-first-url (quri:uri "https://example.com/first"))
        (test-second-url (quri:uri "https://example.com/second")))
    (nyxt:define-configuration nyxt:browser
      ((nyxt:default-new-buffer-url test-first-url)))
    (nyxt:define-configuration nyxt:browser
      ((nyxt:default-new-buffer-url test-second-url)))
    (assert-equality #'quri:uri=
                     test-second-url
                     (nyxt:default-new-buffer-url (make-instance 'browser)))
    (nyxt:clean-configuration)))

(define-test slot-default ()
  (let ((test-url (quri:uri "about:blank")))
    (nyxt:define-configuration nyxt:browser
      ((nyxt:default-new-buffer-url test-url)))
    (assert-equality #'quri:uri=
                     test-url
                     (nyxt:default-new-buffer-url (make-instance 'browser)))
    (nyxt:clean-configuration)
    (nyxt:define-configuration nyxt:browser
      ((nyxt:default-new-buffer-url nyxt:%slot-default%)))
    (assert-equality #'quri:uri=
                     (quri:uri (nyxt-url 'new))
                     (nyxt:default-new-buffer-url (make-instance 'browser)))
    (nyxt:clean-configuration)))

(define-test test-slot-value ()
  (nyxt:define-configuration nyxt:browser
    ((nyxt:before-exit-hook (hooks:add-hook nyxt:%slot-value%
                                            (make-instance 'hooks:handler
                                                           :fn (lambda () (print 'dummy1))
                                                           :name 'dummy1)))))
  (nyxt:define-configuration nyxt:browser
    ((nyxt:before-exit-hook (hooks:add-hook nyxt:%slot-value%
                                            (make-instance 'hooks:handler
                                                           :fn (lambda () (print 'dummy2))
                                                           :name 'dummy2)))))
  (assert-eql 2
              (length (hooks:handlers (nyxt:before-exit-hook (make-instance 'browser))))))
