;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test simple-configuration ()
  (let ((test-url (quri:uri "about:blank")))
    (nyxt:define-configuration nyxt:browser
      ((nyxt:default-new-buffer-url test-url)))
    (let ((browser (make-instance 'browser)))
      (assert-equality #'quri:uri=
                       test-url
                       (nyxt:default-new-buffer-url browser)))
    (nyxt:clean-configuration)
    (let ((browser (make-instance 'browser)))
      (assert-equality #'quri:uri=
                       (quri:uri (nyxt-url 'new))
                       (nyxt:default-new-buffer-url browser)))
    (nyxt:clean-configuration)))

(define-test slot-default ()
  (let ((test-url (quri:uri "about:blank")))
    (nyxt:define-configuration nyxt:browser
      ((nyxt:default-new-buffer-url test-url)))
    (let ((browser (make-instance 'browser)))
      (assert-equality #'quri:uri=
                       test-url
                       (nyxt:default-new-buffer-url browser)))
    (nyxt:clean-configuration)
    (nyxt:define-configuration nyxt:browser
      ((nyxt:default-new-buffer-url nyxt:%slot-default%)))
    (let ((browser (make-instance 'browser)))
      (assert-equality #'quri:uri=
                       (quri:uri (nyxt-url 'new))
                       (nyxt:default-new-buffer-url browser)))
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
  (let ((browser (make-instance 'browser)))
    (assert-eql 2
                (length (hooks:handlers (nyxt:before-exit-hook browser))))))
