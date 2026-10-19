;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test make-find-run-delete-command ()
  (let ((command (make-command 'probe-command
                               '(lambda ()
                                 "Probe command."
                                 :probed)
                               :global)))
    (unwind-protect
         (let ((*browser* (make-instance 'browser))
               (buffer (make-instance 'buffer)))
           (assert-eq command (nyxt::find-command 'probe-command))
           (with-current-buffer buffer
             (assert-eq :probed (nyxt::run-command command))))
      #+sbcl
      (sb-ext:without-package-locks
        (nyxt::delete-command 'probe-command))
      #-sbcl
      (nyxt::delete-command 'probe-command)
      (assert-false (nyxt::find-command 'probe-command))
      (assert-false (fboundp 'probe-command)))))

(define-test lambda-command-closes-over-environment ()
  (let ((n 0))
    (unwind-protect
         (let ((command (lambda-command probe-lambda (x)
                          (+ x n))))
           (assert-false (nyxt::find-command 'probe-lambda))
           (assert-eq 1 (funcall command 1))
           (incf n)
           (assert-eq 3 (funcall command 2)))
      #+sbcl
      (sb-ext:without-package-locks
        (nyxt::delete-command 'probe-lambda))
      #-sbcl
      (nyxt::delete-command 'probe-lambda))))
