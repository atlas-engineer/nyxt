;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)
(use-nyxt-package-nicknames)

(define-test run-action-ad-eternum ()
  ;; Runs twice to ensure that mode instances which have not been garbage
  ;; collected still behave as intended.
  (dotimes (_ 2)
    (let* ((action-run-p (lpara:promise))
           (mode (make-instance 'nyxt/process-mode:process-mode
                                :buffer (make-instance 'modable-buffer)
                                :path-url (quri:uri "test")
                                :firing-condition t
                                :action
                                (lambda (path-url mode)
                                  (declare (ignore path-url mode))
                                  (lpara:fulfill action-run-p t)))))
      (enable mode)
      ;; Wait for action-run-p to be fulfilled for a maximum of 2 seconds.
      (loop repeat 20
            until (lpara:fulfilledp action-run-p)
            do (sleep 0.1))
      (disable mode)
      (assert-true (lpara:force action-run-p)))))

(define-test null-action ()
  (let ((mode (make-instance 'nyxt/process-mode:process-mode
                             :buffer (make-instance 'modable-buffer)
                             :path-url (quri:uri "test")
                             :action nil)))
    (enable mode)
    (assert-true (null (nyxt/process-mode::thread mode)))
    (disable mode)))

(define-test null-firing-condition ()
  (let ((mode (make-instance 'nyxt/process-mode:process-mode
                             :buffer (make-instance 'modable-buffer)
                             :path-url (quri:uri "test")
                             :firing-condition nil)))
    (enable mode)
    (assert-true (null (nyxt/process-mode::thread mode)))
    (disable mode)))

(define-test firing-condition ()
  (let* ((run-action-count 0)
         (repeat-action-count (random 10))
         (mode (make-instance 'nyxt/process-mode:process-mode
                              :buffer (make-instance 'modable-buffer)
                              :path-url (quri:uri "test")
                              :action
                              (lambda (path-url mode)
                                (declare (ignore path-url mode))
                                (incf run-action-count))
                              :firing-condition
                              (lambda (path-url mode)
                                (declare (ignore path-url mode))
                                (if (= run-action-count repeat-action-count) :return t)))))
    (enable mode)
    (bt:join-thread (nyxt/process-mode::thread mode))
    (disable mode)
    (assert-eq repeat-action-count
               run-action-count)))

(define-test cleanup ()
  (let* ((clean-p)
         (mode (make-instance 'nyxt/process-mode:process-mode
                              :buffer (make-instance 'modable-buffer)
                              :path-url (quri:uri "test")
                              :cleanup
                              (lambda (path-url mode)
                                (declare (ignore path-url mode))
                                (setf clean-p t)))))
    (nyxt/process-mode::call-cleanup mode)
    (assert-true clean-p)))

(define-test thread-handling ()
  (let ((mode (make-instance 'nyxt/process-mode:process-mode
                             :buffer (make-instance 'modable-buffer)
                             :path-url (quri:uri "test")
                             :action
                             (lambda (path-url mode)
                               (declare (ignore path-url mode))))))
    ;; Ensure that re-enabling the mode doesn't overwrite an alive thread.
    (assert-eq (nyxt/process-mode::thread (enable mode))
               (nyxt/process-mode::thread (enable mode)))
    (disable mode)))
