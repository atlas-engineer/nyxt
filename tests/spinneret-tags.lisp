;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test nbutton-does-not-block-lisp-requests ()
  (flet ((callback-buffer (id)
           ;; Only callback registration is needed, not a renderer view.
           (let ((buffer (allocate-instance (find-class 'input-buffer))))
             (setf (slot-value buffer 'nyxt::id) id
                   (slot-value buffer 'nyxt::lisp-url-callbacks) (make-hash-table :test #'equal))
             buffer)))
    (let* ((origin (callback-buffer 1))
           (other (callback-buffer 2))
           (callbacks (slot-value origin 'nyxt::lisp-url-callbacks))
           (entered (bt:make-semaphore))
           (release (bt:make-semaphore))
           (finished (bt:make-semaphore))
           (returned (bt:make-semaphore))
           (before nil)
           (after nil)
           (action-thread nil)
           (caller nil)
           (button-result :not-returned)
           (value-result :not-returned)
           (failure nil))
      (spinneret::%nbutton-onclick
       "blocking action" origin
       `((funcall ,(lambda ()
                     (setf action-thread (bt:current-thread)
                           before (current-buffer))
                     (bt:signal-semaphore entered)
                     (bt:wait-on-semaphore release)
                     (setf after (current-buffer))
                     (bt:signal-semaphore finished)))))
      (let ((button (first (alex:hash-table-values callbacks))))
        (clrhash callbacks)
        (ps:ps* `(nyxt/ps:lisp-eval (:buffer ,origin) (identity 42)))
        (let ((value-callback (first (alex:hash-table-values callbacks))))
          (unwind-protect
               (progn
                 ;; Model a single scheme worker: a waiting button must leave
                 ;; it available for the next result-returning Lisp request.
                 (setf caller
                       (bt:make-thread
                        (lambda ()
                          (handler-case
                              (with-current-buffer origin
                                (setf button-result (funcall button)
                                      value-result (funcall value-callback)))
                            (error (condition) (setf failure condition)))
                          (bt:signal-semaphore returned))
                        :name "Button callback test scheme worker"))
                 (assert-true (bt:wait-on-semaphore entered :timeout 5))
                 (assert-true (bt:wait-on-semaphore returned :timeout 5))
                 (assert-false failure)
                 (assert-false button-result)
                 (assert-eql 42 value-result)
                 (assert-false after)
                 (assert-eq origin before)
                 (with-current-buffer other
                   (bt:signal-semaphore release)
                   (assert-true (bt:wait-on-semaphore finished :timeout 5)))
                 (assert-eq origin after))
            (bt:signal-semaphore release)
            (when caller (bt:join-thread caller))
            (when (and action-thread (not (eq action-thread caller)))
              (bt:join-thread action-thread))))))))
