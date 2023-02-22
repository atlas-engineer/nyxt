;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test initialize-null-actions ()
  (let ((source (make-instance 'prompt-source
                               :name "test"
                               :actions-on-return nil
                               :actions-on-current-suggestion nil
                               :actions-on-marks nil))
        (actions (list #'identity)))
    (assert-equal actions
                  (actions-on-return source))
    (assert-equal actions
                  (actions-on-current-suggestion source))
    (assert-equal actions
                  (actions-on-marks source))))

(define-test set-null-actions ()
  (let ((source (make-instance 'prompt-source
                               :name "test"))
        (actions (list #'identity)))
    (setf (actions-on-return source) nil)
    (assert-equal actions
                  (actions-on-return source))
    (setf (actions-on-current-suggestion source) nil)
    (assert-equal actions
                  (actions-on-current-suggestion source))
    (setf (actions-on-marks source) nil)
    (assert-equal actions
                  (actions-on-marks source))))

;; The tests below are disabled for CCL since the compiler is strict enough to
;; not allow setting slots to types that don't satisfy the type declaration.
#-ccl
(define-test initialize-invalid-actions ()
  (dolist (action '(:actions-on-return
                    :actions-on-current-suggestion
                    :actions-on-marks))
    (assert-error 'invalid-prompt-buffer-actions
                  (make-instance 'prompt-source
                                 :name "test"
                                 action 'unbound-symbol))))

#-ccl
(define-test set-invalid-actions ()
  (let ((source (make-instance 'prompt-source
                               :name "test")))
    (assert-error 'invalid-prompt-buffer-actions
                  (setf (actions-on-return source) 'unbound-symbol))
    (assert-error 'invalid-prompt-buffer-actions
                  (setf (actions-on-current-suggestion source) 'unbound-symbol))
    (assert-error 'invalid-prompt-buffer-actions
                  (setf (actions-on-marks source) 'unbound-symbol))))

#-ccl
(unintern 'unbound-symbol)
