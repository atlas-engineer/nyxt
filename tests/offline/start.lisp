;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-class dummy-renderer (renderer)
  ((name "Dummy renderer"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod install ((_ dummy-renderer)) t)

(defmethod uninstall ((_ dummy-renderer)) t)

(setf nyxt::*renderer* (make-instance 'dummy-renderer))

(define-test null-quit ()
  (assert-false (nyxt:quit)))

;; See https://github.com/atlas-engineer/nyxt/issues/2754
;; (define-test start-quit-cycle ()
;;   (assert-false (progn (nyxt:start :failsafe t) (nyxt:quit))))

;; Fails.
(define-test stateless-headless-argument ()
  (nyxt:start :headless t :failsafe t)
  (ffi-within-renderer-thread nyxt:*browser*
                              (lambda () (assert-true nyxt::*headless-p*)))
  (nyxt:quit)

  (nyxt:start :failsafe t)
  (ffi-within-renderer-thread nyxt:*browser*
                              (lambda () (assert-false nyxt::*headless-p*)))
  (nyxt:quit))
