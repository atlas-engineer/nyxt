;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test release-version-accepts-a-triple ()
  (assert-string= "3.12.0" (nyxt::release-version "3.12.0"))
  (assert-string= "3.12.0" (nyxt::release-version (format nil "3.12.0~%"))))

(define-test release-version-rejects-other-text ()
  (assert-false (nyxt::release-version "4"))
  (assert-false (nyxt::release-version "3.12"))
  (assert-false (nyxt::release-version "4.0.0-pre-release-1"))
  (assert-false (nyxt::release-version "03.12.0"))
  (assert-false (nyxt::release-version "nope"))
  (assert-false (nyxt::release-version nil)))

(define-test version-newer-p-compares-numeric-versions ()
  (assert-true (nyxt::version-newer-p "3.12.0" "3.11.8"))
  (assert-false (nyxt::version-newer-p "3.12.0" "3.12.0"))
  (assert-false (nyxt::version-newer-p "3.12.0" "4"))
  (assert-true (nyxt::version-newer-p "4.0.0" "4"))
  (assert-false (nyxt::version-newer-p "4.0.0" "4.0.0-pre-release-3")))
