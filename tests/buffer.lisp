;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test buffer-table ()
  (let* ((*browser* (make-instance 'browser))
         (first-buffer (make-instance 'buffer :id 1))
         (second-buffer (make-instance 'buffer :id 2)))
    (nyxt::buffer-set 1 first-buffer)
    (nyxt::buffer-set 2 second-buffer)
    (assert-eq first-buffer (nyxt::buffer-get 1))
    (assert-eq second-buffer (nyxt::buffer-get 2))
    ;; Higher ids come first.
    (assert-equal (list second-buffer first-buffer) (buffer-list))
    (nyxt::buffer-delete 1)
    (assert-false (nyxt::buffer-get 1))
    (assert-equal (list second-buffer) (buffer-list))
    (assert-true (nyxt::dead-buffer-p first-buffer))
    (assert-false (nyxt::dead-buffer-p second-buffer))))
