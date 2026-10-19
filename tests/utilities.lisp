;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test safe-read-rejects-reader-macros ()
  (with-input-from-string (in "(1 2)")
    (assert-equal '(1 2) (safe-read in)))
  (assert-error 'reader-error
                (with-input-from-string (in "#.(+ 1 2)")
                  (safe-read in))))

(define-test smart-case-test-follows-case ()
  (assert-eq #'string-equal (smart-case-test "abc"))
  (assert-eq #'string= (smart-case-test "Abc")))

(define-test last-word-takes-trailing-word ()
  (assert-string= "world" (last-word "hello, world!"))
  (assert-string= "" (last-word "...")))

(define-test sort-by-time-is-newest-first ()
  (let* ((older (time:now))
         (newer (time:timestamp+ older 1 :sec))
         (sorted (nyxt::sort-by-time (list (list older) (list newer))
                                     :key #'first)))
    (assert-equality #'time:timestamp= newer (first (first sorted)))
    (assert-equality #'time:timestamp= older (first (second sorted)))))

(define-test list-of-p-checks-every-element ()
  (assert-true (nyxt/types::list-of-p '(1 2) 'integer))
  (assert-true (nyxt/types::list-of-p '() 'integer))
  (assert-false (nyxt/types::list-of-p '(1 "a") 'integer))
  (assert-false (nyxt/types::list-of-p 1 'integer)))
