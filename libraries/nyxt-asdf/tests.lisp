;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt-asdf)

;; TODO: Switch to a better test suite (e.g. Lisp-Unit2) and make this more generic.

(export-always 'nyxt-test)
(defclass nyxt-test (asdf:cl-source-file) ())
(import 'nyxt-test :asdf-user)

(export-always 'nyxt-online-test)
(defclass nyxt-online-test (nyxt-test) ())
(import 'nyxt-online-test :asdf-user)

(defun run-test (path &key network-needed-p)
  (and (or (not network-needed-p)
           (not (getenv "NYXT_TESTS_NO_NETWORK")))
       (not (symbol-call :prove :run path))
       (getenv "NYXT_TESTS_ERROR_ON_FAIL")
       ;; Arbitrary exit code.
       (quit 18)))

(defmethod asdf:perform ((op asdf:test-op) (c nyxt-test))
  (run-test c))

(defmethod asdf:perform ((op asdf:test-op) (c nyxt-online-test))
  (run-test c :network-needed-p t))

(export-always 'print-benchmark)
(defun print-benchmark (benchmark-results)
  (labels ((rat->float (num)
             (if (integerp num) num (float num)))
           (print-times (entry)
             (let ((title (first entry))
                   (attr (rest entry)))
               (unless (or (member (symbol-name title) '("RUN-TIME" "SYSTEM-RUN-TIME")) ; Not so interesting.
                           (and (member (symbol-name title) '("PAGE-FAULTS" "EVAL-CALLS")
                                        :test #'string=)
                                (zerop (getf attr :average))))
                 (format t " ~a: ~,9t~a" (string-downcase title) (rat->float (getf attr :average)))
                 (format t "~32,8t[~a, ~a]"
                         (rat->float (getf attr :minimum))
                         (rat->float (getf attr :maximum)))
                 (format t "~56,8t(median ~a, deviation ~a, total ~a)"
                         (rat->float (getf attr :median))
                         (rat->float (getf attr :deviation))
                         (rat->float (getf attr :total)))
                 (format t "~%")))))
    (dolist (mark benchmark-results)
      (format t "~a (~a sample~:p):~%" (first mark)
              (getf (rest (second mark)) :samples))
      (mapc #'print-times (rest mark)))))
