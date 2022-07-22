;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt-asdf)

(export-always 'nyxt-test-system)
(defclass nyxt-test-system (asdf:system)
  ((targets
    :initform '()  ;; (error "Targets required")
    :initarg :targets
    :reader targets
    :documentation "Arguments passed to `lisp-unit2:run-tests'.
Example:

  :targets '(:package my-app/tests :exclude-tags (:foo my-app/tests::bar))"))
  (:documentation "Specialized systems for Nyxt tests.
It automatically depends on Lisp-Unit2 and calls the appropriate invocation for tests.
You must list what to test, see the `targets' slot.

If the NYXT_TESTS_ERROR_ON_FAIL environment variable is set, quit Lisp on failure.
This is useful for some continuous integration systems.

If the NYXT_TESTS_NO_NETWORK environment variable is set, tests with the `:online' tags are excluded."))
(import 'nyxt-test-system  :asdf-user)

(defmethod asdf:component-depends-on ((op asdf:prepare-op) (c nyxt-test-system))
  `((asdf:load-op "lisp-unit2")
    ,@(call-next-method)))

(defmethod asdf:perform ((op asdf:test-op) (c nyxt-test-system))
  (destructuring-bind (&key package tags exclude-tags &allow-other-keys)
      (targets c)
    (let ((exclude-tags (append (when (getenv "NYXT_TESTS_NO_NETWORK")
                                  '(:online))
                                exclude-tags)))
      (when (and
             (lisp-unit2:failed (lisp-unit2:run-tests
                                 :package package
                                 :tags tags
                                 :exclude-tags exclude-tags
                                 :run-contexts 'lisp-unit2:with-summary-context))
             (getenv "NYXT_TESTS_ERROR_ON_FAIL"))
        ;; Arbitrary but hopefully recognizable exit code.
        (quit 18)))))



;; TODO: Remove the following when done with Prove.

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
