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
      (let ((missing-packages (remove-if  #'find-package (uiop:ensure-list package))))
        (when missing-packages
          (logger "Undefined test packages: ~s" missing-packages)))
      (let ((*debugger-hook* (if (env-true-p "NYXT_TESTS_ERROR_ON_FAIL")
                                 nil    ; We are non-interactive.
                                 *debugger-hook*)))
        (let ((test-results
                (uiop:symbol-call :lisp-unit2 :run-tests
                                  :package package
                                  :tags tags
                                  :exclude-tags exclude-tags
                                  :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))
          (when (and
                 (or
                  (uiop:symbol-call :lisp-unit2 :failed test-results)
                  (uiop:symbol-call :lisp-unit2 :errors test-results))
                 (getenv "NYXT_TESTS_ERROR_ON_FAIL"))
            ;; Arbitrary but hopefully recognizable exit code.
            (quit 18)))))))

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

(defun redefinition-p (condition)       ; From Slynk.
  (and (typep condition 'style-warning)
       (every #'char-equal "redefin" (princ-to-string condition))))

#+ccl
(defun osicat-warning-p (condition)
  ;; Osicat triggers a warning on CCL because of some unimplemented chunk.
  ;; See https://github.com/osicat/osicat/issues/37.
  (and (typep condition 'style-warning)
       (search "Undefined function OSICAT::MAKE-FD-STREAM" (princ-to-string condition))))

(export-always 'fail-on-warnings)
(defun fail-on-warnings (thunk) ; TODO: Is it possible to report the offending component?
  (handler-bind ((warning (lambda (c)
                            (unless (or (redefinition-p c)
                                        #+ccl
                                        (osicat-warning-p c))
                              (error c) ))))
    (funcall thunk)))
