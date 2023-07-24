;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nasdf)

(export-always 'nasdf-test-system)
(defclass nasdf-test-system (asdf:system)
  ((targets
    :initform '()  ;; (error "Targets required")
    :initarg :targets
    :reader targets
    :documentation "Arguments passed to `lisp-unit2:run-tests'.
Example:

  :targets '(:package my-app/tests :exclude-tags (:foo my-app/tests::bar))"))
  (:documentation "Specialized systems for enhanced testing.
It automatically depends on Lisp-Unit2 and calls the appropriate invocation for tests.
You must list what to test, see the `targets' slot.

If the NASDF_TESTS_QUIT_ON_FAIL environment variable is set, quit Lisp on failure.
This is useful for some continuous integration systems.

If the NASDF_TESTS_NO_NETWORK environment variable is set, tests with the `:online' tags are excluded."))
(import 'nasdf-test-system  :asdf-user)

(defmethod asdf:component-depends-on ((op asdf:prepare-op) (c nasdf-test-system))
  `((asdf:load-op "lisp-unit2")
    ,@(call-next-method)))

(defmethod asdf:perform :around ((op asdf:test-op) (c nasdf-test-system))
  (let ((*debugger-hook* (if (env-true-p "NASDF_TESTS_QUIT_ON_FAIL")
                             nil        ; We are non-interactive.
                             *debugger-hook*)))
    (handler-bind ((error (lambda (c)
                            (logger "Errors:~&~a" c)
                            (when (env-true-p "NASDF_TESTS_QUIT_ON_FAIL")
                              ;; Arbitrary but hopefully recognizable exit code.
                              (quit 18)))))
      (call-next-method))))

;; TODO: Can we avoid duplicating this `test-op' / `load-op' setup?
(defmethod asdf:perform :around ((op asdf:load-op) (c nasdf-test-system))
  (logger "NASDF_TESTS_QUIT_ON_FAIL=~a~&" (getenv "NASDF_TESTS_QUIT_ON_FAIL"))
  (let ((*debugger-hook* (if (env-true-p "NASDF_TESTS_QUIT_ON_FAIL")
                             nil        ; We are non-interactive.
                             *debugger-hook*)))
    (handler-bind ((error (lambda (c)
                            (logger "Errors:~&~a" c)
                            (when (env-true-p "NASDF_TESTS_QUIT_ON_FAIL")
                              ;; Arbitrary but hopefully recognizable exit code.
                              (quit 18)))))
      (call-next-method))))

(defmethod asdf:perform ((op asdf:test-op) (c nasdf-test-system))
  (destructuring-bind (&key package tags exclude-tags &allow-other-keys)
      (targets c)
    (let ((exclude-tags (append (when (getenv "NASDF_TESTS_NO_NETWORK")
                                  '(:online))
                                exclude-tags)))
      (let ((missing-packages (remove-if #'find-package (uiop:ensure-list package))))
        (when missing-packages
          (logger "Undefined test packages: ~s" missing-packages)))
      ;; Binding `*package*' to test package makes for more reproducible tests.
      (let* ((*package* (find-package package))
             (test-results
               (uiop:symbol-call :lisp-unit2 :run-tests
                                 :package package
                                 :tags tags
                                 :exclude-tags exclude-tags
                                 :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))
        (when (and
               (or
                (uiop:symbol-call :lisp-unit2 :failed test-results)
                (uiop:symbol-call :lisp-unit2 :errors test-results))
               ;; TODO: Always raise error or not?
               (getenv "NASDF_TESTS_QUIT_ON_FAIL"))
          (error "Tests failed."))))))

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
                              (cerror "Continue" "Compilation warning: ~a" c)))))
    (funcall thunk)))
