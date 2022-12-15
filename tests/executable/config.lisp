;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/tests/executable
  (:use :lisp-unit2))
(in-package :nyxt/tests/executable)

(defvar *executable* (asdf:system-relative-pathname :nyxt "nyxt"))
(defvar *timeout* 10
  "Timeout in seconds.  See `exec-with-timeout'.")

(defun exec-with-timeout (args)
  "Execute Nyxt and kill after `*timeout*'."
  (let* ((command-line (append (list *executable*) args) )
         (result-channel (nyxt::make-channel))
         (thread (bt:make-thread
                  (lambda ()
                    (format t "~&; Running:~%~s~&" command-line)
                    (finish-output)
                    (calispel:! result-channel
                                (nth-value 2
                                           (uiop:run-program command-line
                                                             :error-output t
                                                             :output t
                                                             :ignore-error-status t)))))))
    (multiple-value-bind (results not-timed-out?)
        (calispel:? result-channel *timeout*)
      (if not-timed-out?
          results
          (progn
            (ignore-errors (bt:destroy-thread thread))
            (error "Timed out after ~as when executing~%~s"
                   *timeout*
                   command-line))))))

(defun exec-with-config (config-s-exp args)
  (uiop:with-temporary-file (:pathname config)
    (alex:with-output-to-file (f config  :if-exists :supersede)
      (let ((*package* #.*package*))
        (write config-s-exp :stream f)))
    (exec-with-timeout
     (append
      (list "--no-auto-config" "--config" (uiop:native-namestring config))
      args))))

(defun eval-on-startup (&rest args)
  (list
   "--headless"
   "--eval"
   (let ((*package* #.*package*))
     (write-to-string
      `(hooks:once-on (nyxt:after-startup-hook nyxt:*browser*) (browser)
         (handler-case (progn ,@args)
           (condition (c)
             (log:error "~a" c)
             (nyxt:quit 17))))))))

(define-test eval-works ()
  (assert-eql 0
              (exec-with-timeout
               `("--no-config"
                 "--eval" ,(write-to-string `(or
                                              (eq *package* (find-package :nyxt-user))
                                              (nyxt:quit 17)))
                 "--quit"))))

(define-test config-loads-and-browser-starts ()
  (assert-eql 0
              (exec-with-config
               `(defvar foo "foo variable")
               (eval-on-startup
                `(assert (string= foo "foo variable"))
                `(nyxt:quit)))))

(define-test config-fails-and-browser-restarts ()
  (assert-eql
   1
   (exec-with-config
    `(defmethod customize-instance ((buffer buffer) &key)
       ;; `buffer' does not have an `auto-rules-file' slot.
       (setf (auto-rules-file buffer) (make-instance 'auto-rules-file
                                                     :base-path "/path/to/auto/rules")))
    (eval-on-startup
     `(nyxt:quit)))))

(define-test composable-default-modes ()
  (assert-eql
   0
   (exec-with-config
    `(progn
       (nyxt:define-configuration nyxt:web-buffer
         ((nyxt:default-modes (append '(nyxt/reading-line-mode:reading-line-mode) nyxt:%slot-value%))))
       (nyxt:define-configuration nyxt:web-buffer
         ((nyxt:default-modes (append '(nyxt/style-mode:dark-mode) nyxt:%slot-value%)))))
    (eval-on-startup
     `(assert (member 'nyxt/reading-line-mode:reading-line-mode (nyxt:default-modes (nyxt:current-buffer))))
     `(assert (member 'nyxt/style-mode:dark-mode (nyxt:default-modes (nyxt:current-buffer))))
     `(nyxt:quit)))))

(define-test manual-examples ()
  (let* ((*package* (find-package :nyxt))
         (manual-contents (read-from-string (nyxt:function-lambda-string #'nyxt::manual-sections)))
         (code-examples nil))
    (labels ((collect-code-examples (form)
               (cond
                 ((and (listp form)
                       (eq :ncode (first form)))
                  (push (rest form) code-examples))
                 ((listp form)
                  (mapcar #'collect-code-examples
                          (rest form)))
                 (t nil))))
      (collect-code-examples manual-contents))
    (dolist (example code-examples)
      ;; Clean up the quoted forms (Spinneret hack, see `:ncode' docstring).
      (let* ((unquoted-example (mapcar (lambda (form) (if (eq 'quote (first form))
                                                          (rest form)
                                                          form))
                                       example))
             (progn-example (if (> (length unquoted-example) 1)
                                `(progn ,@unquoted-example)
                                unquoted-example)))
        (assert-eql
         0
         (exec-with-config
          progn-example
          (eval-on-startup
           `(nyxt:quit))))))))
