;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)
(use-nyxt-package-nicknames)

(plan nil)

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
      (write config-s-exp :stream f))
    (exec-with-timeout
     (append
      (list "--no-auto-config" "--config" (uiop:native-namestring config))
      args))))

(defun eval-on-startup (&rest args)
  (list
   "--headless"
   "--eval"
   (write-to-string
    `(hooks:once-on nyxt:*after-startup-hook* ()
       (handler-case (progn ,@args)
         (condition (c)
           (log:error "~a" c)
           (nyxt:quit 17)))))))

(subtest "Eval works"
  (prove:is
   (exec-with-timeout
    `("--no-config"
      "--eval" ,(write-to-string `(or
                                   (eq *package* (find-package :nyxt-user))
                                   (nyxt:quit 17)))
      "--quit"))
   0))

(subtest "Config loads and browser starts"
  (prove:is
   (exec-with-config
    `(defvar foo "foo variable")
    (eval-on-startup
     `(assert (string= foo "foo variable"))
     `(nyxt:quit)))
   0))

(subtest "Config fails and browser restarts"
  (prove:is
   (exec-with-config
    `(defmethod customize-instance ((buffer buffer) &key)
       (setf (auto-mode-rules-file buffer) (make-instance 'auto-mode-rules-file
                                                          :base-path "/path/to/auto/rules")))
    (eval-on-startup
     `(nyxt:quit)))
   1))

(subtest "Default-modes are composable"
  (prove:is
   (exec-with-config
    `(progn
       (nyxt:define-configuration nyxt:web-buffer
         ((nyxt:default-modes (append '(nyxt/reading-line-mode:reading-line-mode) nyxt:%slot-value%))))
       (nyxt:define-configuration nyxt:web-buffer
         ((nyxt:default-modes (append '(nyxt/style-mode:dark-mode) nyxt:%slot-value%)))))
    (eval-on-startup
     `(assert (member 'nyxt/reading-line-mode:reading-line-mode (nyxt:default-modes (nyxt:current-buffer))))
     `(assert (member 'nyxt/style-mode:dark-mode (nyxt:default-modes (nyxt:current-buffer))))
     `(nyxt:quit)))
   0))

(finalize)
