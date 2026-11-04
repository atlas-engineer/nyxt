;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test make-find-run-delete-command ()
  (let ((command (make-command 'probe-command
                               '(lambda ()
                                 "Probe command."
                                 :probed)
                               :global)))
    (unwind-protect
         (let ((*browser* (make-instance 'browser))
               (buffer (make-instance 'buffer)))
           (assert-eq command (nyxt::find-command 'probe-command))
           (with-current-buffer buffer
             (assert-eq :probed (nyxt::run-command command))))
      #+sbcl
      (sb-ext:without-package-locks
        (nyxt::delete-command 'probe-command))
      #-sbcl
      (nyxt::delete-command 'probe-command)
      (assert-false (nyxt::find-command 'probe-command))
      (assert-false (fboundp 'probe-command)))))

(define-test lambda-command-closes-over-environment ()
  (let ((n 0))
    (unwind-protect
         (let ((command (lambda-command probe-lambda (x)
                          (+ x n))))
           (assert-false (nyxt::find-command 'probe-lambda))
           (assert-eq 1 (funcall command 1))
           (incf n)
           (assert-eq 3 (funcall command 2)))
      #+sbcl
      (sb-ext:without-package-locks
        (nyxt::delete-command 'probe-lambda))
      #-sbcl
      (nyxt::delete-command 'probe-lambda))))

(defun prompt-source (sources)
  (etypecase sources
    (symbol (make-instance sources))
    (prompter:source sources)))

(defun source-suggestions (source)
  (bordeaux-threads:with-timeout (5)
    (bt:with-lock-held ((slot-value source 'prompter::initial-suggestions-lock))
      (prompter:suggestions source))))

(defun suggestion-text (suggestion)
  (second (first (prompter:attributes suggestion))))

(defun accept-suggestion (source predicate)
  (let ((suggestion (find-if predicate (source-suggestions source))))
    (assert-true suggestion)
    (when suggestion
      (let ((action (first (slot-value source 'prompter::actions-on-return))))
        (first (funcall action (list (prompter:value suggestion))))))))

(defun answer-hook-handler-prompt (hook handler prompt sources)
  (let ((source (prompt-source sources)))
    (cond
      ((string= prompt "Hook")
       (accept-suggestion
        source
        (lambda (suggestion)
          (and (eq hook (nyxt::value (prompter:value suggestion)))
               (string= "before-exit-hook" (suggestion-text suggestion))))))
      ((string= prompt "Handler")
       (accept-suggestion
        source
        (lambda (suggestion)
          (and (eq handler (prompter:value suggestion))
               (string= (string-downcase handler)
                        (suggestion-text suggestion))))))
      (t (error "Unexpected prompt ~s." prompt)))))

(define-test disable-and-enable-hook-handler ()
  (unless lparallel:*kernel*
    (setf lparallel:*kernel*
          (lparallel:make-kernel (or (serapeum:count-cpus) 1))))
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'buffer))
         (hook (before-exit-hook *browser*))
         (handler (gensym "PROBE-HOOK-HANDLER"))
         (other (gensym "OTHER-HOOK-HANDLER"))
         (runs 0)
         (other-runs 0)
         (original-prompt1 #'prompt1))
    (setf (fdefinition handler) (lambda () (incf runs)))
    (setf (fdefinition other) (lambda () (incf other-runs)))
    (hooks:add-hook hook handler)
    (hooks:add-hook hook other)
    (unwind-protect
         (progn
           (flet ((install-prompt ()
                    (setf (fdefinition 'prompt1)
                          (lambda (&key prompt sources &allow-other-keys)
                            (answer-hook-handler-prompt hook handler prompt sources)))))
             #+sbcl
             (sb-ext:without-package-locks
               (install-prompt))
             #-sbcl
             (install-prompt))
           (with-current-buffer buffer
             (disable-hook-handler)
             (assert-false (find handler (hooks:handlers hook)))
             (assert-eq handler (find handler (hooks:disabled-handlers hook)))
             (assert-eq other (find other (hooks:handlers hook)))
             (hooks:run-hook hook)
             (assert-eq 0 runs)
             (assert-eq 1 other-runs)
             (enable-hook-handler)
             (assert-eq handler (find handler (hooks:handlers hook)))
             (assert-false (find handler (hooks:disabled-handlers hook)))
             (hooks:run-hook hook)
             (assert-eq 1 runs)
             (assert-eq 2 other-runs)))
      (hooks:remove-hook hook handler)
      (hooks:remove-hook hook other)
      (when (fboundp handler)
        (fmakunbound handler))
      (when (fboundp other)
        (fmakunbound other))
      #+sbcl
      (sb-ext:without-package-locks
        (setf (fdefinition 'prompt1) original-prompt1))
      #-sbcl
      (setf (fdefinition 'prompt1) original-prompt1))))
