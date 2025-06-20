;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun initialize-lparallel-kernel (&key (worker-count (sera:count-cpus)))
  "Initialize the lparallel kernel with WORKER-COUNT, if not supplied set it to
the amount of CPU cores."
  (unless lpara:*kernel*
    (setf lpara:*kernel* (lpara:make-kernel worker-count))))

(defun restart-browser (c)
  "Restart browser reporting condition C."
  (funcall 'restart-with-message        ; Not defined yet.
           :condition c
           :backtrace (with-output-to-string (stream)
                        (uiop:print-backtrace :stream stream :condition c))))

(export-always 'with-protect)
(defmacro with-protect ((format-string &rest args) &body body)
  "Run body with muffled conditions when `*run-from-repl-p*' is nil, run normally otherwise.
When the condition is muffled, a warning is reported to the user as per
FORMAT-STRING and ARGS.
As a special case, the first `:condition' keyword in ARGS is replaced with the
raised condition."
  (alex:with-gensyms (c sub-c)
    `(if (or *run-from-repl-p*)
         (handler-case (progn ,@body)
           (prompt-buffer-canceled ()
             (log:debug "Prompt buffer interrupted")))
         (ignore-errors
          (handler-bind
              ((error
                 (lambda (,c)
                   (declare (ignorable ,c))
                   (if *restart-on-error*
                       (restart-browser ,c)
                       ,(let ((condition-index (position :condition args)))
                          (flet ((new-args (condition condition-index &optional escaped-p)
                                   (if condition-index
                                       (append (subseq args 0 condition-index)
                                               (list (if escaped-p
                                                         `(plump:encode-entities (princ-to-string ,condition))
                                                         `,condition))
                                               (subseq args (1+ condition-index)))
                                       'args)))
                            `(handler-bind ((t (lambda (,sub-c)
                                                 (declare (ignore ,sub-c))
                                                 (log:error ,format-string ,@(new-args c condition-index))
                                                 (invoke-restart 'continue))))
                               (echo-warning ,format-string ,@(new-args c condition-index :escaped-p)))))))))
            ,@body)))))

(defun make-channel (&optional size)
  "Return a channel of capacity SIZE.
If SIZE is NIL, capacity is infinite."
  (declare (ignore size))
  (lparallel.queue:make-queue))

(defun fair-alt-lparallel (queues &key (sleep-duration 0.01))
  "Loop through QUEUES and return the first available value.
Returns (values value queue) when any queue has a value.

Optional keyword argument SLEEP-DURATION controls the polling interval."
  (loop
    ;; Check each queue in order
    for q in queues
    do (multiple-value-bind (value success) (lparallel.queue:try-pop-queue q)
         (when success
           (return (values value q))))
    ;; If no queue had a value, wait a bit before retrying
    do (sleep sleep-duration)))

(defun drain-channel (channel &optional timeout)
  "Listen to CHANNEL until a value is available, then return all CHANNEL values
as a list.
TIMEOUT specifies how long to wait for a value after the first one.
This is a blocking operation."
  (declare (ignore timeout))
  (labels ((fetch ()
             (multiple-value-bind (value received?)
                 (lparallel.queue:pop-queue channel)
               (if received?
                   (cons value (fetch))
                   nil))))
    (cons (lparallel.queue:pop-queue channel)
          (nreverse (fetch)))))

(export-always 'run-thread)
(defmacro run-thread (name &body body)
  "Run body in a new protected thread.
This supersedes `bt:make-thread' in Nyxt.  Don't use the latter unless you know
what you are doing!"
  `(lparallel.thread-util:with-thread (:name ,(str:concat "Nyxt " name)
                                       :bindings (append '((*run-from-repl-p* . *run-from-repl-p*)
                                                           (*headless-p* . *headless-p*))
                                                         bt:*default-special-bindings*))
     (with-protect ("Error on separate thread: ~a" :condition)
       ,@body)))

(defun evaluate (string)
  "Evaluate all expressions in STRING and return the last result as a list of values.
The list of values is useful when the last result is multi-valued, e.g. (values 'a 'b).
You need not wrap multiple values in a PROGN, all top-level expressions are
evaluated in order."
  (let ((channel (make-channel 2)))
    (run-thread "evaluator"
      (let ((*standard-output* (make-string-output-stream)))
        (lparallel.queue:push-queue
         (with-input-from-string (input string)
           (first
            (last
             (mapcar (lambda (s-exp)
                       (multiple-value-list
                        (with-protect ("Error in s-exp evaluation: ~a" :condition)
                          (eval s-exp))))
                     (safe-slurp-stream-forms input)))))
         channel)
        (lparallel.queue:push-queue (get-output-stream-string *standard-output*) channel)))
    (values (lparallel.queue:pop-queue channel) (lparallel.queue:pop-queue channel))))

(defun evaluate-async (string)
  "Like `evaluate' but does not block and does not return the result."
  (run-thread "async evaluator"
    (with-input-from-string (input string)
      (dolist (s-exp (safe-slurp-stream-forms input))
        (funcall (lambda () (with-protect ("Error in s-exp evaluation: ~a" :condition)
                              (eval s-exp))))))))
