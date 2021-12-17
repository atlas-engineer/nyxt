;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun initialize-lparallel-kernel (&key (worker-count (sera:count-cpus)))
  "Initialize the lparallel kernel with WORKER-COUNT, if not supplied set it to
the amount of CPU cores.."
  (unless lpara:*kernel*
    (setf lpara:*kernel* (lpara:make-kernel worker-count))))

(export-always 'with-protect)
(defmacro with-protect ((format-string &rest args) &body body)
  "Run body with muffled conditions when `*run-from-repl-p*' is nil, run normally otherwise.
When the condition is muffled, a warning is reported to the user as per
FORMAT-STRING and ARGS.
As a special case, the first `:condition' keyword in ARGS is replaced with the
raised condition."
  (alex:with-gensyms (c sub-c)
    `(if *run-from-repl-p*
         (handler-case (progn ,@body)
           (nyxt-prompt-buffer-canceled ()
             (log:debug "Prompt buffer interrupted")))
         (ignore-errors
          (handler-bind
              ((error
                 (lambda (,c)
                   (declare (ignorable ,c))
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
                           (echo-warning ,format-string ,@(new-args c condition-index :escaped-p))))))))
            ,@body)))))

(defun make-channel (&optional size)
  "Return a channel of capacity SIZE.
If SIZE is NIL, capicity is infinite."
  (cond
    ((null size)
     (make-instance 'calispel:channel
                    :buffer (make-instance 'jpl-queues:unbounded-fifo-queue)))
    ((zerop size)
     (make-instance 'calispel:channel))
    ((plusp size)
     (make-instance 'calispel:channel
                    :buffer (make-instance 'jpl-queues:bounded-fifo-queue :capacity size)))))

(defun drain-channel (channel &optional timeout)
  "Listen to CHANNEL until a value is available, then return all CHANNEL values
as a list.
TIMEOUT specifies how long to wait for a value after the first one.
This is a blocking operation."
  (labels ((fetch ()
             (multiple-value-bind (value received?)
                 (calispel:? channel timeout)
               (if received?
                   (cons value (fetch))
                   nil))))
    (cons (calispel:? channel)
          (nreverse (fetch)))))

(export-always 'run-thread)
(defmacro run-thread (&body body)
  "Run body in a new protected new thread.
This supersedes `bt:make-thread' in Nyxt.  Don't use the latter unless you know
what you are doing!"
  ;; We parse the body instead of using a macro argument for backward compatibility.
  (let* ((name (if (stringp (first body))
                   (first body)
                   "anonymous thread"))
         (body (if (stringp (first body))
                   (rest body)
                   body)))
    `(bt:make-thread
      (lambda ()
        (with-protect ("Error on separate thread: ~a" :condition)
          ,@body))
      :name ,(str:concat "Nyxt " name))))
