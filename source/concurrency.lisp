;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'with-muffled-body)      ; TODO: Rename to `with-protect'?
(defmacro with-muffled-body ((format-string &rest args) &body body)
  "Run body with muffled condition when `*keep-alive*' is nil, run normally otherwise.
Then the condition is muffled, a warning is reported to the user as per
FORMAT-STRING and ARGS.
As a special case, the first `:condition' keyword in ARGS is replaced with the
condition."
  (alex:with-gensyms (c)
    `(if *keep-alive*
         (progn ,@body)
         (handler-case (progn ,@body)
           (error (,c)
             (declare (ignorable ,c))
             ,(let* ((condition-index (position :condition args))
                     (new-args (if condition-index
                                   (append (subseq args 0 condition-index)
                                           `(,c)
                                           (subseq args (1+ condition-index)))
                                   args)))
                `(handler-case
                     (echo-warning ,format-string ,@new-args)
                   (t ()
                     (log:error ,format-string ,@new-args)))))))))

(defun make-channel (&optional size)
  "Return a channel of capacity SIZE.
If SIZE is NIL, capicity is infinite."
  (cond
    ((null size)
     (make-instance 'calispel:channel
                    :buffer (make-instance 'jpl-queues:unbounded-fifo-queue)))
    ((= 0 size)
     (make-instance 'calispel:channel))
    ((< 0 size)
     (make-instance 'calispel:channel
                    :buffer (make-instance 'jpl-queues:bounded-fifo-queue :capacity size)))))

(defun drain-channel (channel &optional timeout)
  "Listen to CHANNEL until a value is available, then return all CHANNEL values
as a list.
TIMEOUT specifies how long to wait when draining values after the first one.
This is a blocking operation."
  (labels ((fetch ()
             (multiple-value-bind (value received?)
                 (calispel:? channel 0)
               (if received?
                   (cons value (fetch))
                   nil))))
    (cons (calispel:? channel timeout)
          (nreverse (fetch)))))

(export-always 'run-thread)
(defmacro run-thread (&body body)
  "Run body in a new protected new thread.
This supersedes `bt:make-thread' in Nyxt.  Don't use the latter unless you know
what you are doing!"
  `(bt:make-thread
    (lambda ()
      (with-muffled-body ("Error on separate thead: ~a" :condition)
        ,@body))))
