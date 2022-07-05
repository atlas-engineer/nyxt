;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/process-mode
    (:documentation "Act on file/directory based on a certain condition."))
(in-package :nyxt/process-mode)

(define-mode process-mode ()
  "Conditional execution a file/directory-related actions in a separate thread.

Possible applications:
- Web server.
- Live preview of documents (`preview-mode').
- Refreshing the website at regular intervals (`watch-mode').
- Live tracking of filesystem/data in a file/directory."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (path-url
    nil
    :type (or quri:uri null)
    :documentation "The path to where `process-mode' needs to track things at.
Is not necessarily the same as current buffer URL.")
   (firing-condition
    t
    :type (or boolean (function (quri:uri process-mode)))
    :documentation "The condition for the action firing.
Can be boolean (T to always fire, NIL to never fire), or function over URL and mode instance.
If it is/returns :RETURN, the process is stopped.")
   (action
    nil
    :type (or (function (quri:uri process-mode)) null)
    :documentation "The action (function) to do with file URL and `process-mode' instance.")
   (cleanup
    nil
    :type (or (function (quri:uri process-mode)) null)
    :documentation "Function to run when process ends.
Accepts the path to the acted-on document and `process-mode' instance.")
   (thread
    nil
    :type (or bt:thread null)
    :export nil
    :documentation "The thread that `action' happen in.")
   (thread-terminated-p
    nil
    :documentation "Has the thread been terminated?"))
  (:toggler-command-p nil))

(defmethod enable ((mode process-mode) &key)
  (setf (path-url mode) (or (path-url mode) (url (current-buffer)))
        (thread mode) (unless (thread-terminated-p mode)
                        (run-thread "process"
                          (loop with cond = (firing-condition mode)
                                with cond-func = (typecase cond
                                                   (function cond)
                                                   (boolean (constantly cond)))
                                for condition-value = (funcall cond-func (path-url mode) mode)
                                when (eq condition-value :return)
                                  do (progn
                                       (setf (thread-terminated-p mode) t)
                                       (disable-modes (sera:class-name-of mode) (buffer mode))
                                       (return))
                                else
                                  when condition-value
                                    do (with-current-buffer (buffer mode)
                                         (when (action mode)
                                           (funcall (action mode) (path-url mode) mode))))))))

(defmethod disable ((mode process-mode) &key)
  (and (cleanup mode)
       (funcall (cleanup mode) (path-url mode) mode))
  (unless (thread-terminated-p mode)
    (destroy-thread* (thread mode)))
  (setf (thread-terminated-p mode) t))
