;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/process-mode
    (:documentation "Act on file/directory based on a certain condition."))
(in-package :nyxt/process-mode)

(define-mode process-mode ()
  "Conditionally execute a file/directory-related `action' in a separate thread.

Possible applications:
- Web server.
- Live preview of documents (`nyxt/preview-mode').
- Refreshing a URL at regular intervals (`nyxt/watch-mode').
- Live tracking of filesystem/data in a file/directory."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (path-url
    nil
    :type (or quri:uri null)
    :documentation "Path where `process-mode' executes an `action'.
It's not necessarily the same as the current buffer's URL.")
   (firing-condition
    t
    :type (or boolean (function (quri:uri process-mode)))
    :documentation "The condition for triggering the action.
Can be boolean (T to always fire, NIL to never fire), or function over URL and
mode instance.  If it is/returns :RETURN, the process is stopped.")
   (action
    nil
    :type (or (function (quri:uri process-mode)) null)
    :documentation "The function to do with file URL and `process-mode'
instance.")
   (cleanup
    nil
    :type (or (function (quri:uri process-mode)) null)
    :documentation "Function to run when process ends.
Accepts the path to the acted-on document and `process-mode' instance.")
   (thread
    nil
    :type (or bt:thread null)
    :export nil
    :documentation "Thread that `action' happen in."))
  (:toggler-command-p nil))

(defmethod thread-alive-p ((mode process-mode))
  (and (thread mode) (bt:thread-alive-p (thread mode))))

(defmethod enable ((mode process-mode) &key)
  (when (and (firing-condition mode)
             (action mode)
             (not (thread-alive-p mode)))
    (setf (path-url mode)
          (or (path-url mode) (url (current-buffer))))
    (setf (thread mode)
          (run-thread "process"
            (loop with cond = (firing-condition mode)
                  with cond-func = (typecase cond
                                     (function cond)
                                     (boolean (constantly cond)))
                  for condition-value = (funcall cond-func (path-url mode) mode)
                  when (eq condition-value :return)
                    do (progn
                         (disable-modes* (sera:class-name-of mode) (buffer mode))
                         (return))
                  else
                    when condition-value
                      do (with-current-buffer (buffer mode)
                           (when (action mode)
                             (funcall (action mode) (path-url mode) mode))))))))

(defmethod disable ((mode process-mode) &key)
  (and (cleanup mode)
       (funcall (cleanup mode) (path-url mode) mode))
  (destroy-thread* (thread mode)))
