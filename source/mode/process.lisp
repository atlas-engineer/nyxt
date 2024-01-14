;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/process
  (:documentation "Package for `process-mode', mode to conditionally act on file/directory.

APIs that `process-mode' has:
- Slots:
  - `path-url' to set the path tracked by the mode.
  - `firing-condition', continuously checked for whether to run the...
  - `action'---the actual thing to run.
  - `cleanup' runs after `firing-condition' returns :RETURN to clean up the mode.
  - `thread' as the thread that all the `action's happen on. Usually needs not
    be altered, because the default is intuitive enough.
- Internal helpers:
  - `thread-alive-p' to check on the `thread'.
  - `call-cleanup' to relay things to `cleanup' code.

It also uses threading utilities like `destroy-thread*', `sera:synchronized',
and `bt:thread-alive-p'."))
(in-package :nyxt/mode/process)

(define-mode process-mode ()
  "Conditionally execute a file/directory-related `action' in a separate thread.

Possible applications:
- Web server.
- Live preview of documents (`nyxt/mode/preview').
- Refreshing a URL at regular intervals (`nyxt/mode/watch').
- Live tracking of filesystem/data in a file/directory.

The mode itself should not be used directly. Rather, it should be subclassed and
extended with custom logic.

See `nyxt/mode/process' package documentation for implementation details and internal programming APIs."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (path-url
    nil
    :type (or quri:uri null)
    :documentation "URL where `action' runs.
It's not necessarily the same as the current buffer's URL.")
   (firing-condition
    t
    :type (or boolean (function (quri:uri process-mode)))
    :documentation "The condition for triggering `action'.
It's either a boolean (T to always fire, NIL to never fire) or a function of the
URL and mode instance.  When the function returns :RETURN, the process is
stopped.")
   (action
    nil
    :type (or (function (quri:uri process-mode)) null)
    :documentation "Function that takes a URL and a `process-mode' instance as
arguments.")
   (cleanup
    nil
    :type (or (function (quri:uri process-mode)) null)
    :documentation "Function to run when process ends.
Accepts the path to the acted-on document and `process-mode' instance.")
   (thread
    nil
    :type (or bt:thread null)
    :export nil
    :documentation "Thread where `action' runs."))
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
                           (sera:synchronized (mode)
                             (funcall* (action mode) (path-url mode) mode))))))))

(defmethod call-cleanup ((mode process-mode) &key)
  (funcall* (cleanup mode) (path-url mode) mode))

(defmethod disable ((mode process-mode) &key)
  (call-cleanup mode)
  (destroy-thread* (thread mode)))
