(in-package :next)

(defclass messages-appender (log4cl-impl:appender)
  ())

(defmethod log4cl-impl:appender-do-append ((appender messages-appender) logger level log-func)
  (push
   `(:p ,(with-output-to-string (s)
           (log4cl-impl:layout-to-stream
            (slot-value appender 'log4cl-impl:layout) s logger level log-func)))
   (messages-content *browser*)))

(defun %echo (text &key (message (list text))
                     (window (current-window)))
  "Echo TEXT in the message buffer.
When given, add MESSAGE to the `browser's `message-content' (for the `messages' buffer).
MESSAGE is a cl-markup list."
  (unless (or (null message)
              (null *browser*)
              (equal message '("")))
    (push `(:p (:i "["
                   ,(local-time:format-timestring
                     nil
                     (local-time:now)
                     :format local-time:+asctime-format+)
                   "]")
               " "
               ,@message)
          (messages-content *browser*)))
  ;; This function could be called before the renderer up.
  (when window
    (print-message text)))

(export-always 'echo)
(defun echo (&rest args)
  "Echo ARGS in the message view.
The first argument can be a format string and the following arguments will be
interpreted by `format'.
Untrusted content should be given as argument with a format string."
  (handler-case
      (let ((text (apply #'format nil args)))
        ;; We might still want to echo the empty string to clear the echo area.
        (%echo text)
        (unless (str:emptyp text)
          (log:info "~a" text)))
    (error ()
      (log:warn "Failed to echo these args: ~s
Possible improvements:
- Pass multiple arguments and use format strings for untrusted content. Don't pre-construct a single string that could contain tildes.
  Example: do (echo \"directory is\ ~~a \"~~/Downloads/\")
           instead of (echo \"directory is ~~/Downloads/\")
- Use the ~~s directive." args))))

(export-always 'echo-warning)
(defun echo-warning (&rest args)
  "Like `echo' but prefix with \"Warning\" and output to the standard error."
  (let ((text (apply #'format nil args)))
    (%echo text :message `((:b "Warning:") " " ,text))
    (unless (str:emptyp text)
      (log:warn "~a" text))))

(export-always 'echo-dismiss)
(defmethod echo-dismiss ()
  ;; Don't add to the *Messages* buffer:
  (%echo "" :message nil))
