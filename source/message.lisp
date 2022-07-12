;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defclass messages-appender (log4cl-impl:appender) ())

(defmethod log4cl-impl:appender-do-append ((appender messages-appender) logger level log-func)
  (when (<= level (if (getf *options* :verbose)
                      log4cl:+log-level-warn+
                      log4cl:+log-level-error+))
    (uiop:print-backtrace))
  (when *browser*
    (push
     ;; TODO: Include time in *Messages* entries.
     ;; (make-instance 'log4cl:pattern-layout :conversion-pattern "<%p> [%D{%H:%M:%S}] %m%n" )
     (with-output-to-string (s)
       (log4cl-impl:layout-to-stream
        (slot-value appender 'log4cl-impl:layout) s logger level log-func))
     (slot-value *browser* 'messages-content))))

(defmacro %echo (text &key (logger 'log:info))
  "Echo TEXT in the message buffer.
LOGGER is the log4cl logger to user, for instance `log:warn'."
  (alex:with-gensyms (expanded-text)
    `(progn
       (let ((,expanded-text ,text))
         (unless (str:emptyp ,expanded-text)
           (,logger "~a" ,expanded-text))
         ;; Allow empty strings to clear message area.
         (print-message ,expanded-text)))))

(defun %echo-help (args)
  (log:warn "Failed to echo these args: ~s
Possible improvements:
- Pass multiple arguments and use format strings for untrusted content. Don't pre-construct a single string that could contain tildes.
  Example: do (echo \"directory is\ ~~a \"~~/Downloads/\")
           instead of (echo \"directory is ~~/Downloads/\")
- Use the ~~s directive." args))

(export-always 'echo)
(defun echo (&rest args)
  "Echo ARGS in the message view.
The first argument can be a format string and the following arguments will be
interpreted by `format'.
Untrusted content should be given as argument with a format string."
  (handler-case
      (let ((text (apply #'format nil args)))
        (%echo text))
    (error ()
      (%echo-help args))))

(export-always 'echo-warning)
(defun echo-warning (&rest args)
  "Like `echo' but prefix with \"Warning\" and output to the standard error."
  (handler-case
      (let ((text (apply #'format nil args)))
        (%echo (sera:fmt "Warning: ~a" text)
               :logger log:warn))
    (error ()
      (%echo-help args))))

(export-always 'echo-dismiss)
(defmethod echo-dismiss ()
  (%echo ""))
