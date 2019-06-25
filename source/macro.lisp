;;; macro.lisp --- macros used in Next

(in-package :next)

;; TODO: The distinction between compile-time script and runtime scripts is confusing.
;; It's tempting to write legal PS that depends on run-time values while passing no parameters.
;; Make parenscript always dynamic?
(defmacro define-parenscript (script-name args &body script-body)
  "Define parenscript SCRIPT-NAME.
SCRIPT-BODY must be a valid parenscript and will be wrapped in (PS:PS ...).
Any Lisp expression must be wrapped in (PS:LISP ...).

Without arguments, the script is defined at compile time and thus cannot contain
references to any dynamic value.
If a script without argument must be kept dynamic, pass it the \"_\" dummy parameter.

The returned function is called over 3 key arguments beside ARGS:
- %CALLBACK: a function to call when the script returns.  Defaults to nil.
- %INTERFACE: The remote-interface, defaults to *interface*.
- %BUFFER: The buffer used to execute the script.  Defaults to the current buffer.

Those variables can be used from the body (the parenscript code).

ARGS are also key arguments."
  `(progn
     ,(unless args
        `(defparameter ,script-name
           (ps:ps ,@script-body)))
     (defun ,script-name ,(append '(&key ((:callback %callback) nil)
                                    ((:interface %interface) *interface*)
                                    ((:buffer %buffer) (active-buffer %interface)))
                           args)
       ,(when (and args (eq (first args) '_))
          `(declare (ignore _)))
       (rpc-buffer-evaluate-javascript %interface %buffer
                                     ,(if args
                                          `(ps:ps ,@script-body)
                                          script-name)
                                     %callback))))

(defmacro with-result ((symbol async-form) &body body)
  "Call ASYNC-FORM.
When ASYNC-FORM returns, its result is bound to SYMBOL and BODY is executed.
ASYNC-FORM is a function that has at least a :CALLBACK key argument.

Example:

  (with-result (url (read-from-minibuffer (minibuffer *interface*)
                                          :input-prompt \"Bookmark URL:\"))
    (%bookmark-url url))"
  `(,(first async-form)
    ,@(rest async-form)
    :callback (lambda (,symbol) ,@body)))

(defmacro with-result* (bindings &body body)
  "Like WITH-RESULT but allows for chained asynchronous bindings.

Example:

  (with-result* ((url (buffer-get-url))
                 (title (buffer-get-title)))
    (rpc-window-set-title *interface* (rpc-window-active *interface*)
                        (concatenate 'string \"Next - \" title \" - \" url)))
"
  (if (null bindings)
    `(progn ,@body)
    `(with-result ,(first bindings)
       (with-result* ,(rest bindings) ,@body))))
