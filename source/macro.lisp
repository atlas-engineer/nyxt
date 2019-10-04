;;; macro.lisp --- macros used in Next

(in-package :next)
(annot:enable-annot-syntax)

;; TODO: The distinction between compile-time script and runtime scripts is confusing.
;; It's tempting to write legal PS that depends on run-time values while passing no parameters.
;; Make parenscript always dynamic?
@export
(defmacro define-parenscript (script-name args &body script-body)
  "Define parenscript function SCRIPT-NAME.
SCRIPT-BODY must be a valid parenscript and will be wrapped in (PS:PS ...).
Any Lisp expression must be wrapped in (PS:LISP ...).

The returned function is called over 3 key arguments beside ARGS:
- %CALLBACK: a function to call when the script returns.  Defaults to nil.
- %BUFFER: The buffer used to execute the script.  Defaults to the current buffer.

Those variables can be used from the SCRIPT-BODY (the parenscript code).

ARGS must be key arguments."
  `(progn
     (defun ,script-name ,(append '(&key ((:callback %callback) nil)
                                    ((:buffer %buffer) (current-buffer)))
                           args)
       (rpc-buffer-evaluate-javascript %buffer
                                       (ps:ps ,@script-body)
                                       :callback %callback))))

@export
(defmacro with-result ((symbol async-form) &body body)
  "Call ASYNC-FORM.
When ASYNC-FORM returns, its result is bound to SYMBOL and BODY is executed.
ASYNC-FORM is a function that has at least a :CALLBACK key argument.

Example:

  (with-result (url (read-from-minibuffer
                     (make-instance 'minibuffer
                                    :input-prompt \"Bookmark URL:\"))
    (bookmark-add url))"
  `(,(first async-form)
    ,@(rest async-form)
    :callback (lambda (,symbol) ,@body)))

@export
(defmacro with-result* (bindings &body body)
  "Like WITH-RESULT but allows for chained asynchronous bindings.

Example:

  (with-result* ((links-json (add-link-hints))
                 (selected-hint (read-from-minibuffer
                                 (make-instance 'minibuffer
                                                :input-prompt \"Bookmark hint:\"
                                                :cleanup-function #'remove-link-hints))))
    ...)"
  (if (null bindings)
    `(progn ,@body)
    `(with-result ,(first bindings)
       (with-result* ,(rest bindings) ,@body))))
