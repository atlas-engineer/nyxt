;;; macro.lisp --- macros used in Next

(in-package :next)

(defmacro define-parenstatic (script-name &rest script-body)
  "Define parenscript SCRIPT-NAME at compile time.
SCRIPT-NAME is both a variable containing the script and a function evaluating
it on the platform port."
  `(progn
     (defparameter ,script-name
       (ps:ps ,@script-body))
     (defun ,script-name (&optional (callback nil) (buffer (active-buffer *interface*)))
       (%%buffer-evaluate-javascript *interface* buffer ,script-name callback))))

(defmacro define-parenscript (name lambda-list &body body)
  "Inline a parenscript function that can accept arguments.
This is useful for parenscript that will accept variables from Lisp."
  `(defun ,name (,@lambda-list)
     (ps:ps ,@body)))

(defmacro with-result ((symbol async-form) &body body)
  `(,(first async-form)
    (lambda (,symbol) ,@body)
    ,@(rest async-form)))

(defmacro with-result* (bindings &body body)
  (if (null bindings)
    `(progn ,@body)
    `(with-result ,(first bindings)
       (with-result* ,(rest bindings) ,@body))))
