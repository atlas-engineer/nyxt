;;; macro.lisp --- macros used in Next

(in-package :next)

;; used to allow inlining of parenscript compilation in a lisp file.
;; with the syntax (defparenstatic name) allows definition of a paren
;; to some constant of name "name"
(defmacro defparenstatic (script-name &rest script-body)
  `(progn
     (defparameter ,script-name
       (ps:ps ,@script-body))
     (defun ,script-name (&optional (buffer *active-buffer*))
       (let ((script-result (buffer-execute-javascript *interface* (view buffer) ,script-name)))
         (when (and script-result (not (equalp "" script-result)))
           (cl-json:decode-json-from-string script-result))))))

;; allow inlining of a parenscript function that can accept arguments,
;; useful for parenscript that will accept variables from lisp
(defmacro defparen (name lambda-list &body body)
  `(defun ,name (,@lambda-list)
     (ps:ps ,@body)))

(defmacro with-parenscript ((symbol script) &body body)
  `(buffer-execute-javascript *interface*
    (view *active-buffer*) ,script
    (lambda (json-execution-result)
      (let ((,symbol (cl-json:decode-json-from-string json-execution-result)))
        ,@body))))

(defmacro with-result ((symbol async-form) &body body)
  `(,(first async-form)
    (lambda (,symbol) ,@body)
    ,@(rest async-form)))

(defmacro with-result* (bindings &body body)
  (if (null bindings)
    `(progn ,@body)
    `(with-result ,(first bindings)
       (with-result* ,(rest bindings) ,@body))))

(defmacro deferredvar (variable value &optional (documentation nil))
  `(progn
     (defvar ,variable nil ,documentation)
     (push (lambda () (setf ,variable ,value)) *deferred-variables*)))
