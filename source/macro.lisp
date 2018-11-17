;;; macro.lisp --- macros used in Next

(in-package :next)

;; used to allow inlining of parenscript compilation in a lisp file.
;; with the syntax (define-parenstatic name) allows definition of a paren
;; to some constant of name "name"
(defmacro define-parenstatic (script-name &rest script-body)
  `(progn
     (defparameter ,script-name
       (ps:ps ,@script-body))
     (defun ,script-name (&optional (callback nil) (buffer (active-buffer *interface*)))
       (buffer-evaluate-javascript *interface* buffer ,script-name callback))))

;; allow inlining of a parenscript function that can accept arguments,
;; useful for parenscript that will accept variables from lisp
(defmacro define-parenscript (name lambda-list &body body)
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

(defmacro deferredvar (variable value &optional (documentation nil))
  `(progn
     (defvar ,variable nil ,documentation)
     (push (lambda () (setf ,variable ,value)) *deferred-variables*)))
