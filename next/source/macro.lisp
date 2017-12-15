;;;; macro.lisp --- macros used in nEXT

(in-package :next)

;; used to provide input to buffers, "function" must accept input from
;; the minibuffer
(defmacro :input (minibuffer function &rest rest)
  `#'(lambda () (input (mode ,minibuffer) #',function ,@rest)))

;; used to provide input to buffers with an optional completion
;; function, the completion function must narrow a list of candidates
;; when given input
(defmacro :input-complete (minibuffer function completion &rest rest)
  `#'(lambda () (input (mode ,minibuffer) #',function :completion #',completion ,@rest)))

;; used to allow inlining of parenscript compilation in a lisp file.
;; with the syntax (defparenstatic name) allows definition of a paren
;; to some constant of name "name"
(defmacro defparenstatic (script-name &rest script-body)
  `(progn
     (defparameter ,script-name
       (ps:ps ,@script-body))
     (defun ,script-name (&optional (buffer *active-buffer*))
       (let ((script-result (interface:web-view-execute (view buffer) ,script-name)))
         (when (not (equalp "" script-result))
           (cl-json:decode-json-from-string script-result))))))

;; allow inlining of a parenscript function that can accept arguments,
;; useful for parenscript that will accept variables from lisp
(defmacro defparen (name lambda-list &body body)
  `(defun ,name (,@lambda-list)
       (ps:ps ,@body)))
