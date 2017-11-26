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
;; with the syntax (defparen name) allows definition of a paren
;; to some constant of name "name"
(defmacro defparen (script-name &rest script-body)
  `(defparameter ,script-name
     (ps:ps ,@script-body)))
