;;;; macro.lisp --- macros used in nEXT

(in-package :next)

;; used to provide input to buffers, "function" must accept input from
;; the minibuffer
(defmacro :input (minibuffer function)
  `#'(lambda () (input (buffer-mode ,minibuffer) #',function)))

;; used to provide input to buffers with an optional completion
;; function, the completion function must narrow a list of candidates
;; when given input
(defmacro :input-complete (minibuffer function completion)
  `#'(lambda () (input (buffer-mode ,minibuffer) #',function #',completion)))
