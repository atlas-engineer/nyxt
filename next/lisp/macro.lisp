;;;; macro.lisp --- macros used in nEXT

(in-package :next)

;; used to provide input to buffers
(defmacro :input (function)
  `#'(lambda () (input #',function)))

(defmacro :input-complete (function completion)
  `#'(lambda () (input #',function #',completion)))
