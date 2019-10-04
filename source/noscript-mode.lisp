(uiop:define-package :next/noscript-mode
    (:use :common-lisp :trivia :next)
  (:documentation "Disable Javascript."))
(in-package :next/noscript-mode)

(define-mode noscript-mode ()
    "Disable Javascript in current buffer."
    ((destructor
      :initform
      (lambda (mode)
        (rpc-buffer-set (buffer mode) "enable-javascript-markup" t)))
     (constructor
      :initform
      (lambda (mode)
        (rpc-buffer-set (buffer mode) "enable-javascript-markup" nil)))))
