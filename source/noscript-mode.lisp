(uiop:define-package :nyxt/noscript-mode
    (:use :common-lisp :trivia :nyxt)
  (:documentation "Disable Javascript."))
(in-package :nyxt/noscript-mode)

(define-mode noscript-mode ()
    "Disable Javascript in current buffer."
    ((destructor
      :initform
      (lambda (mode)
        (ffi-buffer-enable-javascript-markup (buffer mode) t)))
     (constructor
      :initform
      (lambda (mode)
        (ffi-buffer-enable-javascript-markup (buffer mode) nil)))))
