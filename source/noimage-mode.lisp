(uiop:define-package :next/noimage-mode
    (:use :common-lisp :trivia :next)
  (:documentation "Disable images."))
(in-package :next/noimage-mode)

(define-mode noimage-mode ()
    "Disable images in current buffer."
    ((destructor
      :initform
      (lambda (mode)
        (ffi-buffer-auto-load-image (buffer mode) t)))
     (constructor
      :initform
      (lambda (mode)
        (ffi-buffer-auto-load-image (buffer mode) nil)))))
