(uiop:define-package :nyxt/noimage-mode
    (:use :common-lisp :trivia :nyxt)
  (:documentation "Disable images."))
(in-package :nyxt/noimage-mode)

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
