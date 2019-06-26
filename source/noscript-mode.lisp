;; noscript-mode.lisp --- Disable Javascript.

(in-package :next)

(define-mode noscript-mode ()
    "Disable Javascript in current buffer."
    ((destructor
      :initform
      (lambda (mode)
        (%%buffer-set *interface* (buffer mode) "enable-javascript" t))))
  (%%buffer-set *interface* (buffer %mode) "enable-javascript" nil))
