;; noscript-mode.lisp --- Disable Javascript.

(in-package :next)

(define-mode noscript-mode ()
    "Disable Javascript in current buffer."
    ((destructor
      :initform
      (lambda (mode)
        (%%buffer-set *interface* (buffer mode) "enable-javascript" t)
        (echo "Noscript disabled."))))
  (%%buffer-set *interface* (buffer %mode) "enable-javascript" nil)
  (echo "Noscript enabled."))
