;; noscript-mode.lisp --- Disable Javascript.

(in-package :next)

(define-mode noscript-mode ()
    "Disable Javascript in current buffer."
    ((destructor
      :initform
      (lambda (mode)
        (rpc-buffer-set *interface* (buffer mode) "enable-javascript-markup" t))))
  (rpc-buffer-set *interface* (buffer %mode) "enable-javascript-markup" nil))
