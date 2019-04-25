;;; application-mode.lisp --- major mode for "web applications"
;;; has no history or bindings to avoid interference with "web applications"'

(in-package :next)

(define-mode application-mode ()
    "Mode that forwards all keys to the platform port."
  ())
