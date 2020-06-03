(uiop:define-package :next/repl-mode
  (:use :common-lisp :next)
  (:import-from #:keymap #:define-key #:define-scheme))
(in-package :next/repl-mode)

(define-mode repl-mode ()
  "Mode for interacting with the REPL Documents."
  ((keymap-scheme
    :initform
    (define-scheme "repl"
      scheme:cua
      (list
       "C-up" 'scroll-to-top)
      scheme:emacs
      (list
       "M-f" 'history-forwards-query)
      scheme:vi-normal
      (list
       "H" 'history-backwards)))))
