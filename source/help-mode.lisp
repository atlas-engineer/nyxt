(uiop:define-package :next/help-mode
  (:use :common-lisp :trivia :next)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for help pages"))
(in-package :next/help-mode)

(define-mode help-mode ()
  "Mode for displaying documentation."
  ())
