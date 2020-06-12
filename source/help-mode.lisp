(uiop:define-package :nyxt/help-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for help pages"))
(in-package :nyxt/help-mode)

(define-mode help-mode ()
  "Mode for displaying documentation."
  ())
