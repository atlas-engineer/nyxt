(in-package :cl-user)

(defpackage :keymap
  (:use :common-lisp)
  (:import-from #:fset)
  (:local-nicknames (#:alex #:alexandria))
  (:export define-key
           make-keymap))
