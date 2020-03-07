(in-package :cl-user)

(defpackage :next-keymap
  (:use :common-lisp)
  (:import-from #:fset)
  (:local-nicknames (#:alex #:alexandria))
  (:export define-key
           make-keymap))
