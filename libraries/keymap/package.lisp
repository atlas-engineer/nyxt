(in-package :cl-user)

(defpackage :keymap
  (:use :common-lisp)
  (:import-from #:fset)
  (:import-from #:str)
  (:local-nicknames (#:alex #:alexandria))
  (:local-nicknames (#:types #:trivial-types))
  (:export
   modifier=
   +control+
   +meta+
   +shift+
   +super+
   +hyper+
   modifier-list

   make-key
   key-code
   key-value
   key-modifiers
   key-status

   define-key
   make-keymap))
