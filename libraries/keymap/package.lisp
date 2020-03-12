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

   *modifier-list*
   *default-translator*

   make-key
   key-code
   key-value
   key-modifiers
   key-status

   make-keymap
   define-key
   lookup-key
   parents
   default
   translator

   translate-shifted-keys))
