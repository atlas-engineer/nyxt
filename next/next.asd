;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; next.asd
(defsystem :next
  :serial t
  :depends-on (:cl-strings :cl-string-match :puri :queues.simple-queue :sqlite)
  :pathname "lisp/"
  :components ((:file "package")
	       (:file "qt")
	       (:file "macro")
	       (:file "mode")
	       (:file "keymap")
	       (:file "minibuffer")
	       (:file "buffer")
	       (:file "tree-history")
	       (:file "document-mode")
	       (:file "bookmark")
	       (:file "completion")
	       (:file "base")))
