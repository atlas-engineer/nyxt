;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; next.asd
(defsystem :next
  :serial t
  :depends-on (:cl-strings :cl-string-match :puri :queues.simple-queue)
  :pathname "lisp/"
  :components ((:file "package")
	       (:file "macro")
	       (:file "qt")
	       (:file "mode")
	       (:file "keymap")
	       (:file "minibuffer")
	       (:file "buffer")
	       (:file "tree-history-mode")
	       (:file "document-mode")
	       (:file "completion")
	       (:file "base")))
