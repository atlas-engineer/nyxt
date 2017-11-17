;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; next.asd
(defsystem :next
  :serial t
  :depends-on (:cl-strings :cl-string-match :puri :queues.simple-queue :sqlite
			   (:require "cocoa") (:require "webkit"))
  :pathname "source/"
  :components ((:file "package")
	       (:file "global")
	       (:file "cocoa/utilities")
	       (:file "cocoa/cocoa")
	       (:file "cocoa/application")
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
