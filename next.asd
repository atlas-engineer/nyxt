;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; next.asd

(asdf:defsystem :next
  :serial t
  :depends-on (:alexandria :cl-strings :cl-string-match :puri
               :queues.simple-queue :sqlite :parenscript :cl-json :swank
               :cl-markup :cl-css :usocket :bordeaux-threads :s-xml-rpc
               :unix-opts)
  :pathname "source/"
  :components ((:file "package")
               (:file "remote")
	       (:file "macro")
               (:file "user")
	       (:file "global")
	       (:file "hook")
	       (:file "command")
	       (:file "utility")
	       (:file "window")
	       (:file "mode")
	       (:file "keymap")
	       (:file "minibuffer")
	       (:file "buffer")
	       (:file "bookmark")
	       (:file "history")
	       (:file "search-buffer")
	       (:file "jump-heading")
	       (:file "link-hint")
	       (:file "completion")
	       (:file "application-mode")
	       (:file "document-mode")
	       (:file "help")
	       (:file "base")))
