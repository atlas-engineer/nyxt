;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; next.asd

(asdf:defsystem :next
  :serial t
  :depends-on (:alexandria :cl-strings :cl-string-match :puri
               :queues.simple-queue :sqlite :parenscript :cl-json :swank
               :cl-markup :cl-css :usocket :bordeaux-threads :s-xml-rpc
               :unix-opts)
  :pathname "source/"
  :components (;; Core Functionality
               (:file "package")
	       (:file "macro")
	       (:file "global")
	       (:file "hook")
	       (:file "command")
	       (:file "utility")
               (:file "remote")
	       (:file "mode")
	       (:file "window")
	       (:file "buffer")
	       (:file "minibuffer")
	       (:file "keymap")
               ;; Core Packages
	       (:file "bookmark")
               (:file "zoom")
	       (:file "history")
	       (:file "search-buffer")
	       (:file "jump-heading")
	       (:file "link-hint")
	       (:file "help")
               ;; Core Modes
	       (:file "application-mode")
	       (:file "document-mode")
	       (:file "base")))
