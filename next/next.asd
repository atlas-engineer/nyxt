;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; next.asd

(defsystem :next
  :serial t
  :depends-on (:cl-strings :cl-string-match :puri :queues.simple-queue
                           :sqlite :parenscript :cl-json :swank)
  :pathname "source/"
  :components ((:file "package")
	       (:file "macro")
               (:file "user")
	       (:file "global")
	       (:file "command")
	       (:file "utility")
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

(defsystem :next/cocoa
  :depends-on (:next (:require "cocoa") (:require "webkit"))
  :pathname "source/"
  :components ((:file "cocoa/repl")
               (:file "cocoa/utility")
               (:file "cocoa/cocoa")))

(defsystem :next/cocoa/application
  :depends-on (:next/cocoa)
  :pathname "source/"
  :components ((:file "cocoa/application")))

(defsystem :next/gtk
  :depends-on (:next :cl-cffi-gtk :cl-webkit2 :lparallel)
  :pathname "source/"
  :components ((:file "gtk/gtk")))
