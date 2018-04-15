;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; next.asd

(asdf:defsystem :next
  :serial t
  :depends-on (:alexandria :cl-strings :cl-string-match :puri
               :queues.simple-queue :sqlite :parenscript :cl-json :swank)
  :pathname "source/"
  :components ((:file "package")
	       (:file "macro")
               (:file "user")
	       (:file "global")
	       (:file "hook")
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
	       (:file "hydra")
	       (:file "base")))

(asdf:defsystem :next/cocoa
  :depends-on (:next (:require "cocoa") (:require "webkit"))
  :pathname "source/"
  :components ((:file "cocoa/repl")
               (:file "cocoa/utility")
               (:file "cocoa/cocoa")))

(asdf:defsystem :next/cocoa/application
  :depends-on (:next/cocoa)
  :pathname "source/"
  :components ((:file "cocoa/application")))

(asdf:defsystem :next/gtk
  :depends-on (:next :cl-cffi-gtk :cl-webkit2 :lparallel)
  :pathname "source/"
  :components ((:file "gtk/gtk"))

  ;; build executable
  :build-operation "program-op"
  ;; built in source/next-gtk
  :build-pathname "next-gtk"
  :entry-point "next:start")
