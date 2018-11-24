;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; next.asd

(asdf:defsystem :next
  :serial t
  :defsystem-depends-on ("trivial-features")
  :depends-on (:alexandria :cl-strings :cl-string-match :puri
               :queues.simple-queue :sqlite :parenscript :cl-json :swank
               :cl-markup :cl-css :usocket :bordeaux-threads :s-xml-rpc
               :unix-opts)
  :components ((:module "source"
                :components
                (;; Core Functionality
                 (:file "package")
	         (:file "macro")
	         (:file "global")
	         (:file "hook")
	         (:file "command")
	         (:file "utility")
                 (:file "remote")
	         (:file "mode")
	         (:file "buffer")
	         (:file "window")
	         (:file "minibuffer")
	         (:file "keymap")
                 ;; Core Packages
	         (:file "bookmark")
                 (:file "zoom")
                 (:file "scroll")
	         (:file "history")
	         (:file "search-buffer")
	         (:file "jump-heading")
	         (:file "link-hint")
	         (:file "help")
                 ;; Core Modes
	         (:file "application-mode")
	         (:file "document-mode")
	         (:file "base")))
               (:module "source/ports"
                :components
                ((:file "cocoa" :if-feature :darwin)
                 (:file "gtk" :if-feature (:and :unix (:not :darwin))))))
  :build-operation "program-op"
  :build-pathname "next"
  :entry-point "next:start")

(asdf:defsystem :next/release
  :depends-on (:next)
  :components ((:module "source/ports"
                :components
                ((:file "cocoa-release" :if-feature :darwin)
                 (:file "gtk-release" :if-feature (:and :unix (:not :darwin))))))
  :build-operation "program-op"
  :build-pathname "next"
  :entry-point "next:start")
