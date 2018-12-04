;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; next.asd

(asdf:defsystem :next
  :version "1.1.0"
  :author "Atlas Engineer LLC"
  :license "BSD 3-Clause"
  :serial t
  :defsystem-depends-on ("trivial-features")
  :depends-on (:alexandria
               :cl-strings
               :cl-string-match
               :puri
               :sqlite
               :parenscript
               :cl-json
               :swank
               :cl-markup
               :cl-css
               :bordeaux-threads
               :s-xml-rpc
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
                 (:file "port")
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
                 ;; Port Compatibility Layers
                 (:file "ports/cocoa-webkit" :if-feature :darwin)
                 (:file "ports/gtk-webkit" :if-feature (:and :unix (:not :darwin)))
                 ;; Base
	         (:file "base"))))
  :build-operation "program-op"
  :build-pathname "next"
  :entry-point "next:start-with-port")
