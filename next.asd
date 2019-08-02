;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; next.asd

(asdf:defsystem :next
  :version "1.2.2"
  :author "Atlas Engineer LLC"
  :license "BSD 3-Clause"
  :serial t
  :defsystem-depends-on ("trivial-features")
  :depends-on (:alexandria
               :bordeaux-threads
               :cl-css
               :cl-json
               :cl-markup
               :cl-ppcre
               :cl-ppcre-unicode
               :cl-string-match
               :cl-strings
               :closer-mop
               :dbus
               :dexador
               :ironclad
               :log4cl
               :lparallel
               :mk-string-metrics
               :parenscript
               :quri
               :sqlite
               :str
               :swank
               :trivia
               :trivial-clipboard
               :unix-opts
               ;; Local systems:
               :download-manager)
  :components ((:module "source"
                :components
                (;; Core Functionality
                 (:file "package")
                 (:file "macro")
                 (:file "global")
                 (:file "port")
                 (:file "remote")
                 (:file "mode")
                 (:file "command")
                 (:file "utility")
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
                 (:file "vi-mode")
                 (:file "blocker-mode")
                 (:file "proxy-mode")
                 (:file "noscript-mode")
                 (:file "download-mode")
                 ;; About
                 (:file "about")
                 ;; Port Compatibility Layers
                 (:file "ports/pyqt-webengine" :if-feature :darwin)
                 (:file "ports/gtk-webkit" :if-feature (:and :unix (:not :darwin)))
                 ;; Base
                 (:file "base"))))
  :build-operation "program-op"
  :build-pathname "next"
  :entry-point "next:entry-point")

(asdf:defsystem download-manager
  :depends-on (cl-ppcre
               dexador
               log4cl
               lparallel
               quri
               str)
  :components ((:module source :pathname "libraries/download-manager/"
                :components ((:file "package")
                             (:file "engine")
                             (:file "native")))))

(asdf:defsystem download-manager/tests
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove
               download-manager)
  :components ((:module source/tests :pathname "libraries/download-manager/tests/"
                :components ((:test-file "tests"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call
                                 :prove-asdf 'run-test-system c)))
