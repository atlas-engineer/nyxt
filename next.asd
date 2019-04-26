;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; next.asd

(defsystem next
  :version "1.2.0"
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
               :unix-opts
               :trivial-clipboard
               :find-port
               :log4cl
               :closer-mop)
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

(defsystem next/engine
                                        ; Can't depend on next!
  :depends-on (lparallel)

                                        ; So we include package
                                        ; definitions for requirees of
                                        ; this system.
  :components ((:module base :pathname "source/"
                        :components ((:file "package")
                                     (:file "config")))
               (:module source :pathname "source/"
                        :depends-on (base)
                        :components ((:file "engine")))))
  
(defsystem next/engine/https
  :depends-on (next/engine
               dexador  
               puri)
  :components ((:module source :pathname "source/"
                        :components ((:file "download")))))

(defsystem next/t
  :in-order-to ((test-op (test-op next/engine/t))))

;;; TODO fix upstream ASDF to successfully locate and load
;;;      DEFSYSTEM-DEPENDS-ON clauses
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (ignore-errors (asdf:find-system :prove-asdf))
    (ql:quickload :prove-asdf)))

(defsystem next/engine/t
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove
                      next/engine/https)
  :components ((:module source/t :pathname "source/t/"
                :components ((:test-file "manager"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call
                                 :prove-asdf 'run-test-system c)))







