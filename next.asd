;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; next.asd

(asdf:defsystem :next
  :version "1.3.1"
  :author "Atlas Engineer LLC"
  :license "BSD 3-Clause"
  :serial t
  :defsystem-depends-on ("trivial-features")
  :depends-on (:alexandria
               :bordeaux-threads
               :cl-annot
               :cl-css
               :cl-hooks
               :cl-json
               :cl-markup
               :cl-ppcre
               :cl-ppcre-unicode
               :closer-mop
               :dbus
               :dexador
               :ironclad
               :local-time
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
               :next/download-manager
               :next/ring)
  :components ((:module "source"
                :components
                (;; Core Functionality
                 (:file "package")
                 (:file "macro")
                 (:file "global")
                 (:file "port")
                 (:file "remote")
                 (:file "command")
                 (:file "mode")
                 (:file "utility")
                 (:file "buffer")
                 (:file "window")
                 (:file "minibuffer")
                 (:file "keymap")
                 (:file "recent-buffers")
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
                 (:file "file-manager-mode")
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

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(asdf:defsystem next/download-manager
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

(asdf:defsystem next/download-manager/tests
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove
               next/download-manager)
  :components ((:module source/tests :pathname "libraries/download-manager/tests/"
                :components ((:test-file "tests"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call
                                 :prove-asdf 'run-test-system c)))

(asdf:defsystem next/ring
  :depends-on (trivial-clipboard
               cl-annot)
  :components ((:module source :pathname "libraries/ring/"
                :components ((:file "package")
                             (:file "ring")))))

(asdf:defsystem next/ring/tests
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove
               next/ring)
  :components ((:module source/tests :pathname "libraries/ring/tests/"
                :components ((:test-file "tests"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call
                                 :prove-asdf 'run-test-system c)))
