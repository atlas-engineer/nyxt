;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; next.asd

(asdf:defsystem :next
  :version "1.3.4"
  :author "Atlas Engineer LLC"
  :license "BSD 3-Clause"
  :serial t
  :defsystem-depends-on ("trivial-features")
  :depends-on (:alexandria
               :bordeaux-threads
               :cl-annot
               :cl-ansi-text
               :cl-css
               :cl-json
               :cl-markup
               :cl-ppcre
               :cl-ppcre-unicode
               :cl-prevalence
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
               :serapeum
               :sqlite
               :str
               :plump
               :swank
               :trivia
               :trivial-clipboard
               :trivial-types
               :unix-opts
               ;; Local systems:
               :next/download-manager
               :next/ring
               :next/history-tree
               :next/password-manager
               :next/hooks)
  :components ((:module "source"
                :components
                ((:file "patch-annot")
                 (:file "patch-serialization")
                 ;; Independent utilities
                 (:file "package")
                 (:file "tags")
                 (:file "time")
                 (:file "types")
                 (:file "conditions")
                 (:file "file-human-size")
                 ;; Core Functionality
                 (:file "macro")
                 (:file "global")
                 (:file "port")
                 (:file "remote")
                 (:file "command")
                 (:file "mode")
                 (:file "utility")
                 (:file "urls")
                 (:file "fuzzy")
                 (:file "buffer")
                 (:file "window")
                 (:file "minibuffer")
                 (:file "keymap")
                 (:file "recent-buffers")
                 ;; Core Packages
                 (:file "password")
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
                 (:file "web-mode")
                 (:file "vi-mode")
                 (:file "blocker-mode")
                 (:file "proxy-mode")
                 (:file "noscript-mode")
                 (:file "file-manager-mode")
                 (:file "download-mode")
                 (:file "vcs-mode")
                 (:file "video-mode")
                 ;; Port Compatibility Layers
                 (:file "ports/pyqt-webengine" :if-feature :darwin)
                 (:file "ports/gtk-webkit" :if-feature (:and :unix (:not :darwin)))
                 ;; Depends on everything else:
                 (:file "about")
                 (:file "session")
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

(asdf:defsystem next/history-tree
  :components ((:module source :pathname "libraries/history-tree/"
                :components ((:file "package")
                             (:file "history-tree")))))

(asdf:defsystem next/history-tree/tests
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove
               next/history-tree)
  :components ((:module source/tests :pathname "libraries/history-tree/tests/"
                :components ((:test-file "tests"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call
                                 :prove-asdf 'run-test-system c)))

(asdf:defsystem next/password-manager
  :depends-on (bordeaux-threads
               cl-ppcre
               cl-annot
               str
               trivial-clipboard
               uiop)
  :components ((:module source :pathname "libraries/password-manager/"
                :components ((:file "package")
                             (:file "password")
                             (:file "password-pass")
                             (:file "password-keepassxc")))))

(asdf:defsystem next/hooks
  :depends-on (alexandria serapeum)
  :components ((:module source :pathname "libraries/hooks/"
                :components ((:file "package")
                             (:file "hooks")))))

(asdf:defsystem next/hooks/tests
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove
               next/hooks)
  :components ((:module source/tests :pathname "libraries/hooks/tests/"
                :components ((:test-file "tests"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call
                                 :prove-asdf 'run-test-system c)))
