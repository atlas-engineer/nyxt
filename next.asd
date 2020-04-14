(asdf:defsystem :next
  :version "1.5.0"
  :author "Atlas Engineer LLC"
  :license "BSD 3-Clause"
  :serial t
  :depends-on (:alexandria
               :bordeaux-threads
               :cl-ansi-text
               :cl-css
               :cl-json
               :cl-markup
               :cl-ppcre
               :cl-ppcre-unicode
               :cl-prevalence
               :closer-mop
               :dexador
               :enchant
               :iolib
               :ironclad
               :local-time
               :log4cl
               :lparallel
               :mk-string-metrics
               :parenscript
               :quri
               :serapeum
               :str
               :plump
               :swank
               :trivia
               :trivial-clipboard
               :trivial-features
               :trivial-package-local-nicknames
               :trivial-types
               :unix-opts
               ;; Local systems:
               :next/download-manager
               :next/ring
               :next/history-tree
               :next/password-manager
               :next/hooks
               :next/keymap)
  :components ((:module "source"
                :components
                ((:file "patches/patch-serialization")
                 (:file "package")
                 ;; Independent utilities
                 (:file "tags")
                 (:file "time")
                 (:file "types")
                 (:file "conditions")
                 ;; Core Functionality
                 (:file "macro")
                 (:file "desktop")
                 (:file "global")
                 (:file "browser")
                 (:file "mode")
                 (:file "command")
                 (:file "utility")
                 (:file "urls")
                 (:file "fuzzy")
                 (:file "buffer")
                 (:file "window")
                 (:file "minibuffer")
                 (:file "input")
                 (:file "recent-buffers")
                 ;; Core Packages
                 (:file "autofill")
                 (:file "password")
                 (:file "bookmark")
                 (:file "zoom")
                 (:file "scroll")
                 (:file "history")
                 (:file "search-buffer")
                 (:file "spell-check")
                 (:file "jump-heading")
                 (:file "element-hint")
                 (:file "help")
                 ;; Core Modes
                 (:file "application-mode")
                 (:file "web-mode")
                 (:file "certificate-whitelist-mode")
                 (:file "vi-mode")
                 (:file "blocker-mode")
                 (:file "proxy-mode")
                 (:file "noscript-mode")
                 (:file "file-manager-mode")
                 (:file "download-mode")
                 (:file "vcs-mode")
                 (:file "video-mode")
                 ;; Depends on everything else:
                 (:file "about")
                 (:file "session")
                 (:file "start")))))

(asdf:defsystem :next-gtk
  :depends-on (:next
               :cl-cffi-gtk
               :cl-webkit2)
  :components ((:module "source"
                :components ((:file "renderer-gtk"))))
  :build-operation "program-op"
  :build-pathname "next"
  :entry-point "next:entry-point")

(asdf:defsystem :next-qt
  :depends-on (:next
               :cl-webengine
               :trivial-main-thread)
  :components ((:module "source"
                :components ((:file "renderer-qt"))))
  :build-operation "program-op"
  :build-pathname "next-qt"
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

(asdf:defsystem next/keymap
  :depends-on (alexandria fset str)
  :components ((:module source :pathname "libraries/keymap/"
                :components ((:file "package")
                             (:file "types")
                             (:file "conditions")
                             (:file "keymap")
                             (:file "scheme")
                             (:file "scheme-names")))))

(asdf:defsystem next/keymap/tests
  :defsystem-depends-on (prove-asdf)
  :depends-on (alexandria fset prove next/keymap)
  :components ((:module source/tests :pathname "libraries/keymap/tests/"
                :components ((:test-file "tests")
                             (:test-file "scheme-tests"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call
                                 :prove-asdf 'run-test-system c)))
