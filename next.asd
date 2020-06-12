(asdf:defsystem :next
  :version "1.5.0"
  :author "Atlas Engineer LLC"
  :license "BSD 3-Clause"
  :serial t
  :depends-on (:alexandria
               :bordeaux-threads
               :cl-css
               :cl-json
               :cl-markup
               :cl-ppcre
               :cl-ppcre-unicode
               :cl-prevalence
               :closer-mop
               :cl-containers
               :moptilities
               :dexador
               :enchant
               :iolib
               :local-time
               :log4cl
               :lparallel
               :mk-string-metrics
               :osicat
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
               :next/text-analysis
               :next/download-manager
               :next/history-tree
               :next/password-manager
               :next/hooks
               :next/keymap)
  :pathname "source/"
  :components ((:file "patches/patch-serialization")
               (:file "package")
               ;; Independent utilities
               (:file "tags")
               (:file "time")
               (:file "types")
               (:file "conditions")
               ;; Core functionality
               (:file "macro")
               (:file "global")
               (:file "data-storage")
               (:file "browser")
               (:file "mode")
               (:file "command")
               (:file "utility")
               (:file "urls")
               (:file "fuzzy")
               (:file "message")
               (:file "input")
               (:file "minibuffer")
               (:file "minibuffer-mode")
               (:file "parse-urls")
               (:file "buffer")
               (:file "window")
               (:file "recent-buffers")
               (:file "password")
               (:file "bookmark")
               (:file "history")
               (:file "autofill")
               (:file "external-editor")
               ;; Core Modes
               (:file "base-mode")
               (:file "help-mode")
               (:file "application-mode")
               (:file "web-mode")
               (:file "certificate-whitelist-mode")
               (:file "emacs-mode")
               (:file "vi-mode")
               (:file "blocker-mode")
               (:file "proxy-mode")
               (:file "noimage-mode")
               (:file "noscript-mode")
               (:file "file-manager-mode")
               (:file "download-mode")
               (:file "vcs-mode")
               ;; Web-mode commands
               (:file "element-hint")
               (:file "jump-heading")
               (:file "scroll")
               (:file "search-buffer")
               (:file "spell-check")
               (:file "zoom")
               ;; Needs web-mode
               (:file "help")
               ;; Depends on everything else:
               (:file "about")
               (:file "session")
               (:file "start")
               (:file "tutorial")
               (:file "manual"))
  :in-order-to ((test-op (test-op "next/tests"))))

(asdf:defsystem next/tests
  :depends-on (next prove)
  :perform (asdf:test-op (op c)
                         (funcall (read-from-string "prove:run")
                                  (asdf:system-relative-pathname c "tests/"))))

(asdf:defsystem :next/gtk
  :depends-on (:next
               :cl-cffi-gtk
               :cl-webkit2)
  :pathname "source/"
  :components ((:file "renderer-gtk")))

(asdf:defsystem :next/qt
  :depends-on (:next
               :cl-webengine
               :trivial-main-thread)
  :pathname "source/"
  :components ((:file "renderer-qt")))

;; We should not set the build-pathname in systems that have a component.
;; Indeed, when an external program (like Guix) builds components, it needs to
;; know the name of the output.  But ASDF/SYSTEM::COMPONENT-BUILD-PATHNAME is
;; non-exported so the only reliable way to know the build pathname is to use
;; the default.
;;
;; The workaround is to set a new dummy system of which the sole purpose is to
;; produce the desired binary.

(asdf:defsystem :next/gtk-application
  :depends-on (:next/gtk)
  :build-operation "program-op"
  :build-pathname "next"
  :entry-point "next:entry-point")

(asdf:defsystem :next/qt-application
  :depends-on (:next/qt)
  :build-operation "program-op"
  :build-pathname "next-qt"
  :entry-point "next:entry-point")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression (not (null (uiop:getenv "NEXT_COMPRESS")))))

(asdf:defsystem next/download-manager
  :depends-on (cl-ppcre
               dexador
               log4cl
               lparallel
               quri
               str)
  :pathname "libraries/download-manager/"
  :components ((:file "package")
               (:file "engine")
               (:file "native"))
  :in-order-to ((test-op (test-op "next/download-manager/tests"))))

(asdf:defsystem next/download-manager/tests
  :depends-on (next/download-manager prove)
  :perform (asdf:test-op (op c)
                         (funcall (read-from-string "prove:run")
                                  (asdf:system-relative-pathname c "libraries/download-manager/tests/"))))

(asdf:defsystem next/text-analysis
  :depends-on (str)
  :pathname "libraries/text-analysis//"
  :components ((:file "package")
               (:file "data")
               (:file "stem")
               (:file "analysis")))

(asdf:defsystem next/history-tree
  :pathname "libraries/history-tree/"
  :components ((:file "package")
               (:file "history-tree"))
  :in-order-to ((test-op (test-op "next/history-tree/tests"))))

(asdf:defsystem next/history-tree/tests
  :depends-on (next/history-tree prove)
  :perform (asdf:test-op (op c)
                         (funcall (read-from-string "prove:run")
                                  (asdf:system-relative-pathname c "libraries/history-tree/tests/"))))

(asdf:defsystem next/password-manager
  :depends-on (bordeaux-threads
               cl-ppcre
               str
               trivial-clipboard
               uiop)
  :pathname "libraries/password-manager/"
  :components ((:file "package")
               (:file "password")
               (:file "password-keepassxc")
               (:file "password-security")
               ;; Keep password-store last so that it has higher priority.
               (:file "password-pass")))

(asdf:defsystem next/hooks
  :depends-on (alexandria serapeum)
  :pathname "libraries/hooks/"
  :components ((:file "package")
               (:file "hooks"))
  :in-order-to ((test-op (test-op "next/hooks/tests"))))

(asdf:defsystem next/hooks/tests
  :depends-on (next/hooks prove)
  :perform (asdf:test-op (op c)
                         (funcall (read-from-string "prove:run")
                                  (asdf:system-relative-pathname c "libraries/hooks/tests/"))))

(asdf:defsystem next/keymap
  :depends-on (alexandria fset str)
  :pathname "libraries/keymap/"
  :components ((:file "package")
                             (:file "types")
                             (:file "conditions")
                             (:file "keymap")
                             (:file "scheme")
                             (:file "scheme-names"))
  :in-order-to ((test-op (test-op "next/keymap/tests"))))

(asdf:defsystem next/keymap/tests
  :depends-on (alexandria fset next/keymap prove)
  :perform (asdf:test-op (op c)
                         (funcall (read-from-string "prove:run")
                                  (asdf:system-relative-pathname c "libraries/keymap/tests/"))))
