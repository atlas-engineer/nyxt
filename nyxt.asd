(asdf:defsystem :nyxt
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
               :nyxt/text-buffer
               :nyxt/text-analysis
               :nyxt/download-manager
               :nyxt/history-tree
               :nyxt/password-manager
               :nyxt/hooks
               :nyxt/keymap)
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
               (:file "command")
               (:file "buffer")
               (:file "window")
               (:file "mode")
               (:file "browser")
               (:file "utility")
               (:file "urls")
               (:file "fuzzy")
               (:file "message")
               (:file "input")
               (:file "minibuffer")
               (:file "minibuffer-mode")
               (:file "parse-urls")
               (:file "recent-buffers")
               (:file "password")
               (:file "bookmark")
               (:file "history")
               (:file "autofill")
               (:file "auto-mode")
               (:file "external-editor")
               (:file "url-group")
               ;; Core Modes
               (:file "base-mode")
               (:file "repl-mode")
               (:file "help-mode")
               (:file "application-mode")
               (:file "web-mode")
               (:file "reading-line-mode")
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
               (:file "force-https-mode")
               ;; Web-mode commands
               (:file "input-edit")
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
  :in-order-to ((test-op (test-op "nyxt/tests"))))

(asdf:defsystem nyxt/tests
  :depends-on (nyxt prove)
  :perform (asdf:test-op (op c)
                         (funcall (read-from-string "prove:run")
                                  (asdf:system-relative-pathname c "tests/"))))

(asdf:defsystem :nyxt/gtk
  :depends-on (:nyxt
               :cl-cffi-gtk
               :cl-webkit2)
  :pathname "source/"
  :components ((:file "renderer-gtk")))

(asdf:defsystem :nyxt/qt
  :depends-on (:nyxt
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

(asdf:defsystem :nyxt/gtk-application
  :depends-on (:nyxt/gtk)
  :build-operation "program-op"
  :build-pathname "nyxt"
  :entry-point "nyxt:entry-point")

(asdf:defsystem :nyxt/build-rpm
  :depends-on (:cffi-toolchain :cl-ppcre :nyxt)
  :pathname "build-scripts/"
  :components ((:file "build-rpm")))

(asdf:defsystem :nyxt/gtk-application-rpm
  :depends-on (:nyxt/build-rpm :nyxt/gtk)
  :build-operation "build-rpm"
  :build-pathname "nyxt"
  :entry-point "nyxt:entry-point")

(asdf:defsystem :nyxt/qt-application
  :depends-on (:nyxt/qt)
  :build-operation "program-op"
  :build-pathname "nyxt-qt"
  :entry-point "nyxt:entry-point")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression (not (null (uiop:getenv "NYXT_COMPRESS")))))

(asdf:defsystem nyxt/download-manager
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
  :in-order-to ((test-op (test-op "nyxt/download-manager/tests"))))

(asdf:defsystem nyxt/download-manager/tests
  :depends-on (nyxt/download-manager prove)
  :perform (asdf:test-op (op c)
                         (funcall (read-from-string "prove:run")
                                  (asdf:system-relative-pathname c "libraries/download-manager/tests/"))))

(asdf:defsystem nyxt/text-analysis
  :depends-on (:str
               :cl-ppcre)
  :pathname "libraries/text-analysis/"
  :components ((:file "package")
               (:file "data")
               (:file "stem")
               (:file "analysis")
               (:file "text-rank")))

(asdf:defsystem nyxt/text-buffer
  :depends-on (:cluffer)
  :pathname "libraries/text-buffer/"
  :components ((:file "package")
               (:file "text-buffer")))

(asdf:defsystem nyxt/history-tree
  :pathname "libraries/history-tree/"
  :components ((:file "package")
               (:file "history-tree"))
  :in-order-to ((test-op (test-op "nyxt/history-tree/tests"))))

(asdf:defsystem nyxt/history-tree/tests
  :depends-on (nyxt/history-tree prove)
  :perform (asdf:test-op (op c)
                         (funcall (read-from-string "prove:run")
                                  (asdf:system-relative-pathname c "libraries/history-tree/tests/"))))

(asdf:defsystem nyxt/password-manager
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

(asdf:defsystem nyxt/hooks
  :depends-on (alexandria serapeum)
  :pathname "libraries/hooks/"
  :components ((:file "package")
               (:file "hooks"))
  :in-order-to ((test-op (test-op "nyxt/hooks/tests"))))

(asdf:defsystem nyxt/hooks/tests
  :depends-on (nyxt/hooks prove)
  :perform (asdf:test-op (op c)
                         (funcall (read-from-string "prove:run")
                                  (asdf:system-relative-pathname c "libraries/hooks/tests/"))))

(asdf:defsystem nyxt/keymap
  :depends-on (alexandria fset str)
  :pathname "libraries/keymap/"
  :components ((:file "package")
                             (:file "types")
                             (:file "conditions")
                             (:file "keymap")
                             (:file "scheme")
                             (:file "scheme-names"))
  :in-order-to ((test-op (test-op "nyxt/keymap/tests"))))

(asdf:defsystem nyxt/keymap/tests
  :depends-on (alexandria fset nyxt/keymap prove)
  :perform (asdf:test-op (op c)
                         (funcall (read-from-string "prove:run")
                                  (asdf:system-relative-pathname c "libraries/keymap/tests/"))))
