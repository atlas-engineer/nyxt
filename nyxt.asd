;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

#+sbcl
(progn
  (sb-ext:assert-version->= 2 0 0)
  (require 'sb-bsd-sockets))

(defvar *prefix* (format nil "~a/~a"
                         (or (uiop:getenv "DESTDIR") "")
                         (or (uiop:getenv "PREFIX")
                             "/usr/local")))
(defvar *datadir* (or (uiop:getenv "DATADIR")
                      (format nil "~a/share" *prefix*)))
(defvar *bindir* (or (uiop:getenv "BINDIR")
                     (format nil "~a/bin" *prefix*)))

(defvar *nyxt-renderer* (or (uiop:getenv "NYXT_RENDERER")
                            "gi-gtk"))

(defvar *submodules-dir* (or (uiop:getenv "NYXT_SUBMODULES_DIR")
                             "_build"))

(defvar *submodules-jobs* (or (uiop:getenv "NYXT_SUBMODULES_JOBS")
                              4)
  "Number of parallel 'git clone' jobs to fetch the Git submodules.
A naive benchmark on a 16 Mpbs bandwidth gives us

    1 job:  5m17s
    2 jobs: 3m38s
    4 jobs: 2m51s
    8 jobs: 2m21s")

(defsystem "nyxt"
  :version "2.2.2"
  :author "Atlas Engineer LLC"
  :homepage "https://nyxt.atlas.engineer"
  :description "Extensible web-browser in Common Lisp"
  :license "BSD 3-Clause"
  :serial t
  :depends-on (alexandria
               bordeaux-threads
               calispel
               cl-base64
               cl-css
               cl-html-diff
               cl-json
               cl-ppcre
               cl-ppcre-unicode
               cl-prevalence
               cl-qrencode
               closer-mop
               cl-containers
               moptilities
               dexador
               enchant
               flexi-streams
               iolib
               iolib/os
               local-time
               lparallel
               log4cl
               mk-string-metrics
               #-sbcl
               osicat
               parenscript
               quri
               serapeum
               str
               plump
               clss
               spinneret
               swank
               trivia
               trivial-clipboard
               trivial-features
               trivial-garbage
               trivial-package-local-nicknames
               trivial-types
               unix-opts
               ;; Local systems:
               nyxt/user-interface
               nyxt/text-buffer
               nyxt/analysis
               nyxt/download-manager
               nyxt/history-tree
               nyxt/password-manager
               nyxt/keymap
               nyxt/class-star
               nyxt/ospm
               nyxt/prompter
               nyxt/theme)
  :pathname "source/"
  :components ((:file "package")
               ;; Independent utilities
               (:file "time")
               (:file "types")
               (:file "conditions")
               (:file "debug")
               (:file "user-interface")
               ;; Core functionality
               (:file "global")
               (:file "concurrency")
               (:file "data-storage")
               (:file "configuration")
               (:file "message")
               (:file "command")
               (:file "parenscript-macro")
               (:file "renderer-script")
               (:file "urls")
               (:file "dom")
               (:file "buffer")
               (:file "window")
               (:file "mode")
               (:file "search-engine")
               (:file "browser")
               (:file "clipboard")
               (:file "input")
               (:file "prompt-buffer")
               (:file "command-commands")
               (:file "recent-buffers")
               (:file "password")
               (:file "bookmark")
               (:file "annotate")
               (:file "history")
               (:file "autofill")
               (:file "external-editor")
               (:file "file-manager")
               #+quicklisp
               (:file "lisp-system")
               ;; Core Modes
               (:file "mode/auto")
               (:file "mode/input-edit")
               (:file "mode/prompt-buffer")
               (:file "mode/editor")
               (:file "mode/plaintext-editor")
               (:file "mode/buffer-listing")
               (:file "mode/base")
               (:file "mode/repl")
               (:file "mode/help")
               (:file "mode/message")
               (:file "mode/passthrough")
               (:file "mode/history-tree")
               (:file "mode/list-history")
               (:file "mode/bookmark-frequent-visits")
               (:file "mode/web")
               (:file "mode/reading-line")
               (:file "mode/style")
               (:file "mode/certificate-exception")
               (:file "mode/emacs")
               (:file "mode/vi")
               (:file "mode/blocker")
               (:file "mode/proxy")
               (:file "mode/no-image")
               (:file "mode/no-sound")
               (:file "mode/no-script")
               (:file "mode/no-webgl")
               (:file "mode/reduce-bandwidth")
               (:file "mode/download")
               (:file "mode/force-https")
               (:file "mode/reduce-tracking")
               (:file "mode/os-package-manager")
               (:file "mode/visual")
               (:file "mode/process")
               (:file "mode/repeat")
               (:file "mode/cruise-control")
               (:file "mode/watch")
               (:file "mode/preview")
               (:file "mode/diff")
               (:file "mode/expedition")
               (:file "mode/tts")
               (:file "mode/bookmarklets")
               (:file "mode/element-hint")
               (:file "mode/element-frame")
               (:file "mode/jump-heading")
               (:file "mode/summarize")
               (:file "mode/scroll")
               (:file "mode/search-buffer")
               (:file "mode/spell-check")
               (:file "mode/zoom")
               (:file "mode/record-input-field")
               (:file "mode/macro-edit")
               ;; Needs web-mode
               (:file "help")
               (:file "status")
               ;; Depends on everything else:
               (:file "about")
               (:file "start")
               (:file "tutorial")
               (:file "changelog")
               (:file "manual"))
  :in-order-to ((test-op (test-op "nyxt/tests")
                         (test-op "nyxt/download-manager/tests")
                         (test-op "nyxt/history-tree/tests")
                         (test-op "nyxt/keymap/tests")
                         (test-op "nyxt/class-star/tests")
                         (test-op "nyxt/ospm/tests")
                         (test-op "nyxt/prompter/tests"))))

(defun register-submodules (component)
  ;; Ideally we should avoid writing global, stateful files to the user file
  ;; system.  So instead of writing to the ASDF config file, we register the
  ;; sudmodule directory with CL_SOURCE_REGISTRY.  This locally overrides
  ;; CL_SOURCE_REGISTRY, but it's fine since submodules are only meant for
  ;; non-developers (who probably don't set CL_SOURCE_REGISTRY).
  ;;
  ;; We must set this globally because the information would be lost within a
  ;; Lisp compiler subprocess (e.g. as used by linux-packaging).
  (flet ((ensure-absolute-path (path component)
           (if (uiop:absolute-pathname-p path)
               path
               (system-relative-pathname component path))))
    (setf (uiop:getenv "CL_SOURCE_REGISTRY")
          (uiop:strcat
           (namestring
            (uiop:truenamize
             (uiop:ensure-directory-pathname
              (ensure-absolute-path *submodules-dir* component))))
           ;; Double-slash tells ASDF to traverse the tree recursively.
           "/"
           ;; Register this directory so that nyxt.asd is included, just in case.
           (uiop:inter-directory-separator)
           (namestring (uiop:truenamize (uiop:pathname-directory-pathname
                                         (asdf:system-source-file component))))
           (if (uiop:getenv "CL_SOURCE_REGISTRY")
               (uiop:strcat (uiop:inter-directory-separator) (uiop:getenv "CL_SOURCE_REGISTRY"))
               ;; End with an empty string to tell ASDF to inherit configuration.
               (uiop:inter-directory-separator)))))
  (asdf:clear-configuration)
  (format t "; CL_SOURCE_REGISTRY: ~s~%" (uiop:getenv "CL_SOURCE_REGISTRY")))

(defsystem "nyxt/submodules"
  :perform (compile-op (o c)
                       (uiop:run-program `("git"
                                           "-C" ,(namestring (system-relative-pathname c ""))
                                           "submodule" "update" "--init" "--force"
                                           "--jobs" ,(write-to-string *submodules-jobs*))
                                         :ignore-error-status t
                                         :output t)
                       (register-submodules c)))

(defun nyxt-run-test (c path &key network-needed-p)
  (and (or (not network-needed-p)
           (not (uiop:getenv "NYXT_TESTS_NO_NETWORK")))
       (not (funcall (read-from-string "prove:run")
                     (system-relative-pathname c path)))
       (uiop:getenv "NYXT_TESTS_ERROR_ON_FAIL")
       (uiop:quit 18)))

;; TODO: Test that Nyxt starts and that --help, --version work.
(defsystem "nyxt/tests"
  :depends-on (nyxt prove)
  :components ((:file "tests/package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "tests/offline/")
                    (nyxt-run-test c "tests/online/" :network-needed-p t)))

(defun print-benchmark (benchmark-results)
  (labels ((rat->float (num)
             (if (integerp num) num (float num)))
           (print-times (entry)
             (let ((title (first entry))
                   (attr (rest entry)))
               (unless (or (member (symbol-name title) '("RUN-TIME" "SYSTEM-RUN-TIME")) ; Not so interesting.
                           (and (member (symbol-name title) '("PAGE-FAULTS" "EVAL-CALLS")
                                        :test #'string=)
                                (zerop (getf attr :average))))
                 (format t " ~a: ~,9t~a" (string-downcase title) (rat->float (getf attr :average)))
                 (format t "~32,8t[~a, ~a]"
                         (rat->float (getf attr :minimum))
                         (rat->float (getf attr :maximum)))
                 (format t "~56,8t(median ~a, deviation ~a, total ~a)"
                         (rat->float (getf attr :median))
                         (rat->float (getf attr :deviation))
                         (rat->float (getf attr :total)))
                 (format t "~%")))))
    (dolist (mark benchmark-results)
      (format t "~a (~a sample~:p):~%" (first mark)
              (getf (rest (second mark)) :samples))
      (mapc #'print-times (rest mark)))))

(defsystem "nyxt/benchmark"
  :depends-on (alexandria
               nyxt
               trivial-benchmark)
  :pathname "tests/benchmarks"
  :components ((:file "../benchmark-package")
               (:file "prompter"))
  :perform (test-op (op c)
                    (let ((results
                            (funcall (read-from-string "alexandria:hash-table-alist")
                                     (funcall (read-from-string "benchmark:run-package-benchmarks")
                                              :package :nyxt/benchmark
                                              :verbose t))))
                      (print-benchmark results))))

(defsystem "nyxt/clean-fasls"
  :depends-on (swank)
  :perform (compile-op (o c)
                       (load (merge-pathnames
                              "contrib/swank-asdf.lisp"
                              (symbol-value
                               (read-from-string "swank-loader:*source-directory*"))))
                       (funcall (read-from-string "swank:delete-system-fasls") "nyxt")))

;; We use a temporary "version" file to generate the final nyxt.desktop with the
;; right version number.  Since "version" is a file target, third-party
;; packaging systems can choose to generate "version" in advance before calling
;; "make install-assets", so that they won't need to rely on submodules.
(defsystem "nyxt/version"
  :depends-on (nyxt)
  :output-files (compile-op (o c)
                            (values (list (system-relative-pathname c "version"))
                                    t))
  :perform (compile-op (o c)
                       (with-open-file (out (output-file o c)
                                            :direction :output
                                            :if-exists :supersede)
                         (princ (symbol-value (read-from-string "nyxt:+version+"))
                                out))))

(defsystem "nyxt/documentation"         ; TODO: Only rebuild if input changed.
  :depends-on (nyxt)
  :output-files (compile-op (o c)
                            (values (list (system-relative-pathname c "manual.html"))
                                    t))
  :perform (compile-op (o c)
                       (with-open-file (out (output-file o c)
                                            :direction :output
                                            :if-exists :supersede)
                         (write-string (funcall (read-from-string "nyxt::manual-content"
                                                                  (find-package 'nyxt)))
                                       out))
                       (format *error-output* "Manual dumped to ~s.~&" (output-file o c))))

(defsystem "nyxt/gtk"
  :depends-on (nyxt
               cl-cffi-gtk
               cl-webkit2
               nyxt/web-extensions)
  :pathname "source/"
  :serial t
  :components ((:file "web-extensions")
               (:file "web-extensions-callbacks")
               (:file "renderer/gtk-clipboard")
               (:file "renderer/gtk")))

(defsystem "nyxt/gi-gtk"
  :depends-on (nyxt/gtk
               cl-gobject-introspection
               bordeaux-threads)
  :pathname "source/"
  :components ((:file "renderer/gi-gtk")))

(defsystem "nyxt/qt"
  :depends-on (nyxt
               cl-webengine
               trivial-main-thread)
  :pathname "source/"
  :components ((:file "renderer/qt")))

;; We should not set the build-pathname in systems that have a component.
;; Indeed, when an external program (like Guix) builds components, it needs to
;; know the name of the output.  But ASDF/SYSTEM::COMPONENT-BUILD-PATHNAME is
;; non-exported so the only reliable way to know the build pathname is to use
;; the default.
;;
;; The workaround is to set a new dummy system of which the sole purpose is to
;; produce the desired binary.

(defsystem "nyxt/gtk-application"
  :depends-on (nyxt/gtk)
  :build-operation "program-op"
  :build-pathname "nyxt"
  :entry-point "nyxt:entry-point")

(defsystem "nyxt/gi-gtk-application"
  :depends-on (nyxt/gi-gtk)
  :build-operation "program-op"
  :build-pathname "nyxt"
  :entry-point "nyxt:entry-point")

(defsystem "nyxt/qt-application"
  :depends-on (nyxt/qt)
  :build-operation "program-op"
  :build-pathname "nyxt-qt"
  :entry-point "nyxt:entry-point")

#+sb-core-compression
(handler-bind ((warning #'muffle-warning))
  (defmethod perform ((o image-op) (c system))
    (uiop:dump-image (output-file o c)
                     :executable t
                     :compression (uiop:getenv "NYXT_COMPRESS"))))

(defmethod perform :before ((o image-op) (c system))
  "Register immutable systems to prevent compiled images of Nyxt from
trying to recompile dependencies.
See `asdf::*immutable-systems*'."
  (map () 'asdf:register-immutable-system (asdf:already-loaded-systems)))

(defsystem "nyxt/install"
  :depends-on (alexandria
               str
               #.(format nil "nyxt/~a-application" *nyxt-renderer*)
               nyxt/version)
  :perform (compile-op
            (o c)
            (flet ((ensure-parent-exists (file)
                     (uiop:ensure-all-directories-exist
                      (list (directory-namestring file)))))
              (let ((desktop-file (format nil "~a/applications/nyxt.desktop" *datadir*)))
                (ensure-parent-exists desktop-file)
                (uiop:copy-file (system-relative-pathname c "assets/nyxt.desktop")
                                (format nil "~a/applications/nyxt.desktop" *datadir*)))
              (mapc (lambda (icon-size)
                      (let ((icon-file (format nil "~a/icons/hicolor/~ax~a/apps/nyxt.png"
                                               *datadir* icon-size icon-size)))
                        (ensure-parent-exists icon-file)
                        (uiop:copy-file (system-relative-pathname
                                         c
                                         (format nil "assets/nyxt_~ax~a.png"
                                                 icon-size icon-size))
                                        icon-file)))
                    '(16 32 128 256 512))
              (let ((binary-file (format nil "~a/nyxt" *bindir*)))
                (ensure-parent-exists binary-file)
                (uiop:copy-file (system-relative-pathname c "nyxt") binary-file)
                ;; TODO: Use iolib/os:file-permissions instead of chmod?  Too verbose?
                (uiop:run-program (list "chmod" "+x" binary-file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library subsystems:

(defsystem "nyxt/download-manager"
  :depends-on (calispel
               cl-ppcre
               dexador
               log4cl
               quri
               str)
  :pathname "libraries/download-manager/"
  :components ((:file "package")
               (:file "engine")
               (:file "native"))
  :in-order-to ((test-op (test-op "nyxt/download-manager/tests"))))

(defsystem "nyxt/download-manager/tests"
  :depends-on (nyxt/download-manager prove)
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/download-manager/tests/"
                                   :network-needed-p t)))

(defsystem "nyxt/analysis"
  :depends-on (str
               serapeum
               alexandria
               cl-ppcre)
  :pathname "libraries/analysis/"
  :components ((:file "package")
               (:file "data")
               (:file "stem")
               (:file "tokenize")
               (:file "analysis")
               (:file "document-vector")
               (:file "text-rank")
               (:file "dbscan")
               (:file "section")))

(defsystem "nyxt/user-interface"
  :depends-on (spinneret)
  :pathname "libraries/user-interface/"
  :components ((:file "package")
               (:file "user-interface")))

(defsystem "nyxt/text-buffer"
  :depends-on (cluffer)
  :pathname "libraries/text-buffer/"
  :components ((:file "package")
               (:file "text-buffer")))

(defsystem "nyxt/history-tree"
  :depends-on (alexandria
               cl-custom-hash-table
               local-time
               nyxt/class-star
               trivial-package-local-nicknames)
  :pathname "libraries/history-tree/"
  :components ((:file "package")
               (:file "history-tree"))
  :in-order-to ((test-op (test-op "nyxt/history-tree/tests"))))

(defsystem "nyxt/history-tree/tests"
  :depends-on (nyxt/history-tree prove str)
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/history-tree/tests/")))

(defsystem "nyxt/password-manager"
  :depends-on (bordeaux-threads
               cl-ppcre
               str
               trivial-clipboard
               uiop
               nyxt/class-star
               serapeum)
  :pathname "libraries/password-manager/"
  :components ((:file "package")
               (:file "password")
               (:file "password-keepassxc")
               (:file "password-security")
               ;; Keep password-store last so that it has higher priority.
               (:file "password-pass")))

(defsystem "nyxt/keymap"
  :depends-on (alexandria fset str)
  :pathname "libraries/keymap/"
  :components ((:file "package")
               (:file "types")
               (:file "conditions")
               (:file "keymap")
               (:file "scheme")
               (:file "scheme-names"))
  :in-order-to ((test-op (test-op "nyxt/keymap/tests"))))

(defsystem "nyxt/keymap/tests"
  :depends-on (alexandria fset nyxt/keymap prove)
  :components ((:file "libraries/keymap/test-package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/keymap/tests/")))

(defsystem "nyxt/class-star"
  :depends-on (hu.dwim.defclass-star moptilities alexandria)
  :pathname "libraries/class-star/"
  :components ((:file "package")
               (:file "patch")
               (:file "class-star"))
  :in-order-to ((test-op (test-op "nyxt/class-star/tests"))))

(defsystem "nyxt/class-star/tests"
  :depends-on (nyxt/class-star prove)
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/class-star/tests/")))

(defsystem "nyxt/ospm"
  :depends-on (alexandria
               calispel
               cl-ppcre
               local-time
               named-readtables
               #-sbcl
               osicat
               serapeum
               str
               trivia
               nyxt/class-star)
  :pathname "libraries/ospm/"
  :components ((:file "package")
               (:file "scheme-syntax")
               (:file "guix-backend")
               (:file "ospm")
               (:file "ospm-guix"))
  :in-order-to ((test-op (test-op "nyxt/ospm/tests"))))

(defsystem "nyxt/ospm/tests"
  :depends-on (nyxt/ospm prove)
  :components ((:file "libraries/ospm/test-package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/ospm/tests/tests.lisp")))

(defsystem "nyxt/prompter"
  :depends-on (alexandria
               calispel
               cl-containers
               closer-mop
               lparallel
               mk-string-metrics
               moptilities
               serapeum
               str
               trivial-package-local-nicknames
               nyxt/class-star)
  :pathname "libraries/prompter/"
  :components ((:file "package")
               (:file "filter-preprocessor")
               (:file "filter")
               (:file "prompter-source")
               (:file "prompter"))
  :in-order-to ((test-op (test-op "nyxt/prompter/tests"))))

(defsystem "nyxt/prompter/tests"
  :depends-on (nyxt/prompter prove)
  :components ((:file "libraries/prompter/test-package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/prompter/tests/")))

(defsystem "nyxt/theme"
  :depends-on (alexandria
               serapeum
               nyxt/class-star
               cl-css)
  :pathname "libraries/theme/"
  :components ((:file "package")
               (:file "theme"))
  :in-order-to ((test-op (test-op "nyxt/theme/tests"))))

(defsystem "nyxt/theme/tests"
  :depends-on (nyxt/theme prove)
  :components ((:file "libraries/theme/test-package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/theme/tests/")))

(defsystem "nyxt/web-extensions"
  :pathname "libraries/web-extensions/"
  :depends-on (:cffi-toolchain)
  :components ((:static-file "alarms.c")
               (:static-file "bookmarks.c")
               (:static-file "browser.c")
               (:static-file "browser_action.c")
               (:static-file "commands.c")
               (:static-file "extension.c")
               (:static-file "extevent.c")
               (:static-file "globals.c")
               (:static-file "history.c")
               (:static-file "management.c")
               (:static-file "notifications.c")
               (:static-file "nyxt.c")
               (:static-file "permissions.c")
               (:static-file "runtime.c")
               (:static-file "storage.c")
               (:static-file "tabs.c")
               (:static-file "web_navigation.c")
               (:static-file "web_request.c"))
  :output-files (compile-op (o c)
                            (values (list (uiop:merge-pathnames* "libnyxt.so" (asdf:component-pathname c)))
                                    t))
  :perform (compile-op
            (o c)
            (let ((c-compiler (symbol-value
                               (uiop:find-symbol* :*cc* :cffi-toolchain)))
                  (c-flags (uiop:split-string
                            (uiop:run-program
                             '("pkg-config" "gobject-2.0" "webkit2gtk-web-extension-4.0" "--cflags")
                             :output '(:string :stripped t)
                             :error-output :output)))
                  (ld-flags (uiop:split-string
                             (uiop:run-program
                              '("pkg-config" "gobject-2.0" "webkit2gtk-web-extension-4.0" "--libs")
                              :output '(:string :stripped t)
                              :error-output :output))))
              (uiop:with-current-directory ((component-pathname c))
                (mapc (lambda (c-component)
                        ;; TODO: Allow compiler customization?
                        (uiop:run-program `(,c-compiler "-c" ,(uiop:native-namestring (component-pathname c-component))
                                                        ,@c-flags "-fPIC")
                                          :output t
                                          :error-output :output))
                      (module-components c))
                ;; TODO: Allow linker customization.
                (uiop:run-program `(,c-compiler ,@ld-flags "-fPIC" "-shared" "-o" "libnyxt.so"
                                                ,@(mapcar #'uiop:native-namestring
                                                          (uiop:directory-files (component-pathname c) "*.o"))))))))
