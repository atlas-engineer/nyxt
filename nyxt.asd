;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

#+sbcl
(progn
  (sb-ext:assert-version->= 2 0 0)
  (require 'sb-bsd-sockets))

;; REVIEW: Use `*load-pathname*' or `*load-truename*' instead of `*default-pathname-defaults*'?
;; We could also use (asdf:system-source-directory :nyxt)), but only after the definition of the system.
(setf (logical-pathname-translations "NYXT")
      `(("NYXT:source;**;*.fasl.*"
         ,(uiop:ensure-pathname (asdf:apply-output-translations *default-pathname-defaults*)
                                :wilden t))
        ("NYXT:source;**;*.*.*"
         ,(uiop:ensure-pathname (uiop:subpathname* *default-pathname-defaults* "source/") :wilden t))
        ("NYXT:libraries;**;*.fasl.*"
         ,(uiop:ensure-pathname (asdf:apply-output-translations *default-pathname-defaults*)
                                :wilden t))
        ("NYXT:libraries;**;*.*.*"
         ,(uiop:ensure-pathname (uiop:subpathname* *default-pathname-defaults* "libraries/") :wilden t))))

(defun npath (path)
  "If NYXT_LOGICAL_PATH environment variable is set, use logical path source
location, otherwise use the translated path.

Tools such as Emacs (SLIME and SLY) may fail to make use of logical paths, say,
to go to the compilation error location."
  (if (uiop:getenv "NYXT_LOGICAL_PATH")
      (translate-logical-pathname path)
      path))

(defvar *prefix* (merge-pathnames* (if (getenv "PREFIX")
                                       (relativize-pathname-directory (ensure-directory-pathname (getenv "PREFIX")))
                                       #p"usr/local/")
                                   (if (getenv "DESTDIR")
                                       (ensure-directory-pathname (getenv "DESTDIR"))
                                       #p"/")))
(defvar *datadir* (if (getenv "DATADIR")
                      (ensure-directory-pathname (getenv "DATADIR"))
                      (merge-pathnames* "share/" *prefix*)))
(defvar *bindir* (if (getenv "BINDIR")
                     (ensure-directory-pathname (getenv "BINDIR"))
                     (merge-pathnames* "bin/" *prefix*)))
(defvar *dest-source-dir* (if (getenv "NYXT_SOURCE_PATH")
                              (ensure-directory-pathname (getenv "NYXT_SOURCE_PATH"))
                              (merge-pathnames* "nyxt/" *datadir*)))

(defvar *nyxt-renderer* (or (getenv "NYXT_RENDERER")
                            "gi-gtk"))

(defvar *submodules-dir* (or (getenv "NYXT_SUBMODULES_DIR")
                             "_build"))

(defvar *submodules-jobs* (or (getenv "NYXT_SUBMODULES_JOBS")
                              4)
  "Number of parallel 'git clone' jobs to fetch the Git submodules.
A naive benchmark on a 16 Mpbs bandwidth gives us

    1 job:  5m17s
    2 jobs: 3m38s
    4 jobs: 2m51s
    8 jobs: 2m21s")

(defsystem "nyxt"
  :version "3"                          ; Development version.
  :author "Atlas Engineer LLC"
  :homepage "https://nyxt.atlas.engineer"
  :description "Extensible web browser in Common Lisp"
  :license "BSD 3-Clause"
  :depends-on (alexandria
               bordeaux-threads
               calispel
               cl-base64
               cl-css
               cl-gopher
               cl-html-diff
               cl-json
               cl-ppcre
               cl-ppcre-unicode
               cl-prevalence
               cl-qrencode
               cl-tld
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
               nfiles
               #-sbcl
               osicat
               parenscript
               quri
               serapeum
               nhooks
               str
               phos
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
  :pathname #.(npath #p"NYXT:source;")
  :components ((:file "package")
               (:module "Utilities"
                :pathname ""
                :depends-on ("package")
                :components
                ((:file "time")
                 (:file "types")
                 (:file "scheme" :depends-on ("types"))
                 (:file "conditions")
                 (:file "debug")
                 (:file "user-interface")))
               (:module "Core"
                :pathname ""
                :serial t
                :depends-on ("Utilities")
                :components
                ((:file "global")
                 (:file "concurrency")
                 (:file "gpg")
                 (:file "user-files")
                 (:file "user-classes")
                 (:file "configuration")
                 (:file "spinneret-tags")
                 (:file "parenscript-macro")
                 (:file "message")
                 (:file "command")
                 (:file "renderer-script")
                 (:file "urls")
                 (:file "inspector")
                 (:file "dom")
                 (:file "buffer")
                 (:file "window")
                 (:file "mode")
                 (:file "search-engine")
                 (:file "browser")
                 (:file "foreign-interface")
                 (:file "clipboard")
                 (:file "input")
                 (:file "prompt-buffer")
                 (:file "command-commands")
                 (:file "describe")
                 (:file "debugger")
                 (:file "recent-buffers")
                 (:file "history")
                 (:file "external-editor")))
               (:module "Core modes"
                :pathname "mode"
                :depends-on ("Core")
                :components
                ((:file "auto")
                 (:file "input-edit")
                 (:file "prompt-buffer" :depends-on ("input-edit"))
                 (:file "file-manager" :depends-on ("prompt-buffer"))
                 (:file "buffer-listing")
                 (:file "message")
                 (:file "passthrough")
                 (:file "history-tree")
                 (:file "list-history")
                 (:file "document" :depends-on ("passthrough"))
                 (:file "hint" :depends-on ("document" "download"))
                 (:file "hint-prompt-buffer" :depends-on ("hint"))
                 (:file "search-buffer" :depends-on ("hint"))
                 (:file "spell-check" :depends-on ("document"))
                 (:file "help" :depends-on ("document" "search-buffer"))
                 (:file "history" :depends-on ("history-tree" "list-history"))
                 (:file "certificate-exception" :depends-on ("history"))
                 (:file "keymap-scheme")
                 (:file "proxy")
                 (:file "download" :depends-on ("file-manager"))
                 (:file "process")
                 (:file "user-script")))
               (:file "mode/base" :depends-on ("Core modes"))
               #+quicklisp
               (:file "lisp-system" :depends-on ("Core modes"))
               (:file "status" :depends-on ("Core"))
               (:file "start" :depends-on ("Core"))
               (:module "Editor modes"
                :pathname "mode"
                :depends-on ("Core modes")
                :components
                ((:file "editor")
                 (:file "plaintext-editor")))
               (:module "Help"
                :pathname ""
                :depends-on ("Core modes" "Modes")
                :components
                ((:file "help")
                 (:file "about")
                 (:file "tutorial")
                 (:file "changelog")
                 (:file "migration")
                 (:file "manual")))
               (:module "Modes"
                :pathname "mode"
                :depends-on ("Core modes")
                :components
                ((:file "annotate")
                 (:file "autofill")
                 (:file "blocker")
                 (:file "bookmark")
                 (:file "bookmark-frequent-visits" :depends-on ("bookmark"))
                 (:file "bookmarklets")
                 (:file "cruise-control" :depends-on ("repeat"))
                 (:file "diff")
                 (:file "emacs")
                 (:file "expedition")
                 (:file "force-https")
                 (:file "macro-edit")
                 (:file "no-image")
                 (:file "no-procrastinate" :depends-on ("blocker"))
                 (:file "no-script")
                 (:file "no-sound")
                 (:file "no-webgl")
                 (:file "os-package-manager")
                 (:file "password")
                 (:file "preview")
                 (:file "reading-line")
                 (:file "record-input-field")
                 (:file "reduce-bandwidth")
                 (:file "reduce-tracking")
                 (:file "repeat")
                 (:file "repl")
                 (:file "small-web")
                 (:file "style" :depends-on ("bookmarklets"))
                 (:file "tts")
                 (:file "vi")
                 (:file "visual")
                 (:file "watch"))))
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
           (if (absolute-pathname-p path)
               path
               (system-relative-pathname component path))))
    (setf (getenv "CL_SOURCE_REGISTRY")
          (strcat
           (native-namestring
            (ensure-directory-pathname
             (ensure-absolute-path *submodules-dir* component)))
           ;; Double-slash tells ASDF to traverse the tree recursively.
           "/"
           ;; Register this directory so that nyxt.asd is included, just in case.
           (inter-directory-separator)
           (native-namestring (system-source-directory component))
           (if (getenv "CL_SOURCE_REGISTRY")
               (strcat (inter-directory-separator) (getenv "CL_SOURCE_REGISTRY"))
               ;; End with an empty string to tell ASDF to inherit configuration.
               (inter-directory-separator)))))
  (clear-configuration)
  (format t "; CL_SOURCE_REGISTRY: ~s~%" (getenv "CL_SOURCE_REGISTRY")))

(defsystem "nyxt/submodules"
  :perform (compile-op (o c)
                       (run-program `("git"
                                      "-C" ,(namestring (system-relative-pathname c ""))
                                      "submodule" "update" "--init" "--force"
                                      "--jobs" ,(write-to-string *submodules-jobs*))
                                    :ignore-error-status t
                                    :output t)
                       (register-submodules c)))

(defun nyxt-run-test (c path &key network-needed-p)
  (and (or (not network-needed-p)
           (not (getenv "NYXT_TESTS_NO_NETWORK")))
       (not (funcall (read-from-string "prove:run")
                     (system-relative-pathname c path)))
       (getenv "NYXT_TESTS_ERROR_ON_FAIL")
       (quit 18)))

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
               cl-webkit2)
  :pathname #.(npath #p"NYXT:source;")
  :serial t
  :components ((:file "web-extensions")
               (:file "web-extensions-callbacks")
               (:file "renderer/gtk-clipboard")
               (:file "renderer/gtk")))

(defsystem "nyxt/gi-gtk"
  :depends-on (nyxt/gtk
               cl-gobject-introspection
               bordeaux-threads)
  :pathname #.(npath #p"NYXT:source;")
  :components ((:file "renderer/gi-gtk"))
  :in-order-to ((test-op (test-op "nyxt/gi-gtk/tests"))))

(defsystem "nyxt/gi-gtk/tests"
  :depends-on (nyxt/gi-gtk prove)
  :components ((:file "tests/renderer-package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "tests/renderer-offline/")
                    (nyxt-run-test c "tests/renderer-online/" :network-needed-p t)))

(defsystem "nyxt/qt"
  :depends-on (nyxt
               cl-webengine
               trivial-main-thread)
  :pathname #.(npath #p"NYXT:source;")
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

(defsystem "nyxt/application/tests"
  :depends-on (#.(format nil "nyxt/~a-application" *nyxt-renderer*)
                 prove)
  :components ((:file "tests/package"))
  :perform (test-op (op c)
                    (if (file-exists-p (system-relative-pathname :nyxt "nyxt"))
                        (nyxt-run-test c "tests/executable/")
                        (warn "`nyxt' executable missing, skipping tests."))))

#+sb-core-compression
(handler-bind ((warning #'muffle-warning))
  (defmethod perform ((o image-op) (c system))
    (dump-image (output-file o c)
                :executable t
                :compression (when (getenv "NYXT_COMPRESS")
                               (parse-integer (getenv "NYXT_COMPRESS"))))))

(defmethod perform :before ((o image-op) (c system))
  "Perform some last minute tweaks to the final image.

- Register immutable systems to prevent compiled images of Nyxt from
trying to recompile dependencies.
See `asdf::*immutable-systems*'.

- If on SBCL, include `sb-sprof', the statistical profiler, since it's one of
the few modules that's not automatically included in the image."
  #+sbcl
  (require :sb-sprof)
  (map () 'register-immutable-system
       (remove-if (lambda (system) (uiop:string-prefix-p "nyxt" system))
                  (asdf:already-loaded-systems))))

(defsystem "nyxt/install"
  :depends-on (alexandria
               str
               #.(format nil "nyxt/~a-application" *nyxt-renderer*)
               nyxt/version)
  :perform (compile-op
            (o c)
            (flet ((ensure-parent-exists (file)
                     (ensure-all-directories-exist
                      (list (directory-namestring file)))))
              (let ((desktop-file (merge-pathnames* "applications/nyxt.desktop" *datadir*)))
                (ensure-parent-exists desktop-file)
                (copy-file (system-relative-pathname c "assets/nyxt.desktop")
                                desktop-file))
              (mapc (lambda (icon-size)
                      (let ((icon-file (format nil "~a/icons/hicolor/~ax~a/apps/nyxt.png"
                                               *datadir* icon-size icon-size)))
                        (ensure-parent-exists icon-file)
                        (copy-file (system-relative-pathname
                                         c
                                         (format nil "assets/nyxt_~ax~a.png"
                                                 icon-size icon-size))
                                        icon-file)))
                    '(16 32 128 256 512))
              (let ((binary-file (merge-pathnames* "nyxt" *bindir*)))
                (ensure-parent-exists binary-file)
                (copy-file (system-relative-pathname c "nyxt") binary-file)
                ;; TODO: Use iolib/os:file-permissions instead of chmod?  Too verbose?
                (run-program (list "chmod" "+x" (native-namestring binary-file))))
              (flet ((copy-directory (source destination &key verbose-p)
                       "Copy the content (the file tree) of SOURCE to DESTINATION."
                       (when verbose-p
                         (format *error-output* "~&;; Copy ~s/* inside ~s.~%" source destination))
                       (collect-sub*directories
                        (ensure-directory-pathname source)
                        (constantly t)
                        t
                        (lambda (subdirectory)
                          (mapc (lambda (file)
                                  (unless (member (pathname-type file) '("o" "fasl") :test 'equalp)
                                    (let ((destination-file
                                            (merge-pathnames*
                                             (subpathp file (ensure-directory-pathname source))
                                             (ensure-pathname destination :truenamize t :ensure-directory t))))
                                      (ensure-parent-exists destination-file)
                                      (copy-file file destination-file))))
                                (directory-files subdirectory))))))
                (handler-case
                    (progn
                      (format *error-output* "~&;; Copying Git files to ~s.~%" *dest-source-dir*)
                      (dolist (file (cons
                                     ;; Find `libnyxt' file regardless of its extension:
                                     (subpathp (first (delete-if
                                                       (complement (lambda (file)
                                                                     (string-prefix-p "libnyxt" (pathname-name file))))
                                                       (directory-files (system-relative-pathname
                                                                         :nyxt "libraries/web-extensions"))))
                                               (system-source-directory :nyxt))
                                     (mapcan (lambda (dir)
                                               (split-string
                                                (run-program `("git" "-C" ,(native-namestring
                                                                            (system-source-directory :nyxt))
                                                                     "ls-files" ,dir)
                                                             :output '(:string :stripped t))
                                                :separator '(#\newline #\return #\linefeed)))
                                             '("source" "libraries"))))
                        (let ((dest (merge-pathnames* file *dest-source-dir*)))
                          (ensure-parent-exists dest)
                          (copy-file (system-relative-pathname :nyxt file)
                                     dest))))
                  (condition (c)
                    (format *error-output* "~&;; Git copy error: ~a" c)
                    (format *error-output* "~&;; Fallback to copying whole directories instead.~%")
                    (dolist (dir '("source" "libraries"))
                      (copy-directory (system-relative-pathname :nyxt dir)
                                      (merge-pathnames* dir *dest-source-dir*)
                                      :verbose-p t))))
                (copy-file (system-source-file :nyxt) (merge-pathnames* "nyxt.asd" *dest-source-dir*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library subsystems:

(defsystem "nyxt/download-manager"
  :depends-on (calispel
               cl-ppcre
               dexador
               log4cl
               quri
               str)
  :pathname #.(npath #p"NYXT:libraries;download-manager;")
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
  :pathname #.(npath #p"NYXT:libraries;analysis;")
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
  :pathname #.(npath #p"NYXT:libraries;user-interface;")
  :components ((:file "package")
               (:file "user-interface")))

(defsystem "nyxt/text-buffer"
  :depends-on (cluffer)
  :pathname #.(npath #p"NYXT:libraries;text-buffer;")
  :components ((:file "package")
               (:file "text-buffer")))

(defsystem "nyxt/history-tree"
  :depends-on (alexandria
               cl-custom-hash-table
               local-time
               nyxt/class-star
               trivial-package-local-nicknames)
  :pathname #.(npath #p"NYXT:libraries;history-tree;")
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
  :pathname #.(npath #p"NYXT:libraries;password-manager;")
  :components ((:file "package")
               (:file "password")
               (:file "password-keepassxc")
               (:file "password-security")
               ;; Keep password-store last so that it has higher priority.
               (:file "password-pass")))

(defsystem "nyxt/keymap"
  :depends-on (alexandria fset str)
  :pathname #.(npath #p"NYXT:libraries;keymap;")
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
  :pathname #.(npath #p"NYXT:libraries;class-star;")
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
  :pathname #.(npath #p"NYXT:libraries;ospm;")
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
               moptilities
               serapeum
               str
               trivial-package-local-nicknames
               nyxt/class-star)
  :pathname #.(npath #p"NYXT:libraries;prompter;")
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
  :pathname #.(npath #p"NYXT:libraries;theme;")
  :components ((:file "package")
               (:file "theme"))
  :in-order-to ((test-op (test-op "nyxt/theme/tests"))))

(defsystem "nyxt/theme/tests"
  :depends-on (nyxt/theme prove)
  :components ((:file "libraries/theme/test-package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/theme/tests/")))
