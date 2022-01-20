;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=build-scripts/nyxt.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell --container -D -f build-scripts/nyxt.scm glib glib-networking gsettings-desktop-schemas
;;
;; Replace --container by --pure if you still want ASDF to see external
;; libraries in ~/common-lisp, etc.
;; To build a local executable and then run it:
;;
;;   guix shell --container -D -f build-scripts/nyxt.scm -- make all NYXT_SUBMODULES=false
;;   guix shell --pure -D -f build-scripts/nyxt.scm -- ./nyxt
;;
;; To start in a container, run:
;;
;;   guix shell -f build-scripts/nyxt.scm --container --network --share=/PATH/TO/YOUR/NYXT/CHECKOUT=/nyxt --preserve='^DISPLAY$' --expose=/etc/ssl/certs --ad-hoc nss-certs glib glib-networking gsettings-desktop-schemas
;;
;; Replace '/PATH/TO/YOUR/NYXT/CHECKOUT' as appropriate.
;; Then in the container environment:
;;
;;   cd /nyxt
;;   make all NYXT_SUBMODULES=false
;;   ./nyxt
;;
;;; Code:

(define-module (nyxt)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((guix build utils) #:select (with-directory-excursion))
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf) ; TODO: Remove sbcl-cl-webkit once Guix has 3.5.0.
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages webkit))

;; TODO: Remove sbcl-cl-webkit once Guix has 3.5.0.
(define-public sbcl-cl-webkit
  (package
    (name "sbcl-cl-webkit")
    (version "3.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joachifm/cl-webkit")
             (commit version)))
       (file-name (git-file-name "cl-webkit" version))
       (sha256
        (base32
         "1a16dka15lqzpli0f0qd3afmi14vgdxnfkn9z9d1r4cw9p11s71l"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("cffi" ,sbcl-cffi)
       ("cl-cffi-gtk" ,sbcl-cl-cffi-gtk)
       ("webkitgtk" ,webkitgtk)))
    (native-inputs
     `(("calispel" ,sbcl-calispel)
       ("fiveam" ,sbcl-fiveam)
       ("float-features" ,sbcl-float-features)
       ;; Tests seem to need Xorg.
       ;; ("xorg-server" ,xorg-server-for-tests)
       ))
    (arguments
     `(#:asd-systems '("cl-webkit2")
       #:tests? #f                      ; TODO: Tests hang, why?
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "webkit2/webkit2.init.lisp"
               (("libwebkit2gtk" all)
                (string-append
                 (assoc-ref inputs "webkitgtk") "/lib/" all)))))
         ;; (add-before 'check 'start-xorg-server
         ;;   (lambda* (#:key inputs #:allow-other-keys)
         ;;     ;; The test suite requires a running X server.
         ;;     (system (string-append (assoc-ref inputs "xorg-server")
         ;;                            "/bin/Xvfb :1 &"))
         ;;     (setenv "DISPLAY" ":1")
         ;;     #t))
         )))
    (home-page "https://github.com/joachifm/cl-webkit")
    (synopsis "Binding to WebKitGTK+ for Common Lisp")
    (description
     "@command{cl-webkit} is a binding to WebKitGTK+ for Common Lisp,
currently targeting WebKit version 2.  The WebKitGTK+ library adds web
browsing capabilities to an application, leveraging the full power of the
WebKit browsing engine.")
    (license license:expat)))

(define-public cl-webkit
  (sbcl-package->cl-source-package sbcl-cl-webkit))

(define %source-dir (dirname (dirname (current-filename))))

(define git-file?
  (let* ((pipe (with-directory-excursion %source-dir
                 (open-pipe* OPEN_READ "git" "ls-files")))
         (files (let loop ((lines '()))
                  (match (read-line pipe)
                    ((? eof-object?)
                     (reverse lines))
                    (line
                     (loop (cons line lines))))))
         (status (close-pipe pipe)))
    (lambda (file stat)
      (match (stat:type stat)
        ('directory
         #t)
        ((or 'regular 'symlink)
         (any (cut string-suffix? <> file) files))
        (_
         #f)))))

(define (nyxt-git-version)              ; Like Nyxt's `+version+'.
  (let* ((pipe (with-directory-excursion %source-dir
                 (open-pipe* OPEN_READ "git" "describe" "--always" "--tags")))
         (version (read-line pipe)))
    (close-pipe pipe)
    version))

(define-public nyxt
  (package
    (name "nyxt")
    (version (nyxt-git-version))
    (source (local-file %source-dir #:recursive? #t #:select? git-file?))
    (build-system gnu-build-system)     ; TODO: Use glib-or-gtk-build-system instead?
    (arguments
     `(#:make-flags (list "nyxt" "NYXT_SUBMODULES=false"
                          (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "PREFIX=")
       #:strip-binaries? #f             ; Stripping breaks SBCL binaries.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-common-lisp-cache-folder
           (lambda _
             (setenv "HOME" "/tmp")
             #t))
         (add-before 'check 'configure-tests
           (lambda _
             (setenv "NYXT_TESTS_NO_NETWORK" "1")
             (setenv "NYXT_TESTS_ERROR_ON_FAIL" "1")
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin/nyxt"))
                    (glib-networking (assoc-ref inputs "glib-networking"))
                    (libs '("gsettings-desktop-schemas"))
                    (path (string-join
                           (map (lambda (lib)
                                  (string-append (assoc-ref inputs lib) "/lib"))
                                libs)
                           ":"))
                    (gi-path (getenv "GI_TYPELIB_PATH"))
                    (xdg-path (string-join
                               (map (lambda (lib)
                                      (string-append (assoc-ref inputs lib) "/share"))
                                    libs)
                               ":")))
               (wrap-program bin
                 `("GIO_EXTRA_MODULES" prefix
                   (,(string-append glib-networking "/lib/gio/modules")))
                 `("GI_TYPELIB_PATH" prefix (,gi-path))
                 `("LD_LIBRARY_PATH" ":" prefix (,path))
                 `("XDG_DATA_DIRS" ":" prefix (,xdg-path)))
               #t))))))
    ;; We use `cl-*' inputs and not `sbcl-*' ones so that CCL users can also use
    ;; this Guix manifests.
    ;;
    ;; Another reason is to not fail when an input dependency is found in
    ;; ~/common-lisp, which would trigger a rebuild of the SBCL input in the
    ;; store, which is read-only and would thus fail.
    ;;
    ;; The official Guix package should use `sbcl-*' inputs though.
    (native-inputs
     `(("prove" ,cl-prove)
       ("sbcl" ,sbcl)
       ;; Only for development, unneeded for the upstream Guix package:
       ("cl-trivial-benchmark" ,cl-trivial-benchmark)
       ;; To generate the right version in Nyxt.  Unneeded for the upstream Guix package.
       ("git" ,git-minimal)))
    (inputs
     `(("alexandria" ,cl-alexandria)
       ("bordeaux-threads" ,cl-bordeaux-threads)
       ("cl-base64" ,cl-base64)
       ("cl-calispel" ,cl-calispel)
       ("cl-containers" ,cl-containers)
       ("cl-css" ,cl-css)
       ("cl-custom-hash-table" ,cl-custom-hash-table)
       ("cl-gopher" ,cl-gopher)
       ("cl-html-diff" ,cl-html-diff)
       ("cl-json" ,cl-json)
       ("cl-ppcre" ,cl-ppcre)
       ("cl-prevalence" ,cl-prevalence)
       ("cl-qrencode" ,cl-qrencode)
       ("closer-mop" ,cl-closer-mop)
       ("cluffer" ,cl-cluffer)
       ("dexador" ,cl-dexador)
       ("enchant" ,cl-enchant)
       ("flexi-streams" ,cl-flexi-streams)
       ("fset" ,cl-fset)
       ("hu.dwim.defclass-star" ,cl-hu.dwim.defclass-star)
       ("iolib" ,cl-iolib)
       ("local-time" ,cl-local-time)
       ("lparallel" ,cl-lparallel)
       ("log4cl" ,cl-log4cl)
       ("mk-string-metrics" ,cl-mk-string-metrics)
       ("moptilities" ,cl-moptilities)
       ("named-readtables" ,cl-named-readtables)
       ;; ("osicat" ,cl-osicat) ; Not needed for SBCL.
       ("parenscript" ,cl-parenscript)
       ("phos" ,cl-phos)
       ("plump" ,cl-plump)
       ("clss" ,cl-clss)
       ("quri" ,cl-quri)
       ("serapeum" ,cl-serapeum)
       ("spinneret" ,cl-spinneret)
       ("str" ,cl-str)
       ("swank" ,cl-slime-swank)
       ("trivia" ,cl-trivia)
       ("trivial-clipboard" ,cl-trivial-clipboard)
       ("trivial-features" ,cl-trivial-features)
       ("trivial-garbage" ,cl-trivial-garbage)
       ("trivial-package-local-nicknames" ,cl-trivial-package-local-nicknames)
       ("trivial-types" ,cl-trivial-types)
       ("unix-opts" ,cl-unix-opts)
       ;; System deps
       ("gcc" ,gcc-toolchain)           ; Needed for cl-iolib.
       ;; WebKitGTK deps
       ("cl-cffi-gtk" ,cl-cffi-gtk)
       ("cl-webkit" ,cl-webkit)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ;; GObjectIntrospection
       ("cl-gobject-introspection" ,cl-gobject-introspection)
       ("gtk" ,gtk+)                    ; For the main loop.
       ("webkitgtk" ,webkitgtk)         ; Required when we use its typelib.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (synopsis "Extensible web-browser in Common Lisp")
    (home-page "https://nyxt.atlas.engineer")
    (description "Nyxt is a keyboard-oriented, extensible web-browser
designed for power users.  The application has familiar Emacs and VI
key-bindings and is fully configurable and extensible in Common Lisp.")
    (license license:bsd-3)))

nyxt
