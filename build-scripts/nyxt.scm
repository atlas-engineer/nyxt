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
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages webkit))

(define-public sbcl-nfiles
  (package
   (name "sbcl-nfiles")
   (version "0.2.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/atlas-engineer/nfiles")
           (commit version)))
     (file-name (git-file-name "nfiles" version))
     (sha256
      (base32
       "02diypc5i36sv3kwjs0lk1y3r2zjv1k8g65w22844rbc881znb2h"))))
   (build-system asdf-build-system/sbcl)
   (inputs
    (list gnupg
          sbcl-alexandria
          sbcl-hu.dwim.defclass-star
          sbcl-serapeum
          sbcl-trivial-garbage
          sbcl-trivial-package-local-nicknames
          sbcl-trivial-types))
   (native-inputs
    (list sbcl-prove))
   (arguments
    (list #:phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'fix-paths
                (lambda _
                  (substitute* "gpg.lisp"
                    (("\"gpg\"")
                     (string-append "\""
                                    #$(this-package-input "gnupg") "/bin/gpg\""))))))))
   (home-page "https://github.com/atlas-engineer/nfiles")
   (synopsis "Manage file persistence and loading in Common Lisp")
   (description
    "NFiles is a Common Lisp library to help manage file persistence and
loading, in particular user-centric files like configuration files.")
   (license license:bsd-3)))

(define-public cl-nfiles                ; TODO: Add iolib for non-SBCL.
  (sbcl-package->cl-source-package sbcl-nfiles))

(define %source-dir (dirname (dirname (current-filename))))

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
   (source (local-file %source-dir #:recursive? #t #:select? (git-predicate %source-dir)))
   (build-system gnu-build-system) ; TODO: Use glib-or-gtk-build-system instead?
   (arguments
    (list
     #:make-flags #~(list "nyxt" "NYXT_SUBMODULES=false"
                          (string-append "DESTDIR=" #$output)
                          "PREFIX=")
     #:strip-binaries? #f            ; Stripping breaks SBCL binaries.
     #:phases
     #~(modify-phases %standard-phases
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
           (lambda _
             (let* ((bin (string-append #$output "/bin/nyxt"))
                    (libs (list #$(this-package-input "gsettings-desktop-schemas")))
                    (path (string-join
                           (map (lambda (lib)
                                  (string-append lib "/lib"))
                                libs)
                           ":"))
                    (gi-path (getenv "GI_TYPELIB_PATH"))
                    (xdg-path (string-join
                               (map (lambda (lib)
                                      (string-append lib "/share"))
                                    libs)
                               ":")))
               (wrap-program bin
                 `("GIO_EXTRA_MODULES" prefix
                   (,(string-append #$(this-package-input "glib-networking") "/lib/gio/modules")))
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
    (list cl-prove
          sbcl
          ;; Only for development, uneeded for the upstream Guix package:
          cl-trivial-benchmark))
   (inputs
    (list cl-alexandria
          cl-bordeaux-threads
          cl-base64
          cl-calispel
          cl-containers
          cl-css
          cl-closer-mop
          cl-clss
          cl-cluffer
          cl-custom-hash-table
          cl-dexador
          cl-enchant
          cl-flexi-streams
          cl-fset
          cl-gopher
          cl-html-diff
          cl-hu.dwim.defclass-star
          cl-iolib
          cl-json
          cl-local-time
          cl-lparallel
          cl-log4cl
          cl-mk-string-metrics
          cl-moptilities
          cl-named-readtables
          cl-nfiles
          cl-nhooks
          ;; cl-osicat ; Not needed for SBCL.
          cl-parenscript
          cl-phos
          cl-plump
          cl-ppcre
          cl-prevalence
          cl-qrencode
          cl-quri
          cl-serapeum
          cl-spinneret
          cl-str
          cl-slime-swank
          cl-tld
          cl-trivia
          cl-trivial-clipboard
          cl-trivial-features
          cl-trivial-garbage
          cl-trivial-package-local-nicknames
          cl-trivial-types
          cl-unix-opts
          ;; System deps
          gcc-toolchain                 ; Needed for cl-iolib
          cl-cffi-gtk
          cl-webkit
          glib-networking
          gsettings-desktop-schemas
          cl-gobject-introspection
          gtk+                      ; For the main loop
          webkitgtk                 ; Required when we use its typelib
          gobject-introspection
          pkg-config))
   (synopsis "Extensible web-browser in Common Lisp")
   (home-page "https://nyxt.atlas.engineer")
   (description "Nyxt is a keyboard-oriented, extensible web-browser
designed for power users.  The application has familiar Emacs and VI
key-bindings and is fully configurable and extensible in Common Lisp.")
   (license license:bsd-3)))

nyxt
