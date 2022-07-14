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
  #:use-module (gnu packages aspell)
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

(define %source-dir (dirname (dirname (current-filename))))

(define (nyxt-git-version)              ; Like Nyxt's `+version+'.
  (let* ((pipe (with-directory-excursion %source-dir
                 (open-pipe* OPEN_READ "git" "describe" "--always" "--tags")))
         (version (read-line pipe)))
    (close-pipe pipe)
    version))

;; TODO: Contribute ndebug and trivial-custom-debugger to Guix.

(define-public sbcl-trivial-custom-debugger
  (let ((commit "a560594a673bbcd88136af82086107ee5ff9ca81"))
    (package
      (name "sbcl-trivial-custom-debugger")
      (version (git-version "1.0.0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/phoe/trivial-custom-debugger")
               (commit commit)))
         (file-name (git-file-name "cl-trivial-custom-debugger" version))
         (sha256
          (base32 "1iri5wsp9sc1f5q934cj87zd79r5dc8fda0gl7x1pz95v0wx28yk"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-parachute))
      (home-page "https://github.com/phoe/trivial-custom-debugger")
      (synopsis
       "Allows arbitrary functions to become the standard Lisp debugger")
      (description
       "This is a portability library that allows one to fully override the standard
debugger provided by their Common Lisp system for situations where binding
@code{*debugger-hook*} is not enough - most notably, for @code{#'break}.
@itemize

@item The main interface is the @code{with-debugger} macro that accepts a hook
function that would be acceptable for @code{*debugger-hook*} (it must accept two
arguments: a condition that the debugger is invoked with and a similar hook
function).

@item A functional interface for the above is provided in form of
@code{call-with-debugger}, which accepts a hook function and a zero-argument thunk.

@item It is possible, though not advisable, to install a debugger function
globally across the whole Lisp system. The function @code{install-debugger} is
provided for that case. (If you want to use it, you might want to contribute a
matching @code{uninstall-debugger} function that restores the Lisp system to the
previous state.)

@end itemize")
      (license license:bsd-3))))

(define-public cl-trivial-custom-debugger
  (sbcl-package->cl-source-package sbcl-trivial-custom-debugger))

(define-public ecl-trivial-custom-debugger
  (sbcl-package->ecl-package sbcl-trivial-custom-debugger))

(define-public sbcl-ndebug
  (let ((commit "0f2df5e537eb749d769de1d786bdbb282f655104"))
    (package
      (name "sbcl-ndebug")
      (version (git-version "0.0.1" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/atlas-engineer/ndebug")
               (commit commit)))
         (file-name (git-file-name "cl-ndebug" version))
         (sha256
          (base32 "1fjg17b9148razg2avrjq8qw665q39nlaag6k0rw93rz31r1sm4k"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-dissect
             sbcl-trivial-custom-debugger
             sbcl-slime-swank
             sbcl-lparallel))
      (home-page "https://github.com/atlas-engineer/ndebug")
      (synopsis
       "A Common Lisp tool to construct interface-aware debugger hooks")
      (description
       "NDebug provides a small set of utilities to make graphical (or, rather
non-REPL-resident) Common Lisp application easier to integrate with the standard
Lisp debugger (@code{*debugger-hook*}, namely) and implementation-specific
debugger hooks (via @code{trivial-custom-debugger}), especially in the
multi-threaded context.")
      (license license:bsd-3))))

(define-public cl-ndebug
  (sbcl-package->cl-source-package sbcl-ndebug))

(define-public ecl-ndebug
  (sbcl-package->ecl-package sbcl-ndebug))

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
          cl-dissect
          cl-trivial-custom-debugger
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
          cl-ndebug
          cl-nfiles
          cl-nhooks
          cl-nkeymaps
          cl-osicat ; Not needed for SBCL, remove it in Guix upstream package.
          cl-parenscript
          cl-phos
          cl-plump
          cl-ppcre
          cl-prevalence
          cl-py-configparser
          cl-qrencode
          cl-quri
          cl-serapeum
          cl-spinneret
          cl-str
          cl-slime-swank
          cl-slynk
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
   (propagated-inputs
    (list
     ;; Useful for video playback in all-inclusive Guix profiles.
     ;; For now upstream Guix does not include the GST plugins.
     gst-libav
     gst-plugins-bad
     gst-plugins-base
     gst-plugins-good
     gst-plugins-ugly
     ;; For spell-checking.  Same, upstream Guix does not include it in the package.
     aspell
     aspell-dict-en))
   (synopsis "Extensible web browser in Common Lisp")
   (home-page "https://nyxt.atlas.engineer")
   (description "Nyxt is a keyboard-oriented, extensible web browser
designed for power users.  The application has familiar Emacs and VI
keybindings and is fully configurable and extensible in Common Lisp.")
   (license license:bsd-3)))

nyxt
