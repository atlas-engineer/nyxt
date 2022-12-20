;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; Test with
;;
;;  guix shell --pure -D -f build-scripts/nyxt-quicklisp.scm openssl -- bash -c 'make all && make install DESTDIR=/tmp/nyxt-output/'

;; Notes: openssl is required for cl-cookie, etc.  For some reason, it's not
;; dragged into the environment when added as propagated-input.

(define-module (nyxt-quicklisp)
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages c)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages gnome)
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

(define nyxt
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
             (setenv "NASDF_TESTS_NO_NETWORK" "1")
             (setenv "NASDF_TESTS_QUIT_ON_FAIL" "1")
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
                    (gi-path (string-join
                              (map (lambda (lib)
                                     (string-append lib "/lib/girepository-1.0"))
                                   libs)
                              ":"))
                    (xdg-path (string-join
                               (map (lambda (lib)
                                      (string-append lib "/share"))
                                    libs)
                               ":")))
               (wrap-program bin
                 `("GIO_EXTRA_MODULES" prefix
                   (,(string-append #(this-package-input "glib-networking") "/lib/gio/modules")))
                 `("GI_TYPELIB_PATH" prefix (,gi-path))
                 `("LD_LIBRARY_PATH" ":" prefix (,path))
                 `("XDG_DATA_DIRS" ":" prefix (,xdg-path)))
               #t))))))
   (native-inputs
    (list sbcl))
   (inputs
    (list
     ;; System deps
     gcc-toolchain                      ; Needed for cl-iolib.
     glib
     glib-networking
     gsettings-desktop-schemas
     gtk+                          ; For the main loop.
     webkitgtk                     ; Required when we use its typelib.
     gobject-introspection))
   (propagated-inputs
    ;; For iolib, etc.
    (list libfixposix))
   (synopsis "Extensible web browser in Common Lisp")
   (home-page "https://nyxt.atlas.engineer")
   (description "Nyxt is a keyboard-oriented, extensible web browser
designed for power users.  The application has familiar Emacs and VI
keybindings and is fully configurable and extensible in Common Lisp.")
   (license license:bsd-3)))

nyxt
