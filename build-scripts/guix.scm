;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=build-scripts/guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment --pure --load=build-scripts/guix.scm --ad-hoc glib glib-networking gsettings-desktop-schemas
;;
;; To build a local executable and then run it:
;;
;;   guix environment --pure --load=build-scripts/guix.scm -- make all NYXT_INTERNAL_QUICKLISP=false
;;   guix environment --pure --load=build-scripts/guix.scm -- ./nyxt
;;
;; To start in a container, run:
;;
;;   guix environment --no-grafts --load=build-scripts/guix.scm --container --network --share=/PATH/TO/YOUR/NYXT/CHECKOUT=/nyxt --preserve='^DISPLAY$' --expose=/etc/ssl/certs --ad-hoc nss-certs glib glib-networking gsettings-desktop-schemas
;;
;; Replace '/PATH/TO/YOUR/NYXT/CHECKOUT' as appropriate.
;; Then in the container environment:
;;
;;   cd /nyxt
;;   make all NYXT_INTERNAL_QUICKLISP=false
;;   ./nyxt
;;
;;; Code:

(use-modules (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-26)
             ((guix build utils) #:select (with-directory-excursion))
             (guix gexp)
             (guix packages)
             ((guix licenses) #:prefix license:)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (guix build-system glib-or-gtk)
             (gnu packages)
             (gnu packages glib)
             (gnu packages lisp)
             (gnu packages lisp-xyz)
             (gnu packages gnome)
             (gnu packages gstreamer)
             (gnu packages gtk)
             (gnu packages pkg-config)
             (gnu packages version-control)
             (gnu packages webkit))

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
     `(#:make-flags (list "nyxt" "NYXT_INTERNAL_QUICKLISP=false"
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
         (add-before 'build 'set-version
           (lambda _
             (setenv "NYXT_VERSION" ,version)
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
                    (gi-path (string-join
                              (map (lambda (lib)
                                     (string-append (assoc-ref inputs lib)
                                                    "/lib/girepository-1.0"))
                                   libs)
                              ":"))
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
    (native-inputs
     `(("prove" ,sbcl-prove)
       ("sbcl" ,sbcl)))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cl-calispel" ,sbcl-calispel)
       ("cl-containers" ,sbcl-cl-containers)
       ("cl-css" ,sbcl-cl-css)
       ("cl-custom-hash-table" ,sbcl-custom-hash-table)
       ("cl-json" ,sbcl-cl-json)
       ("cl-markup" ,sbcl-cl-markup)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("cl-prevalence" ,sbcl-cl-prevalence)
       ("closer-mop" ,sbcl-closer-mop)
       ("cluffer" ,sbcl-cluffer)
       ("dexador" ,sbcl-dexador)
       ("enchant" ,sbcl-enchant)
       ("fset" ,sbcl-fset)
       ("hu.dwim.defclass-star" ,sbcl-hu.dwim.defclass-star)
       ("iolib" ,sbcl-iolib)
       ("local-time" ,sbcl-local-time)
       ("log4cl" ,sbcl-log4cl)
       ("mk-string-metrics" ,sbcl-mk-string-metrics)
       ("moptilities" ,sbcl-moptilities)
       ("named-readtables" ,sbcl-named-readtables)
       ("osicat" ,sbcl-osicat)
       ("parenscript" ,sbcl-parenscript)
       ("plump" ,sbcl-plump)
       ("quri" ,sbcl-quri)
       ("serapeum" ,sbcl-serapeum)
       ("str" ,sbcl-cl-str)
       ("swank" ,sbcl-slime-swank)
       ("trivia" ,sbcl-trivia)
       ("trivial-clipboard" ,sbcl-trivial-clipboard)
       ("trivial-features" ,sbcl-trivial-features)
       ("trivial-package-local-nicknames" ,sbcl-trivial-package-local-nicknames)
       ("trivial-types" ,sbcl-trivial-types)
       ("unix-opts" ,sbcl-unix-opts)
       ("usocket" ,sbcl-usocket)
       ;; WebKitGTK deps
       ("cl-cffi-gtk" ,sbcl-cl-cffi-gtk)
       ("cl-webkit" ,sbcl-cl-webkit)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ;; GObjectIntrospection
       ("cl-gobject-introspection" ,sbcl-cl-gobject-introspection)
       ("gtk" ,gtk+)                    ; For the main loop.
       ("webkitgtk" ,webkitgtk)         ; Required when we use its typelib.
       ("gobject-introspection" ,gobject-introspection)))
    (synopsis "Extensible web-browser in Common Lisp")
    (home-page "https://nyxt.atlas.engineer")
    (description "Nyxt is a keyboard-oriented, extensible web-browser
designed for power users.  The application has familiar Emacs and VI
key-bindings and is fully configurable and extensible in Common Lisp.")
    (license license:bsd-3)))

nyxt
