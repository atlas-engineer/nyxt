;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment --load=guix.scm
;;
;; To start in a container, run:
;;
;;   guix environment --load=guix.scm --container --network --share=/PATH/TO/YOUR/NYXT/CHECKOUT=/nyxt --preserve='^DISPLAY$' --expose=/etc/ssl/certs --ad-hoc nss-certs
;;
;; Replace '/PATH/TO/YOUR/NYXT/CHECKOUT' as appropriate.
;; Then in the container environment:
;;
;;   cd /nyxt
;;   make nyxt NYXT_INTERNAL_QUICKLISP=false LISP_FLAGS=
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
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (guix build-system glib-or-gtk)
             (gnu packages)
             (gnu packages lisp)
             (gnu packages lisp-xyz)
             (gnu packages gnome)
             (gnu packages gstreamer)
             (gnu packages gtk)
             (gnu packages pkg-config)
             (gnu packages gcc)
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
        ('directory #t)
        ((or 'regular 'symlink)
         (any (cut string-suffix? <> file) files))
        (_ #f)))))

(define-public nyxt
  (package
    (name "nyxt")
    (version "0.0.0")
    (source (local-file %source-dir #:recursive? #t #:select? git-file?))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "nyxt" "NYXT_INTERNAL_QUICKLISP=false"
                          (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "PREFIX=")
       #:strip-binaries? #f             ; Stripping breaks SBCL binaries.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-version ; Version is guessed from .git which Guix does not have.
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((version (format #f "~a" ,version)))
               (substitute* "source/global.lisp"
                 (("version\\)\\)\\)")
                  (string-append "version)))"
                                 "\n"
                                 "(setf +version+ \"" version "\")"))))
             #t))
         (add-before 'build 'make-desktop-version-number
           (lambda _
             (with-output-to-file "version"
               (lambda _
                 (format #t "~a" ,version)))))

         (delete 'configure)
         (add-before 'build 'fix-common-lisp-cache-folder
           (lambda _
             (setenv "HOME" "/tmp")
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
                                     (string-append (assoc-ref inputs lib) "/lib/girepository-1.0"))
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
     ;; We need to avoid sbcl-* inputs (sbcl-cl-cffi-gtk in particular) as they
     ;; seem to cause Nyxt to hang into a hogging process in about 10 minutes.
     ;; Probably an issue between CFFI and how we build SBCL packages.
     ;; See https://github.com/atlas-engineer/nyxt/issues/680.
     `(("alexandria" ,cl-alexandria)
       ("bordeaux-threads" ,cl-bordeaux-threads)
       ("cl-containers" ,cl-containers)
       ("cl-css" ,cl-css)
       ("cl-json" ,cl-json)
       ("cl-markup" ,cl-markup)
       ("cl-ppcre" ,cl-ppcre)
       ("cl-prevalence" ,cl-prevalence)
       ("closer-mop" ,cl-closer-mop)
       ("cluffer" ,cl-cluffer)
       ("dexador" ,cl-dexador)
       ("enchant" ,cl-enchant)
       ("fset" ,cl-fset)
       ("hu.dwim.defclass-star" ,cl-hu.dwim.defclass-star)
       ("iolib" ,cl-iolib)
       ("local-time" ,cl-local-time)
       ("log4cl" ,cl-log4cl)
       ("lparallel" ,cl-lparallel)
       ("mk-string-metrics" ,cl-mk-string-metrics)
       ("moptilities" ,cl-moptilities)
       ("osicat" ,sbcl-osicat)          ; SBCL version needed for libosicat.so.
       ("parenscript" ,cl-parenscript)
       ("plump" ,cl-plump)
       ("quri" ,cl-quri)
       ("serapeum" ,cl-serapeum)
       ("str" ,cl-str)
       ("swank" ,cl-slime-swank)
       ("trivia" ,cl-trivia)
       ("trivial-clipboard" ,cl-trivial-clipboard)
       ("trivial-features" ,cl-trivial-features)
       ("trivial-package-local-nicknames" ,cl-trivial-package-local-nicknames)
       ("trivial-types" ,cl-trivial-types)
       ("unix-opts" ,cl-unix-opts)
       ;; WebKitGTK deps
       ("cl-cffi-gtk" ,cl-cffi-gtk)
       ("cl-webkit" ,cl-webkit)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (synopsis "Extensible web-browser in Common Lisp")
    (home-page "https://nyxt.atlas.engineer")
    (description "Nyxt is a keyboard-oriented, extensible web-browser
designed for power users.  The application has familiar Emacs and VI
key-bindings and is fully configurable and extensible in Common Lisp.")
    (license bsd-3)))

nyxt
