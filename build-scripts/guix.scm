;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to it and run:
;;
;;   guix package -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;
;; To start in a container, run:
;;
;;   guix environment --container --network --expose=/etc/ssl/certs --ad-hoc coreutils nss-certs -l guix.scm -- env DISPLAY="$DISPLAY" next
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
             (guix build-system asdf)
             (guix build-system glib-or-gtk)
             (guix build-system trivial)
             (gnu packages)
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

(define sbcl-next-download-manager
  (package
    (name "sbcl-next-download-manager")
    (version "0.0.0")                   ; Because this can be checkout.
    (source (local-file %source-dir #:recursive? #t #:select? git-file?))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #f                      ; Require network.
       #:asd-file "next.asd"
       #:asd-system-name "next/download-manager"))
    (inputs
     `(("cl-ppcre" ,sbcl-cl-ppcre)
       ("dexador" ,sbcl-dexador)
       ("log4cl" ,sbcl-log4cl)
       ("lparallel" ,sbcl-lparallel)
       ("quri" ,sbcl-quri)
       ("str" ,sbcl-cl-str)))
    (synopsis "Extensible web-browser in Common Lisp (download manager)")
    (home-page "https://next.atlas.engineer")
    (description "Next is a keyboard-oriented, extensible web-browser
designed for power users.  The application has familiar Emacs and VI
key-bindings and is fully configurable and extensible in Common Lisp.")
    (license bsd-3)))

(define sbcl-next-ring
  (package
    (inherit sbcl-next-download-manager)
    (name "sbcl-next-ring")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:asd-file "next.asd"
       #:asd-system-name "next/ring"))
    (native-inputs
     `(("prove" ,sbcl-prove)))
    (synopsis "Extensible web-browser in Common Lisp (ring)")))

(define sbcl-next-history-tree
  (package
    (inherit sbcl-next-download-manager)
    (name "sbcl-next-history-tree")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:asd-file "next.asd"
       #:asd-system-name "next/history-tree"))
    (native-inputs
     `(("prove" ,sbcl-prove)))
    (synopsis "Extensible web-browser in Common Lisp (history-tree)")))

(define sbcl-next-password-manager
  (package
    (inherit sbcl-next-download-manager)
    (name "sbcl-next-password-manager")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:asd-file "next.asd"
       #:asd-system-name "next/password-manager"))
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cl-annot" ,sbcl-cl-annot)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("str" ,sbcl-cl-str)
       ("trivial-clipboard" ,sbcl-trivial-clipboard)))
    (synopsis "Extensible web-browser in Common Lisp (password manager)")))

(define sbcl-next-hooks
  (package
    (inherit sbcl-next-download-manager)
    (name "sbcl-next-hooks")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:asd-file "next.asd"
       #:asd-system-name "next/hooks"))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("serapeum" ,sbcl-serapeum)))
    (native-inputs
     `(("prove" ,sbcl-prove)))
    (synopsis "Extensible web-browser in Common Lisp (hooks)")))

(define sbcl-next-keymap
  (package
    (inherit sbcl-next-download-manager)
    (name "sbcl-next-keymap")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:asd-file "next.asd"
       #:asd-system-name "next/keymap"))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("fset" ,sbcl-fset)
       ("str" ,sbcl-cl-str)))
    (native-inputs
     `(("prove" ,sbcl-prove)))
    (synopsis "Extensible web-browser in Common Lisp (keymap)")))

(define sbcl-next
  (package
    (inherit sbcl-next-download-manager)
    (name "sbcl-next")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:asd-file "next.asd"
       #:asd-system-name "next"))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cl-annot" ,sbcl-cl-annot)
       ("cl-ansi-text" ,sbcl-cl-ansi-text)
       ("cl-css" ,sbcl-cl-css)
       ("cl-json" ,sbcl-cl-json)
       ("cl-markup" ,sbcl-cl-markup)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("cl-ppcre-unicode" ,sbcl-cl-ppcre-unicode)
       ("cl-prevalence" ,sbcl-cl-prevalence)
       ("closer-mop" ,sbcl-closer-mop)
       ("dexador" ,sbcl-dexador)
       ("enchant" ,sbcl-enchant)
       ("fset" ,sbcl-fset)
       ("iolib" ,sbcl-iolib)
       ("ironclad" ,sbcl-ironclad)
       ("local-time" ,sbcl-local-time)
       ("log4cl" ,sbcl-log4cl)
       ("lparallel" ,sbcl-lparallel)
       ("mk-string-metrics" ,sbcl-mk-string-metrics)
       ("moptilities" ,sbcl-moptilities)
       ("osicat" ,next-osicat)
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
       ;; Local deps
       ("next-download-manager" ,sbcl-next-download-manager)
       ("next-ring" ,sbcl-next-ring)
       ("next-history-tree" ,sbcl-next-history-tree)
       ("next-password-manager" ,sbcl-next-password-manager)
       ("next-hooks" ,sbcl-next-hooks)
       ("next-keymap" ,sbcl-next-keymap)))
    (native-inputs
     `(("prove" ,sbcl-prove)))
    (synopsis "Extensible web-browser in Common Lisp (without renderer)")))

(define-public next
  (let ((version (package-version sbcl-next-download-manager)))
    (package
      (inherit sbcl-next-download-manager)
      (name "next")
      (build-system asdf-build-system/sbcl)
      (outputs '("out" "lib"))
      (arguments
       `(#:asd-file "next.asd"
         #:asd-system-name "next/gtk"
         #:phases
         (modify-phases %standard-phases
           (add-after 'patch-platform-port-path 'patch-version
             ;; When the version is not just dot-separated numerals
             ;; (e.g. a git-commit version), Guix modifies the .asd with
             ;; an illegal version number, and then Next fails to query
             ;; it.  So we hard-code it here.
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((version (format #f "~a" ,version)))
                 (substitute* "source/global.lisp"
                   (("version\\)\\)\\)")
                    (string-append "version)))"
                                   "\n"
                                   "(setf +version+ \"" version "\")"))))
               #t))
           (add-before 'cleanup 'move-bundle
             (lambda* (#:key outputs #:allow-other-keys)
               (define lib (assoc-ref outputs "lib"))
               (define actual-fasl (string-append
                                    lib "/lib/sbcl/source/next/gtk--system.fasl"))
               (define expected-fasl (string-append
                                      lib "/lib/sbcl/gtk--system.fasl"))
               (format #t "Move ~s -> ~s" actual-fasl expected-fasl)
               (copy-file actual-fasl expected-fasl)
               #t))
           (add-after 'create-symlinks 'build-program
             (lambda* (#:key outputs #:allow-other-keys)
               (build-program
                (string-append (assoc-ref outputs "out") "/bin/next-gtk")
                outputs
                #:entry-program '((next:entry-point) 0))))
           (add-after 'build-program 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((final-bin (string-append (assoc-ref outputs "out") "/bin/next"))
                      (bin (string-append final-bin "-gtk"))
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
                 (rename-file bin final-bin)
                 #t)))
           (add-before 'build 'install-assets
             ;; Since the ASDF build system generates a new .asd with a
             ;; possibly suffixed and thus illegal version number, assets
             ;; should not be installed after the 'build phase or else
             ;; the illegal version will result in NIL in the .desktop
             ;; file.
             (lambda* (#:key outputs #:allow-other-keys)
               (with-output-to-file "version"
                 (lambda _
                   (format #t "~a" ,(package-version sbcl-next-download-manager))))
               (invoke "make" "install-assets"
                       (string-append "PREFIX="
                                      (assoc-ref outputs "out"))))))))
      (inputs
       `(,@(package-inputs sbcl-next)
         ("cl-cffi-gtk" ,sbcl-cl-cffi-gtk)
         ("cl-webkit" ,sbcl-cl-webkit)
         ;; WebKitGTK deps
         ("glib-networking" ,glib-networking)
         ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
         ("next" ,sbcl-next)))
      (synopsis "Extensible web-browser in Common Lisp"))))

next
