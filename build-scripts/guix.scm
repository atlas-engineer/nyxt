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
             (gnu packages lisp)
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

(define next-gtk-webkit
  (package
    (name "next-gtk-webkit")
    (version "0.0.0")                   ; Because this can be checkout.
    (source (local-file %source-dir #:recursive? #t #:select? git-file?))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list "gtk-webkit"
                          (string-append
                           "CC="
                           (assoc-ref %build-inputs "gcc-7")
                           "/bin/gcc")
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "install-gtk-webkit" make-flags))))))
    (inputs
     `(("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("webkitgtk" ,webkitgtk-2.24)))
    (native-inputs
     `(("gcc-7" ,gcc-7) ; needed because webkitgtk-2.24 is compiled with gcc-7
       ("pkg-config" ,pkg-config)))
    (home-page "https://next.atlas.engineer")
    (synopsis "Infinitely extensible web-browser (user interface only)")
    (description "Next is a keyboard-oriented, extensible web-browser
inspired by Emacs and designed for power users.  The application has familiar
key-bindings, is fully configurable and extensible in Lisp, and has powerful
features for productive professionals.")
    (license bsd-3)))

(define sbcl-next-download-manager
  (package
    (inherit next-gtk-webkit)
    (name "sbcl-next-download-manager")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #f                      ; Need online access.
       #:asd-file "next.asd"
       #:asd-system-name "next/download-manager"))
    (inputs
     `(("cl-ppcre" ,sbcl-cl-ppcre)
       ("dexador" ,sbcl-dexador)
       ("log4cl" ,sbcl-log4cl)
       ("lparallel" ,sbcl-lparallel)
       ("quri" ,sbcl-quri)
       ("str" ,sbcl-cl-str)))
    (native-inputs
     `(("trivial-features" ,sbcl-trivial-features)
       ("prove-asdf" ,sbcl-prove-asdf)))
    (synopsis "Infinitely extensible web-browser (download manager)")))

(define sbcl-next-ring
  (package
    (inherit next-gtk-webkit)
    (name "sbcl-next-ring")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #t
       #:asd-file "next.asd"
       #:asd-system-name "next/ring"))
    (native-inputs
     `(("trivial-features" ,sbcl-trivial-features)
       ("prove-asdf" ,sbcl-prove-asdf)))
    (synopsis "Infinitely extensible web-browser (ring)")))

(define sbcl-next-history-tree
  (package
    (inherit next-gtk-webkit)
    (name "sbcl-next-history-tree")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #t
       #:asd-file "next.asd"
       #:asd-system-name "next/history-tree"))
    (native-inputs
     `(("trivial-features" ,sbcl-trivial-features)
       ("prove-asdf" ,sbcl-prove-asdf)))
    (synopsis "Infinitely extensible web-browser (history-tree)")))

(define sbcl-next-password-manager
  (package
    (inherit next-gtk-webkit)
    (name "sbcl-next-password-manager")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #t
       #:asd-file "next.asd"
       #:asd-system-name "next/password-manager"))
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cl-annot" ,sbcl-cl-annot)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("str" ,sbcl-cl-str)
       ("trivial-clipboard" ,sbcl-trivial-clipboard)))
    (native-inputs
     `(("trivial-features" ,sbcl-trivial-features)
       ("prove-asdf" ,sbcl-prove-asdf)))
    (synopsis "Infinitely extensible web-browser (password manager)")))

(define-public next
  (let ((version (package-version next-gtk-webkit)))
    (package
      (inherit next-gtk-webkit)
      (name "next")
      (build-system asdf-build-system/sbcl)
      (outputs '("out" "lib"))
      (arguments
       `(#:tests? #f                    ; no tests
         #:asd-system-name "next"
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'patch-platform-port-path
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "source/ports/gtk-webkit.lisp"
                          (("\"next-gtk-webkit\"")
                           (string-append "\"" (assoc-ref inputs "next-gtk-webkit")
                                          "/bin/next-gtk-webkit\"")))
                        #t))
                    (add-after 'patch-platform-port-path 'patch-version
                      ;; When the version is not just dot-separated numerals
                      ;; (e.g. a git-commit version), Guix modifies the .asd with
                      ;; an illegal version number, and then Next fails to query
                      ;; it.  So we hard-code it here.
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let ((version (format #f "~a" ,version)))
                          (substitute* "source/global.lisp"
                            (("version\\)\\)\\)")
                             (string-append "version)))
(setf +version+ \"" version "\")"))))
                        #t))
                    (add-before 'cleanup 'move-bundle
                      (lambda* (#:key outputs #:allow-other-keys)
                        (define lib (assoc-ref outputs "lib"))
                        (define actual-fasl (string-append
                                             lib
                                             "/lib/sbcl/next.fasl"))
                        (define expected-fasl (string-append
                                               lib
                                               "/lib/sbcl/next--system.fasl"))
                        (copy-file actual-fasl expected-fasl)
                        #t))
                    (add-after 'create-symlinks 'build-program
                      (lambda* (#:key outputs #:allow-other-keys)
                        (build-program
                         (string-append (assoc-ref outputs "out") "/bin/next")
                         outputs
                         #:entry-program '((next:entry-point) 0))))
                    (add-before 'build 'install-assets
                      ;; Since the ASDF build system generates a new .asd with a
                      ;; possibly suffixed and thus illegal version number, assets
                      ;; should not be installed after the 'build phase or else
                      ;; the illegal version will result in NIL in the .desktop
                      ;; file.
                      (lambda* (#:key outputs #:allow-other-keys)
                        (with-output-to-file "version"
                          (lambda _
                            (format #t "~a" ,(package-version next-gtk-webkit))))
                        (invoke "make" "install-assets"
                                (string-append "PREFIX="
                                               (assoc-ref outputs "out"))))))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cl-annot" ,sbcl-cl-annot)
         ("cl-ansi-text" ,sbcl-cl-ansi-text)
         ("cl-css" ,sbcl-cl-css)
         ("cl-hooks" ,sbcl-cl-hooks)
         ("cl-json" ,sbcl-cl-json)
         ("cl-markup" ,sbcl-cl-markup)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("cl-ppcre-unicode" ,sbcl-cl-ppcre-unicode)
         ("cl-prevalence" ,sbcl-cl-prevalence)
         ("closer-mop" ,sbcl-closer-mop)
         ("dbus" ,cl-dbus)
         ("dexador" ,sbcl-dexador)
         ("ironclad" ,sbcl-ironclad)
         ("local-time" ,sbcl-local-time)
         ("log4cl" ,sbcl-log4cl)
         ("lparallel" ,sbcl-lparallel)
         ("mk-string-metrics" ,sbcl-mk-string-metrics)
         ("parenscript" ,sbcl-parenscript)
         ("quri" ,sbcl-quri)
         ("sqlite" ,sbcl-cl-sqlite)
         ("str" ,sbcl-cl-str)
         ("swank" ,sbcl-slime-swank)
         ("trivia" ,sbcl-trivia)
         ("trivial-clipboard" ,sbcl-trivial-clipboard)
         ("unix-opts" ,sbcl-unix-opts)
         ;; Local deps
         ("next-gtk-webkit" ,next-gtk-webkit)
         ("next-download-manager" ,sbcl-next-download-manager)
         ("next-ring" ,sbcl-next-ring)
         ("next-history-tree" ,sbcl-next-history-tree)
         ("next-password-manager" ,sbcl-next-password-manager)))
      (native-inputs
       `(("trivial-features" ,sbcl-trivial-features)
         ("prove-asdf" ,sbcl-prove-asdf)))
      (synopsis "Infinitely extensible web-browser (with Lisp development files)"))))

(define-public sbcl-next
  (deprecated-package "sbcl-next" next))

next
