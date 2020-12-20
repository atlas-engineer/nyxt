;;; Commentary:
;;
;; GNU Guix SBCL development package.
;;
;; This recipe provides an SBCL tailored for Nyxt.  It contains all Nyxt
;; dependencies, preloaded.  Thus SBCL should start instantly and build Nyxt
;; blazingly fast.
;;
;; Switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm --profile=/path/to/profile
;;
;; To start the resulting SBCL in a pure environment:
;;
;;   env -i HOME=$HOME $(which bash) --norc --noprofile -c 'source /path/to/profile/etc/source && sbcl'
;;
;; Alternatively, you can start this SBCL without installing a profile:
;;
;;   guix environment --pure --preserve='^HOME$' --load=guix-sbcl-for-nyxt.scm -- sbcl
;;
;;; Code:

(load "guix.scm")

;; From guix.scm:
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
             (gnu packages lisp)
             (gnu packages lisp-xyz)
             (gnu packages gnome)
             (gnu packages gstreamer)
             (gnu packages gtk)
             (gnu packages pkg-config)
             (gnu packages version-control)
             (gnu packages webkit))

;; For SBCL:
(use-modules (guix build-system copy)
             (gnu packages glib)
             (gnu packages gnupg))

(define-public sbcl-for-nyxt
  (package
    (name "sbcl-for-nyxt")
    (version (package-version sbcl))
    (source (package-source nyxt))
    (build-system copy-build-system)
    (arguments
     `(#:strip-binaries? #f          ; Stripping breaks SBCL binaries.
       #:install-plan '()            ; Don't copy the Nyxt source to the output.
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'dump-sbcl
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "HOME" "/tmp")
             (let* ((out (assoc-ref outputs "out"))
                    (core (string-append out "/bin/sbcl"))
                    (setup-file "setup.lisp"))
               (with-output-to-file setup-file
                 (lambda ()
                   (write
                    '(require "asdf"))
                   (write
                    '(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2)))
                   (write
                    `(push (uiop:ensure-directory-pathname ,(getcwd)) asdf:*central-registry*))
                   (write
                    '(mapc 'asdf:load-system
                           (delete-if (lambda (s)
                                        (string= "nyxt" (subseq s 0 (min (length s) 4))))
                                      (append
                                       (asdf:system-depends-on (asdf:find-system :nyxt))
                                       (asdf:system-depends-on (asdf:find-system :nyxt/gtk))))))
                   (write
                    `(setf asdf:*central-registry*
                           (delete (uiop:ensure-directory-pathname ,(getcwd)) asdf:*central-registry*
                                   :test 'equal)))
                   (write
                    '(asdf:clear-configuration))
                   ;; If we don't set `uiop:*image-entry-point*' the result exits immediately.
                   (write
                    '(setf uiop:*image-entry-point* 'sb-impl::toplevel-init))
                   (write
                    ;; Important to set it executable so that `uiop:restore-image'
                    ;; is called on image startup, which resets the cache dir.
                    `(uiop:dump-image ,core :executable t))))
               (mkdir-p (dirname core))
               (invoke "sbcl" "--non-interactive" "--load" setup-file)))))))
    ;; We put everything as propagated input so that installing this package to
    ;; a profile exposes everything necessary to build Nyxt.
    (propagated-inputs
     `(,@(package-native-inputs nyxt)
       ,@(package-inputs nyxt)
       ;; To export GIO_EXTRA_MODULES, etc.
       ("glib" ,glib)
       ;; For user data decryption:
       ("gnupg" ,gnupg)))
    (synopsis "Optimized SBCL for Nyxt development")
    (home-page (package-home-page sbcl))
    (description (package-description sbcl))
    (license (package-license sbcl))))

sbcl-for-nyxt
