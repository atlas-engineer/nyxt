;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:
;;
;; GNU Guix development package for Nyxt browser.
;;
;; To install:
;;
;;   guix package -f guix.scm
;;
;; To build:
;;
;;   guix build -f guix.scm
;;
;; To start the REPL:
;;
;;   guix shell -D -f guix.scm -- bash -c 'env LD_LIBRARY_PATH="$GUIX_ENVIRONMENT/lib" sbcl'
;;
;; See developer-manual.org for setting up the development environment.
;;
;;; Code:

(use-modules (guix build-system gnu)
             (guix gexp)
             (guix packages)
             (gnu packages c)
             (gnu packages glib)
             (gnu packages gnome)
             (gnu packages gstreamer)
             (gnu packages gtk)
             (gnu packages lisp)
             (gnu packages pkg-config)
             (gnu packages sqlite)
             (gnu packages tls)
             (gnu packages webkit)
             (gnu packages xdisorg)
             ((guix licenses) #:prefix license:))

(package
  (name "nyxt")
  (version "dev")
  (source (local-file (dirname (current-filename)) #:recursive? #t))
  (build-system gnu-build-system)
  (arguments
   `(#:make-flags (list "nyxt"
                        (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                        "PREFIX=")
     #:strip-binaries? #f               ; Stripping breaks SBCL binaries.
     #:phases
     (modify-phases %standard-phases
       (delete 'configure)
       (add-after 'unpack 'fix-so-paths
         (lambda* (#:key inputs #:allow-other-keys)
           (substitute* "_build/cl-plus-ssl/src/reload.lisp"
             (("libssl.so" all)
              (string-append (assoc-ref inputs "openssl") "/lib/" all))
             (("libcrypto.so" all)
              (string-append (assoc-ref inputs "openssl") "/lib/" all)))
           (substitute* "_build/iolib/src/syscalls/ffi-functions-unix.lisp"
             (("\(:default \"libfixposix\"\)")
              (string-append "(:default \""
                             (assoc-ref inputs "libfixposix")
                             "/lib/libfixposix\")")))
           (substitute* "_build/cl-sqlite/sqlite-ffi.lisp"
             (("libsqlite3" all)
              (string-append (assoc-ref inputs "sqlite") "/lib/" all)))
           (substitute* "_build/cl-gobject-introspection/src/init.lisp"
             (("libgobject-2\\.0\\.so")
              (search-input-file inputs "/lib/libgobject-2.0.so"))
             (("libgirepository-1\\.0\\.so")
              (search-input-file inputs "/lib/libgirepository-1.0.so")))
           (substitute* "_build/cl-webkit/webkit2/webkit2.init.lisp"
             (("libwebkit2gtk" all)
              (string-append (assoc-ref inputs "webkitgtk-for-gtk3") "/lib/" all)))
           (substitute* "_build/cl-cffi-gtk/glib/glib.init.lisp"
             (("libglib-[0-9.]*\\.so" all)
              (search-input-file inputs (string-append "/lib/" all)))
             (("libgthread-[0-9.]*\\.so" all)
              (search-input-file inputs (string-append "/lib/" all))))
           (substitute* "_build/cl-cffi-gtk/gobject/gobject.init.lisp"
             (("libgobject-[0-9.]*\\.so" all)
              (search-input-file inputs (string-append "/lib/" all))))
           (substitute* "_build/cl-cffi-gtk/gio/gio.init.lisp"
             (("libgio-[0-9.]*\\.so" all)
              (search-input-file inputs (string-append "/lib/" all))))
           (substitute* "_build/cl-cffi-gtk/cairo/cairo.init.lisp"
             (("libcairo\\.so" all)
              (search-input-file inputs (string-append "/lib/" all))))
           (substitute* "_build/cl-cffi-gtk/pango/pango.init.lisp"
             (("libpango-[0-9.]*\\.so" all)
              (search-input-file inputs (string-append "/lib/" all)))
             (("libpangocairo-[0-9.]*\\.so" all)
              (search-input-file inputs (string-append "/lib/" all))))
           (substitute* "_build/cl-cffi-gtk/gdk-pixbuf/gdk-pixbuf.init.lisp"
             (("libgdk_pixbuf-[0-9.]*\\.so" all)
              (search-input-file inputs (string-append "/lib/" all))))
           (substitute* "_build/cl-cffi-gtk/gdk/gdk.init.lisp"
             (("libgdk-[0-9]\\.so" all)
              (search-input-file inputs (string-append "/lib/" all))))
           (substitute* "_build/cl-cffi-gtk/gdk/gdk.package.lisp"
             (("libgtk-[0-9]\\.so" all)
              (search-input-file inputs (string-append "/lib/" all))))))
       (add-after 'unpack 'fix-clipboard-paths
         (lambda* (#:key inputs #:allow-other-keys)
           (substitute* "_build/trivial-clipboard/src/text.lisp"
             (("\"xsel\"")
              (string-append "\"" (assoc-ref inputs "xsel") "/bin/xsel\""))
             (("\"wl-copy\"")
              (string-append "\"" (assoc-ref inputs "wl-clipboard") "/bin/wl-copy\""))
             (("\"wl-paste\"")
              (string-append "\"" (assoc-ref inputs "wl-clipboard") "/bin/wl-paste\"")))))
       (add-before 'build 'fix-common-lisp-cache-folder
         (lambda _ (setenv "HOME" "/tmp")))
       (add-before 'check 'configure-tests
         (lambda _ (setenv "NASDF_TESTS_NO_NETWORK" "1")))
       (add-after 'install 'wrap-program
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((gsettings (assoc-ref inputs "gsettings-desktop-schemas")))
             (wrap-program (string-append (assoc-ref outputs "out") "/bin/nyxt")
               `("GIO_EXTRA_MODULES" prefix
                 (,(string-append (assoc-ref inputs "glib-networking")
                                  "/lib/gio/modules")))
               `("GI_TYPELIB_PATH" prefix (,(getenv "GI_TYPELIB_PATH")))
               `("LD_LIBRARY_PATH" ":" prefix (,(string-append gsettings "/lib")))
               `("XDG_DATA_DIRS" ":" prefix (,(string-append gsettings "/share"))))))))))
  (native-inputs (list sbcl))
  (inputs (list cairo
                gdk-pixbuf
                glib
                glib-networking
                gobject-introspection
                gsettings-desktop-schemas
                gst-libav
                gst-plugins-bad
                gst-plugins-base
                gst-plugins-good
                gst-plugins-ugly
                gtk+
                libfixposix
                openssl
                pango
                pkg-config
                sqlite
                webkitgtk-for-gtk3
                wl-clipboard
                xsel))
  (synopsis "Extensible web-browser in Common Lisp")
  (home-page "https://nyxt-browser.com/")
  (description "Nyxt is a keyboard-oriented, extensible web-browser designed
for power users.  The application has familiar Emacs and VI key-bindings and
is fully configurable and extensible in Common Lisp.")
  (license license:bsd-3))
