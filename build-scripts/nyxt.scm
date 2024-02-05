;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:
;;
;; GNU Guix development package.
;;
;; To install:
;;
;;   guix package --install-from-file=path/to/build-scripts/nyxt.scm
;;
;; To start the REPL:
;;
;;   guix shell -D -f build-scripts/nyxt.scm -- sbcl
;;
;; See documents/README.org on how to setup the development environment.
;;
;;; Code:

(use-modules (guix packages)
             (guix gexp)
             (gnu packages gstreamer)
             (gnu packages web-browsers)
             (gnu packages glib)
             (gnu packages gtk)
             (gnu packages webkit)
             (gnu packages gnome)
             (gnu packages pkg-config)
             (gnu packages lisp)
             (gnu packages lisp-xyz)
             (gnu packages lisp-check))

(package
  (inherit nyxt)
  (version "dev")
  (source (local-file (dirname (dirname (current-filename))) #:recursive? #t))
  (native-inputs
   (list sbcl
         cl-lisp-unit2
         ;; Useful for development, not needed upstream.
         ccl
         cl-trivial-benchmark))
  ;; `cl-*' inputs instead of `sbcl-*', since it defines a development
  ;; environment for any CL implementation.  Upstream uses `sbcl-*' to define
  ;; the Nyxt program.
  (inputs (list cl-alexandria
                cl-base64
                cl-bordeaux-threads
                cl-calispel
                cl-cffi-gtk             ; WebKitGTK
                cl-closer-mop
                cl-clss
                cl-cluffer
                cl-colors2
                cl-containers
                cl-custom-hash-table
                cl-dexador
                cl-dissect
                cl-enchant
                cl-flexi-streams
                cl-gobject-introspection ; WebKitGTK
                cl-gopher
                cl-history-tree
                cl-iolib
                cl-json
                cl-lass
                cl-local-time
                cl-log4cl
                cl-lparallel
                cl-moptilities
                cl-nclasses
                cl-ndebug
                cl-nfiles
                cl-nhooks
                cl-njson
                cl-nkeymaps
                cl-nsymbols
                cl-parenscript
                cl-phos
                cl-plump
                cl-ppcre
                cl-prevalence
                cl-prompter
                cl-py-configparser
                cl-qrencode
                cl-quri
                cl-serapeum
                cl-slime-swank
                cl-slynk
                cl-spinneret
                cl-sqlite
                cl-str
                cl-tld
                cl-trivia
                cl-trivial-clipboard
                cl-trivial-features
                cl-trivial-package-local-nicknames
                cl-trivial-types
                cl-unix-opts
                cl-webkit               ; WebKitGTK
                glib-networking
                gobject-introspection
                gsettings-desktop-schemas
                gst-libav
                gst-plugins-bad
                gst-plugins-base
                gst-plugins-good
                gst-plugins-ugly
                gtk+                    ; For the main loop
                pkg-config
                webkitgtk-for-gtk3      ; Required when we use its typelib
                )))
