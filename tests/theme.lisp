;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test runtime-theme-resolution ()
  (assert-eq theme:+acme-theme+ (nyxt::runtime-theme "acme"))
  (assert-eq theme:+kanagawa-dragon-theme+
             (nyxt::runtime-theme "kanagawa-dragon"))
  (assert-error 'error (nyxt::runtime-theme "unknown")))

(define-test runtime-theme-socket-request ()
  (assert-string= "acme"
                  (nyxt::parse-runtime-theme-request
                   "(nyxt:set-runtime-theme \"acme\")"))
  (assert-string= "kanagawa-dragon"
                  (nyxt::parse-runtime-theme-request
                   "(nyxt:set-runtime-theme \"kanagawa-dragon\")"))
  (assert-false
   (nyxt::parse-runtime-theme-request
    "(nyxt:set-runtime-theme \"unknown\")"))
  (assert-false
   (nyxt::parse-runtime-theme-request
    "(nyxt:set-runtime-theme \"acme\" :extra)"))
  (assert-false
   (nyxt::parse-runtime-theme-request
    "(cl-user::set-runtime-theme \"acme\")"))
  (assert-false
   (nyxt::parse-runtime-theme-request
    "(set-runtime-theme \"acme\")"))
  (assert-false
   (nyxt::parse-runtime-theme-request
    "(nyxt:set-runtime-theme \"acme\") (nyxt:set-runtime-theme \"acme\")"))
  (assert-false
   (nyxt::parse-runtime-theme-request
    "(nyxt:set-runtime-theme \"acme\" . :extra)"))
  (assert-false
   (nyxt::parse-runtime-theme-request
    "#1=(nyxt:set-runtime-theme \"acme\" . #1#)"))
  (assert-false
   (nyxt::parse-runtime-theme-request
    "#.(error \"reader evaluation must stay disabled\")")))

(define-test socket-url-parsing ()
  (assert-equal 1
                (length (nyxt::parse-urls
                         "(nyxt::open-urls \"https://example.org/\")")))
  (assert-false
   (nyxt::parse-urls "(nyxt:echo \"https://example.org/\")"))
  (assert-false
   (nyxt::parse-urls "#.(error \"reader evaluation must stay disabled\")")))
