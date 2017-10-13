;;; run.lisp --- helper runner program to run from the command line
;;; to launch nEXT simply `eql5 run.lisp`
(push "./" asdf:*central-registry*)
(ql:quickload "next")
(asdf:load-system "next")

(in-package #:next)

(initialize-bookmark-db)

;; start nEXT
(start)

;; load the user configuration if it exists
;;; FIXME: should this execute before START?
(load "~/.next.d/init.lisp" :if-does-not-exist nil)
