;;; run.lisp --- helper runner program to run from the command line
(require "asdf")

(push (directory-namestring *load-pathname*) asdf:*central-registry*)
(ql:quickload "next")

;; start nEXT
(next:start)
