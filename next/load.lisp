;;; load.lisp --- helper runner program to load system from the command line
(require "asdf")

(push (directory-namestring *load-pathname*) asdf:*central-registry*)
(ql:quickload "next")
