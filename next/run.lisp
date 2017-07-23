;;; run.lisp --- helper runner program to run from the command line
;;; to launch nEXT simply `eql5 run.lisp`
(load "dependencies")

(push "./" asdf:*central-registry*)
(asdf:load-system "next")
