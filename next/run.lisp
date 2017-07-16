;;; run.lisp --- helper runner program to run from the command line
;;; to launch nEXT simply `eql5 run.lisp`

(load "lisp/base")
(load "lisp/qt")
(load "lisp/keymap")

;; load the user configuration if it exists
(load "~/.next.d/init.lisp" :if-does-not-exist nil)

(next:start)
