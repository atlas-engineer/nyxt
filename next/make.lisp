;;;; make.lisp --- create binary files for nEXT
;;;;
;;;; See Next/next/README.org for more information on installing the
;;;; dependencies necessary to build nEXT from source
;;;;
;;;; Please note that this script must be run from the directory
;;;; Next/next.

(push "./" asdf:*central-registry*)
(ql:quickload "next")

;; execute make
(ext:run-program "make" nil :output t)

(quit)
