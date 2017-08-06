;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; next.asd
(defsystem :next
  :serial t
  :depends-on (:cl-strings)
  :components ((:file "lisp/package")
	       (:file "lisp/qt")
	       (:file "lisp/document-mode")
	       (:file "lisp/buffer")
	       (:file "lisp/keymap")
	       (:file "lisp/base")))

;; (asdf:make-build :next
;;                  :type :program
;;                  :move-here #P"./"
;;                  :epilogue-code '(next:start))
