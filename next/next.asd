;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; next.asd
(defsystem :next
  :serial t
  :depends-on (:cl-strings :cl-string-match)
  :components ((:file "lisp/package")
	       (:file "lisp/qt")
	       (:file "lisp/keymap")
	       (:file "lisp/minibuffer")
	       (:file "lisp/buffer")
	       (:file "lisp/document-mode")
	       (:file "lisp/completion")
	       (:file "lisp/base")))
