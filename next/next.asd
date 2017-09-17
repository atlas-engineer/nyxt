;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; next.asd
(defsystem :next
  :serial t
  :depends-on (:cl-strings :cl-string-match :quri)
  :components ((:file "lisp/package")
	       (:file "lisp/macro")
	       (:file "lisp/qt")
	       (:file "lisp/mode")
	       (:file "lisp/tree-history-mode")
	       (:file "lisp/keymap")
	       (:file "lisp/minibuffer")
	       (:file "lisp/buffer")
	       (:file "lisp/document-mode")
	       (:file "lisp/completion")
	       (:file "lisp/base")))
