;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; next.asd
(defsystem :next
  :serial t
  :depends-on (:cl-strings)
  :components ((:file "lisp/base")))

;; (asdf:make-build :next
;;                  :type :program
;;                  :move-here #P"./"
;;                  :epilogue-code '(next:start))

