;;; package.lisp --- Definition of packages used by Next.

(in-package :cl-user)

;; Some compilers (e.g. SBCL) fail to reload the system with `defpackage' when
;; exports are spread around.  `uiop:define-package' does not have this problem.
(uiop:define-package next
  (:use :common-lisp :trivia)
  (:import-from #:keymap #:define-key #:define-scheme))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :next)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :next))

(uiop:define-package next-user
  (:use :common-lisp :trivia :next)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "
Package left for the user to fiddgle with.
It's recommended to use this package in the Next configuration file, instead of
`next' itself."))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :next)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :next-user))

(uiop:define-package parenscript-user
  (:use :common-lisp :next :parenscript))
