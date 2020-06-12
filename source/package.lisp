(in-package :cl-user)

;; Some compilers (e.g. SBCL) fail to reload the system with `defpackage' when
;; exports are spread around.  `uiop:define-package' does not have this problem.
(uiop:define-package nyxt
  (:use :common-lisp :trivia)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:serapeum #:export-always))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nyxt)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :nyxt))

(uiop:define-package nyxt-user
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "
Package left for the user to fiddgle with.
It's recommended to use this package in the Nyxt configuration file, instead of
`nyxt' itself."))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nyxt)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :nyxt-user))

(uiop:define-package parenscript-user
  (:use :common-lisp :nyxt :parenscript))
