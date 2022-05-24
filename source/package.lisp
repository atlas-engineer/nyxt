;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

;; Some compilers (e.g. SBCL) fail to reload the system with `defpackage' when
;; exports are spread around.  `uiop:define-package' does not have this problem.
(uiop:define-package nyxt
  (:use #:common-lisp)
  (:import-from #:trivia #:match #:multiple-value-match #:lambda-match #:guard)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:export #:use-nyxt-package-nicknames)
  (:documentation "The core package of Nyxt, the infinitely extensible browser.

This package should not be modified by the users.

It's recommended to use the `nyxt-user' package instead to create new functions,
modes, commands, etc."))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nyxt)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :nyxt)
  (trivial-package-local-nicknames:add-package-local-nickname :lpara :lparallel :nyxt)
  (trivial-package-local-nicknames:add-package-local-nickname :class* :hu.dwim.defclass-star :nyxt)
  (trivial-package-local-nicknames:add-package-local-nickname :hooks :nhooks :nyxt)
  (trivial-package-local-nicknames:add-package-local-nickname :files :nfiles :nyxt))

(defmacro nyxt::use-nyxt-package-nicknames ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (dolist (package (trivial-package-local-nicknames:package-local-nicknames :nyxt))
       (trivial-package-local-nicknames:add-package-local-nickname (first package) (package-name (rest package))))))

(uiop:define-package nyxt-user
  (:use #:common-lisp #:nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:class-star #:define-class)
  (:documentation "Package left for the user to fiddle with.  If the
configuration file package is left unspecified, it defaults to this.  It's not
recommended to use `nyxt' itself to avoid clobbering internal symbols.

By default, the `:nyxt' and `:common-lisp' packages are `:use'd.

To import more symbols, you can use the `import' function.
For instance, to access `match' directly (without having to prefix it with
`trivia:', add this at the top of your configuration file:

  (import 'trivia:match)

You can also use package local nicknames if you want to abbreviate package
prefix.
For instance, to be able to use `alex:' and `sera:' in place of `alexandria:'
and `serapeum:':

  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nyxt-user)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :nyxt-user)"))

(trivial-package-local-nicknames:add-package-local-nickname :hooks :nhooks :nyxt-user)
(trivial-package-local-nicknames:add-package-local-nickname :files :nfiles :nyxt-user)

;; Unlike other modes, nyxt/prompt-buffer-mode is declared here because
;; certain files depend upon its existence being declared beforehand
;; (for compilation).
;; TODO: See if prompt-buffer-mode can be declared in prompt-buffer-mode.lisp.
(uiop:define-package nyxt/prompt-buffer-mode
  (:use #:common-lisp #:nyxt)
  (:import-from #:trivia #:match #:multiple-value-match #:lambda-match #:guard)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:documentation "Mode for prompter buffer."))
