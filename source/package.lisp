;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

;; Some compilers (e.g. SBCL) fail to reload the system with `defpackage' when
;; exports are spread around.  `uiop:define-package' does not have this problem.
(uiop:define-package nyxt
  (:use :common-lisp :trivia)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum #:export-always))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nyxt)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :nyxt)
  (trivial-package-local-nicknames:add-package-local-nickname :hooks :serapeum/contrib/hooks :nyxt))

(uiop:define-package :nyxt/repl-mode
    (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-scheme)
  (:export :repl-mode))

(uiop:define-package nyxt-user
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "
Package left for the user to fiddgle with.
It's recommended to use this package in the Nyxt configuration file, instead of
`nyxt' itself."))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nyxt-user)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :nyxt-user)
  (trivial-package-local-nicknames:add-package-local-nickname :hooks :serapeum/contrib/hooks :nyxt-user))

(uiop:define-package parenscript-user
    (:use :common-lisp :nyxt :parenscript))


;; Unlike other modes, nyxt/minibuffer-mode is declared here because
;; certain files depend upon its existence being declared beforehand
;; (for compilation).
(uiop:define-package :nyxt/minibuffer-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:serapeum #:export-always)
  (:export :minibuffer-mode)
  (:documentation "Mode for minibuffer"))
;; TODO: See if prompt-buffer-mode can be declared in prompt-buffer-mode.lisp.
(uiop:define-package :nyxt/prompt-buffer-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:serapeum #:export-always)
  (:documentation "Mode for prompter buffer."))

;; nyxt/file-manager-mode is declared here due to the same reason as
;; nyxt/minibuffer-mode.
(uiop:define-package :nyxt/file-manager-mode
    (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Manage files.

Open any file from within Nyxt, with the usual fuzzy suggestion.

`M-x open-file (C-x C-f)'

\"file manager\" is a bit excessive for now. Currently, we can:
- browse files, with fuzzy-suggestion
- go one directory up (C-l)
- enter a directory (C-j)
- open files. By default, with xdg-open. See `open-file-function'.
")
  (:export :file-manager-mode)
  (:export :open-file-from-directory-suggestion-filter))
