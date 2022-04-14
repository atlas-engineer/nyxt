;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/help-mode
  (:use :common-lisp :nyxt)
  (:import-from #:serapeum #:export-always)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for help pages"))
(in-package :nyxt/help-mode)

(define-mode help-mode ()
  "Mode for displaying documentation."
  ((rememberable-p nil)
   ;; add 'Type "?" for help' at the top of the web-buffer
   ;; (constructor
   ;;  (lambda (mode)
   ;;    (...)))
   (keymap-scheme
    (define-scheme "help-mode"
      scheme:cua
      (list
       "q" 'nyxt::delete-current-buffer
       "n" 'nyxt/web-mode::next-heading
       "p" 'nyxt/web-mode::previous-heading
       "m" 'nyxt/web-mode:jump-to-heading
       "s" 'nyxt/web-mode::search-buffer
       "t" 'nyxt::headings-panel
       ;; TODO local help-mode keybindings don't appear when describe-bindings
       ;; is called.  Why?  %define-internal-page?
       "?" 'nyxt::describe-bindings)))))
