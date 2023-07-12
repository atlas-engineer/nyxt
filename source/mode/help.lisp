;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/help
  (:documentation "Mode to enhance navigation on internal documentation pages."))
(in-package :nyxt/mode/help)

(define-mode help-mode ()
  "Mode for help and documentation pages.

Useful to enable on Nyxt help pages (such as `manual' or `describe-*') to
provide convenient navigation keybindings.  For instance, \"s\" becomes bound
`nyxt/mode/search-buffer:search-buffer'."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "help-mode" ()
      keyscheme:default
      (list
       "q" 'delete-current-buffer
       "n" 'nyxt/mode/document:next-heading
       "p" 'nyxt/mode/document:previous-heading
       "m" 'nyxt/mode/document:jump-to-heading
       "s" 'nyxt/mode/search-buffer:search-buffer
       "t" 'nyxt/mode/document:headings-panel
       "?" (sym:resolve-symbol :describe-bindings :command)))))
  (:toggler-command-p nil))
