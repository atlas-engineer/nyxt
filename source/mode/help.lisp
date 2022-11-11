;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/help-mode
    (:documentation "Mode to enhance navigation on internal documentation pages."))
(in-package :nyxt/help-mode)

(define-mode help-mode ()
  "Mode for help and documentation pages."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "help-mode" ()
      keyscheme:default
      (list
       "q" 'delete-current-buffer
       "n" 'nyxt/document-mode:next-heading
       "p" 'nyxt/document-mode:previous-heading
       "m" 'nyxt/document-mode:jump-to-heading
       "s" 'nyxt/search-buffer-mode:search-buffer
       "t" 'nyxt/document-mode:headings-panel
       "?" (resolve-symbol :describe-bindings :command)))))
  (:toggler-command-p nil))
