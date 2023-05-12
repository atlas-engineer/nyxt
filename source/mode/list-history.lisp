;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/list-history
  (:documentation "Package for `list-history-mode', mode for history listing styles."))
(in-package :nyxt/mode/list-history)

(define-mode list-history-mode ()
  "Mode for listing history.
Used by `nyxt/mode/history:list-history'."
  ((visible-in-status-p nil)
   (style (theme:themed-css (theme *browser*)
            `(a
              :color ,theme:on-background)
            `("a:hover"
              :opacity 0.5))))
  (:toggler-command-p nil))
