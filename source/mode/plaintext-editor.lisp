;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/mode/editor)

(define-mode plaintext-editor-mode (editor-mode)
  "Mode for basic plaintext editing.

It renders the file in a single textarea HTML element.  Enabled by default for
`editor-buffer's."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (style (theme:themed-css (theme *browser*)
            `("body"
              :margin 0)
            `("#editor"
              :font-family ,theme:monospace-font-family
              :margin 0
              :height "100%"
              :width "100%"
              :border "none"
              :outline "none"
              :padding "5px"
              :padding-top "18px"
              :autofocus "true"
              :background-color ,theme:background
              :color ,theme:on-background))))
  (:toggler-command-p nil))

(defmethod markup ((editor plaintext-editor-mode) content)
  (spinneret:with-html-string
    (:head
     (:nstyle (style editor))
     (:nstyle (style (buffer editor))))
    (:body
     (render-menu 'nyxt/mode/editor:editor-mode)
     (:textarea :id "editor" :name "editor" :autofocus t content))))

(defmethod set-content ((editor plaintext-editor-mode) content)
  (ps-eval :async t :buffer (buffer editor)
    (setf (ps:@ (nyxt/ps:qs document "#editor") value)
          (ps:lisp content))))

(defmethod get-content ((editor plaintext-editor-mode))
  (ps-eval :buffer (buffer editor)
    (ps:chain (nyxt/ps:qs document "#editor") value)))
