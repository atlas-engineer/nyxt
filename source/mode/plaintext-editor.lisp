;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/mode/editor)

(define-mode plaintext-editor-mode (editor-mode)
  "Mode for basic plaintext editing.

To enable it, add this to your configuration file:
\(define-configuration nyxt/mode/editor::editor-buffer
  ((default-modes (cons 'nyxt/mode/editor::plaintext-editor-mode %slot-value%))))"
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (style (theme:themed-css (theme *browser*)
            `("body"
              :margin 0)
            `("#editor"
              :margin 0
              :position "absolute"
              :top "0"
              :right "0"
              :bottom "0"
              :left "0"
              :border "none"
              :outline "none"
              :padding "5px"
              :autofocus "true"
              :background-color ,theme:background
              :color ,theme:on-background))))
  (:toggler-command-p nil))

(defmethod markup ((editor plaintext-editor-mode))
  (spinneret:with-html-string
    (:head
     (:nstyle (style editor)))
    (:body
     (:textarea :id "editor" :name "editor" :autofocus t))))

(defmethod set-content ((editor plaintext-editor-mode) content)
  (ps-labels :async t :buffer (buffer editor)
    ((set-content
      (content)
      (setf (ps:@ (nyxt/ps:qs document "#editor") value)
            (ps:lisp content))))
    (set-content content)))

(defmethod get-content ((editor plaintext-editor-mode))
  (ps-eval :buffer (buffer editor)
    (ps:chain (nyxt/ps:qs document "#editor") value)))
