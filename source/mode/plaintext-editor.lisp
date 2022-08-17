;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/editor-mode)

(define-mode plaintext-editor-mode (editor-mode)
  "Mode for basic plaintext editing."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (style (theme:themed-css (theme *browser*)
            ("body"
             :margin 0)
            ("#editor"
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
             :background-color theme:background
             :color theme:on-background))))
  (:toggler-command-p nil))

(defmethod enable ((editor plaintext-editor-mode) &key)
  (let ((content (spinneret:with-html-string
                   (:head (:style (style editor)))
                   (:body
                    (:textarea :id "editor" :name "editor" :autofocus t)))))
    (peval :async t :buffer (buffer editor)
      (ps:chain document (write (ps:lisp content))))))

(defmethod set-content ((editor plaintext-editor-mode) content)
  (pflet ((set-content
           :async t :buffer (buffer editor) (content)
           (setf (ps:chain (nyxt/ps:qs document "#editor") value)
                 (ps:lisp content))))
    (set-content content)))

(defmethod get-content ((editor plaintext-editor-mode))
  (peval :buffer (buffer editor)
    (ps:chain (nyxt/ps:qs document "#editor") value)))

(defmethod nyxt:default-modes append ((buffer editor-buffer))
  '(plaintext-editor-mode))
