;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/headings-panel-mode
  (:use :common-lisp :nyxt)
  (:documentation "Mode for selecting and jumping to headings"))
(in-package :nyxt/headings-panel-mode)

(define-mode headings-panel-mode ()
  "Mode for selecting and jumping to headings"
  ((rememberable-p nil)))

(nyxt::define-panel headings (panel-buffer)
  (flet ((buffer-markup (buffer)
           "Create the presentation for a buffer."
           (spinneret:with-html
             (:p (:a :class "button"
                     :href (lisp-url `(nyxt::switch-buffer :id ,(id buffer)))
                     (:span :title (title buffer) :class "title" (title buffer)))))))
    (spinneret:with-html-string (:style (style panel-buffer))
      (:style (cl-css:css
               '((".button"
                  :white-space "nowrap"
                  :overflow-x "hidden"
                  :display "block"
                  :text-overflow "ellipsis"))))
      (:body
       (:h1 "Buffers")
       (:a :class "button" :href (lisp-url '(nyxt/buffer-listing-mode::|SHOW-BUFFERS-PANEL|)) "Update ↺")
       (loop for buffer in (nyxt/web-mode::get-headings)
             collect (buffer-markup buffer))))))
