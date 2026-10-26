;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/buffer-listing
  (:documentation "Package for `buffer-listing-mode', mode for buffer listing."))
(in-package :nyxt/mode/buffer-listing)

(define-mode buffer-listing-mode ()
  "Mode for buffer-listing.
Hosts `list-buffers' page."
  ()
  (:toggler-command-p nil))

(define-internal-page-command-global list-buffers ()
    (listing-buffer "*Buffers*" 'nyxt/mode/buffer-listing:buffer-listing-mode)
  "Show all buffers."
  ;; This page is rendered before its own title is stored on the buffer.
  (when (or (str:emptyp (title listing-buffer))
            (string= (title listing-buffer) (render-url (url listing-buffer))))
    (setf (title listing-buffer) "*Buffers*"))
  (labels ((buffer-row (buffer)
             "Present BUFFER as a title, its URL, and a close control."
             ;; Fresh binding so each row's callback closes over its own buffer.
             (let* ((buffer buffer)
                    (url (render-url (url buffer)))
                    (title (title buffer))
                    (heading (if (str:emptyp title) url title))
                    (detail (unless (string= heading url)
                              url)))
               (spinneret:with-html
                 (:li
                   (:a :href "#"
                       :title (or detail heading)
                       :onclick (ps:ps
                                  (ps:chain event (prevent-default))
                                  (nyxt/ps:lisp-eval
                                   (:title "switch-buffer"
                                    :buffer listing-buffer)
                                   (set-current-buffer buffer))
                                  nil)
                       (:span heading)
                       (:span detail))
                   (:button :class "close"
                            :type "button"
                            :title "Close"
                            :onclick (ps:ps
                                       (nyxt/ps:lisp-eval
                                        (:title "close-buffer"
                                         :buffer listing-buffer)
                                        (delete-buffer :buffers buffer)
                                        (unless (eq buffer listing-buffer)
                                          (ffi-buffer-reload listing-buffer))))
                            "×"))))))
    (spinneret:with-html-string
      (:nstyle (style listing-buffer))
      ;; Close control is 28px inside a 17px inset. 17 + 28 + 17 = 62.
      ;; The control sits on the link, so the link's own hover ends there.
      (:nstyle
        '("ul.list > li > a"
          :padding-right "62px")
        `("ul.list > li:hover > a"
          :background-color ,theme:background-color-
          :opacity "1")
        '("ul.list > li:hover::after"
          :background-color "transparent")
        '("ul.list > li:has(+ li:hover)::after"
          :background-color "transparent")
        `("ul.list button.close"
          :position "absolute"
          :z-index "1"
          :top "13px"
          :right "17px"
          :display "flex"
          :align-items "center"
          :justify-content "center"
          :box-sizing "border-box"
          :width "28px"
          :height "28px"
          :margin "0"
          :padding "0"
          :border "none"
          :border-radius "14px"
          :appearance "none"
          :background-color "transparent"
          :color ,theme:primary-color
          :font-size "17px"
          :line-height "17px"
          :cursor "pointer"
          :opacity "0"
          :transition "opacity 120ms ease, background-color 120ms ease")
        '("ul.list > li:hover > button.close, ul.list button.close:focus-visible"
          :opacity "1")
        `("ul.list button.close:hover"
          :background-color ,theme:background-color
          :color ,theme:on-background-color)
        `("ul.list button.close:focus-visible"
          :outline "2px solid"
          :outline-color ,theme:action-color
          :outline-offset "2px"))
      (render-menu 'buffer-listing-mode listing-buffer)
      (:h1 "Buffers")
      (:ul :class "list"
           (dolist (buffer (buffer-list))
             (buffer-row buffer))))))
