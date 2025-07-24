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
  (labels ((buffer-markup (buffer)
             "Present a buffer in HTML."
             (let ((*print-pretty* nil))
               (spinneret:with-html
                 (:p :class "buffer-listing"
                     (:nbutton
                       :text "✕"
                       :title "Delete buffer"
                       `(nyxt::delete-buffer :buffers ,buffer)
                       `(ffi-buffer-reload ,listing-buffer))
                     (:nbutton
                       :class "buffer-button"
                       :text (format nil "~a - ~a"
                                     (render-url (url buffer)) (title buffer))
                       :title "Switch to buffer"
                       `(nyxt::set-current-buffer ,buffer)))))))
    (spinneret:with-html-string
      (render-menu 'nyxt/mode/buffer-listing:buffer-listing-mode listing-buffer)
      (:h1 "Buffers")
      (:nstyle
        '(.buffer-listing
         :display "flex")
        '(.buffer-button
          :text-align "left"
          :flex-grow "1"))
      (:div
       (dolist (buffer (buffer-list))
         (buffer-markup buffer))))))
