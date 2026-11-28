;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/buffer-listing
  (:documentation "Package for `buffer-listing-mode', mode for buffer listing."))
(in-package :nyxt/mode/buffer-listing)

(-> shown-buffer (buffer) (maybe buffer))
(defun shown-buffer (page)
  "Return the buffer shown in the window PAGE belongs to.
A panel belongs to the window it is attached to. Any other page belongs to
the window that shows it."
  (when-let ((window (and page (window page))))
    (let ((current (buffer window)))
      (when (and current (not (dead-buffer-p current)))
        current))))

(-> buffer-stamp (buffer buffer) list)
(defun buffer-stamp (buffer page)
  "Return the parts of BUFFER the list shows.
Includes whether BUFFER is the one shown in PAGE's window."
  (list (title buffer)
        (render-url (url buffer))
        (eq buffer (shown-buffer page))))

(-> buffer-row (buffer buffer) t)
(defun buffer-row (buffer page)
  "Return a row for BUFFER: its title, its address, and a close control.
The row of the buffer shown in PAGE's window is marked current."
  (let* ((url (render-url (url buffer)))
         (title (title buffer))
         (heading (if (str:emptyp title) url title))
         (detail (unless (string= heading url)
                   url))
         (current-p (eq buffer (shown-buffer page))))
    (make-instance
     'user-interface:element
     :tag :li
     :html-class (when current-p "current")
     :attributes (when current-p '(:aria-current "true"))
     :children
     (list (make-instance 'user-interface:element
                          :tag :a
                          :title (or detail heading)
                          :attributes '(:href "#")
                          :action (ps:ps
                                    (ps:chain event (prevent-default))
                                    (nyxt/ps:lisp-eval
                                     (:title "switch-buffer"
                                      :buffer page)
                                     (set-current-buffer buffer))
                                    nil)
                          :children
                          (list (make-instance 'user-interface:element
                                               :tag :span
                                               :text heading)
                                (make-instance 'user-interface:element
                                               :tag :span
                                               :text (or detail ""))))
           (make-instance 'user-interface:button
                          :html-class "close"
                          :alt-text "Close"
                          :text "×"
                          :attributes '(:type "button")
                          :action (ps:ps
                                    (nyxt/ps:lisp-eval
                                     (:title "close-buffer"
                                      :buffer page)
                                     (delete-buffer :buffers buffer))))))))

(define-mode buffer-listing-mode ()
  "Mode for buffer-listing.
Hosts `list-buffers' page."
  ((listing
    (make-instance 'user-interface:dynamic-element
                   :tag :p
                   :html-class "empty"
                   :text "No buffers.")
    :export nil
    :documentation "The list shown on `list-buffers'.
A `user-interface:dynamic-element'."))
  (:toggler-command-p nil))

(-> render-buffers (buffer-listing-mode) t)
(defun render-buffers (mode)
  "Show the buffers in MODE.
The list element keeps a row it has already built."
  (let ((page (buffer mode))
        (element (listing mode)))
    (when (and *browser* page (not (dead-buffer-p page)))
      (user-interface:batch (element)
        (setf (user-interface:items element) (buffer-list))
        (if (plusp (length (user-interface:items element)))
            (setf (user-interface:tag element) :ul
                  (user-interface:html-class element) "list"
                  (user-interface:text element) nil)
            (setf (user-interface:tag element) :p
                  (user-interface:html-class element) "empty"
                  (user-interface:text element) "No buffers."))))))

(defmethod enable ((mode buffer-listing-mode) &key)
  "Keep MODE's list in step with the buffers.
The handlers are the ones `status-buffer' uses for its tabs."
  (setf (user-interface:template (listing mode))
        (lambda (buffer)
          (buffer-row buffer (buffer mode))))
  (setf (user-interface:item-key (listing mode))
        (lambda (buffer)
          (buffer-stamp buffer (buffer mode))))
  (define-setf-handler document-buffer url mode
    (lambda (_)
      (declare (ignore _))
      (render-buffers mode)))
  (define-setf-handler document-buffer title mode
    (lambda (_)
      (declare (ignore _))
      (render-buffers mode)))
  (define-setf-handler browser buffers mode
    (lambda (_)
      (declare (ignore _))
      (render-buffers mode)))
  (define-setf-handler window buffer mode
    (lambda (window)
      (when-let ((page (buffer mode)))
        (when (eq window (window page))
          (render-buffers mode)))))
  (render-buffers mode))

(defmethod disable ((mode buffer-listing-mode) &key)
  (user-interface:disconnect (listing mode)))

(define-internal-page-command-global list-buffers ()
    (listing-buffer "*Buffers*" 'nyxt/mode/buffer-listing:buffer-listing-mode)
  "Show all buffers."
  ;; This page is rendered before its own title is stored on the buffer.
  (when (or (str:emptyp (title listing-buffer))
            (string= (title listing-buffer) (render-url (url listing-buffer))))
    (setf (title listing-buffer) "*Buffers*"))
  (let ((listing (listing (find-mode listing-buffer 'buffer-listing-mode))))
    (user-interface:connect listing listing-buffer)
    (spinneret:with-html-string
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
        `("ul.list > li.current > a"
          :background-color ,theme:background-color-
          :opacity "1")
        '("ul.list > li.current > a span:first-of-type"
          :font-weight "600")
        '("ul.list > li.current::after"
          :background-color "transparent")
        '("ul.list > li:has(+ li.current)::after"
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
          :outline-offset "2px")
        `("p.empty"
          :margin "0"
          :font-size "17px"
          :line-height "24px"
          :color ,theme:primary-color))
      (when (include-in-buffer-list-p listing-buffer)
        (render-menu 'buffer-listing-mode listing-buffer)
        (:h1 "Buffers"))
      (user-interface:to-html listing))))

(define-buffer-panel buffer-listing-panel (:title "Buffers")
  "Panel that lists the open buffers."
  (ffi-buffer-load panel (quri:uri (nyxt-url 'list-buffers))))
