;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/history
  (:documentation "Package for `history-mode', mode to manage browsing history."))

(in-package :nyxt/mode/history)

(define-mode history-mode ()
  "Mode to manage navigation history."
  ((visible-in-status-p nil)
   (history-blocklist
    '()
    :type (or null (list-of string))
    :documentation "A list of URL prefixes that are excluded from history.")
   (keyscheme-map
    (define-keyscheme-map "history-mode" ()
      keyscheme:default
      (list
       "M-left" 'history-backwards
       "M-right" 'history-forwards
       "M-]" 'history-forwards
       "M-[" 'history-backwards)
      #+darwin keyscheme:cua
      #+darwin
      (list
       "S-[" 'history-backwards
       "S-]" 'history-forwards
       "S-y" 'list-history)
      keyscheme:emacs
      (list
       "C-b" 'history-backwards
       "C-f" 'history-forwards)
      keyscheme:vi-normal
      (list
       "H" 'history-backwards
       "L" 'history-forwards)))))

(define-configuration context-buffer
  ((default-modes (cons 'history-mode %slot-value%))))

(define-command history-backwards (buffer)
  "Navigate backwards."
  (ffi-buffer-navigate-backwards buffer))

(define-command history-forwards (buffer)
  "Navigate forwards."
  (ffi-buffer-navigate-forwards buffer))

(export-always 'blocked-p)
(defmethod blocked-p (url (mode history-mode))
  "Check whether URL belongs to MODE's `history-blocklist'."
  (find-if (rcurry #'str:starts-with? (render-url url))
           (history-blocklist mode)))

(-> history-stamp (history-entry) list)
(defun history-stamp (entry)
  "Return the parts of ENTRY the list shows."
  (list (title entry) (render-url (url entry))))

(-> history-row (history-entry buffer) t)
(defun history-row (entry page)
  "Return a row for ENTRY: its title and its address.
A panel row loads the address in the page beside PAGE."
  (let* ((url (render-url (url entry)))
         (title (title entry))
         (heading (if (str:emptyp title) url title))
         (detail (unless (string= heading url) url))
         (panel-p (panel-buffer-p page)))
    (make-instance
     'user-interface:element
     :tag :li
     :children
     (list (make-instance
            'user-interface:element
            :tag :a
            :title (or detail heading)
            :attributes (if panel-p
                            '(:href "#")
                            (list :href url))
            :action (when panel-p
                      (ps:ps
                        (ps:chain event (prevent-default))
                        (nyxt/ps:lisp-eval
                         (:title "open-history"
                          :buffer page)
                         (open-url-from-panel page url))
                        nil))
            :children
            (list (make-instance 'user-interface:element
                                 :tag :span
                                 :text heading)
                  (make-instance 'user-interface:element
                                 :tag :span
                                 :text (or detail ""))))))))

(define-mode history-listing-mode ()
  "Mode for the `list-history' page."
  ((entry-limit
    100
    :type integer
    :documentation "How many entries `list-history' shows.")
   (listing
    (make-instance 'user-interface:dynamic-element
                   :tag :p
                   :html-class "empty"
                   :text "No history."
                   :item-key #'history-stamp)
    :export nil
    :documentation "The list shown on `list-history'.
A `user-interface:dynamic-element'."))
  (:toggler-command-p nil))

(-> render-history (history-listing-mode) t)
(defun render-history (mode)
  "Show the recent history in MODE.
The list element keeps a row it has already built."
  (let ((page (buffer mode))
        (element (listing mode)))
    (when (and *browser* page element (not (dead-buffer-p page)))
      (user-interface:batch (element)
        (setf (user-interface:items element)
              (recent-history-entries (entry-limit mode) *browser*))
        (if (plusp (length (user-interface:items element)))
            (setf (user-interface:tag element) :ul
                  (user-interface:html-class element) "list"
                  (user-interface:text element) nil)
            (setf (user-interface:tag element) :p
                  (user-interface:html-class element) "empty"
                  (user-interface:text element) "No history."))))))

(defmethod enable ((mode history-listing-mode) &key)
  "Keep MODE's list in step with browsing history."
  (setf (user-interface:template (listing mode))
        (lambda (entry)
          (history-row entry (buffer mode))))
  (define-setf-handler history-file history-revision mode
    (lambda (file)
      (when (and *browser* (eq file (history-file *browser*)))
        (render-history mode))))
  (render-history mode))

(defmethod disable ((mode history-listing-mode) &key)
  (user-interface:disconnect (listing mode)))

(-> history-limit (t) integer)
(defun history-limit (value)
  "Return VALUE as a history entry count.
A value that is not an integer, or not its decimal text, counts as 100."
  (handler-case (if (integerp value) value (parse-integer value))
    (error () 100)))

(define-internal-page-command-global list-history (&key (limit 100))
    (buffer "*History list*" 'nyxt/mode/history:history-listing-mode)
  "Display the most recent browsing history entries up to LIMIT."
  (let ((mode (find-mode buffer 'history-listing-mode)))
    (setf (entry-limit mode) (history-limit limit))
    (render-history mode)
    (let ((listing (listing mode)))
      (user-interface:connect listing buffer)
      (spinneret:with-html-string
        (:nstyle
          `("p.empty"
            :margin "0"
            :font-size "17px"
            :line-height "24px"
            :color ,theme:primary-color))
        (when (include-in-buffer-list-p buffer)
          (render-menu 'history-mode buffer)
          (:h1 "History"))
        (user-interface:to-html listing)))))

(define-buffer-panel history-panel (:title "History")
  "Panel that lists recent history."
  (ffi-buffer-load panel (quri:uri (nyxt-url 'list-history))))

(defmethod add-url-to-history (url (mode history-mode) &key (title ""))
  "Push URL to `history-vector'."
  (unless (blocked-p url mode)
    (let ((parsed (url url)))
      (unless (url-empty-p parsed)
        (bt:with-recursive-lock-held ((nyxt::history-lock *browser*))
          (with-slots (history-vector history-file) *browser*
            (vector-push-extend (make-instance 'history-entry
                                               :url parsed
                                               :title (or title ""))
                                history-vector)
            (setf (files:content history-file) history-vector)))
        (note-history-changed)
        url))))

(defmethod nyxt:on-signal-load-finished ((mode history-mode) url title)
  (add-url-to-history url mode :title title)
  url)
