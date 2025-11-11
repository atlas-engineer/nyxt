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

(define-command history-backwards (&key (buffer (current-buffer)))
  "Navigate backwards."
  (ffi-buffer-navigate-backwards buffer))

(define-command history-forwards (&key (buffer (current-buffer)))
  "Navigate forwards."
  (ffi-buffer-navigate-forwards buffer))

(export-always 'blocked-p)
(defmethod blocked-p (url (mode history-mode))
  "Check whether URL belongs to MODE's `history-blocklist'."
  (find-if (rcurry #'str:starts-with? (render-url url))
           (history-blocklist mode)))

(define-internal-page-command-global list-history (&key (limit 100))
    (buffer "*History list*")
  "Display the most recent browsing history entries up to LIMIT."
  (spinneret:with-html-string
    (:nstyle (style buffer))
    (:h1 "History")
    (:table :class "resizable-table"
            (:tr (:th "Title") (:th "URL"))
            (loop for entry in (recent-history-entries limit *browser*)
                  for title = (title entry)
                  for url = (quri:render-uri (url entry))
                  collect (:tr (:td title) (:td (:a :href url url)))))))

(defmethod add-url-to-history (url (mode history-mode) &key (title ""))
  "Push URL to `history-vector'."
  (unless (or (blocked-p url mode)
              (nyxt:find-submode 'nyxt/mode/incognito:incognito-mode (buffer mode)))
    (with-slots (history-vector history-file) *browser*
      (vector-push-extend (make-instance 'history-entry
                                         :url (quri:uri url)
                                         :title title)
                          history-vector)
      (files:with-file-content (history history-file)
        (setf history history-vector))
      url)))

(defmethod nyxt:on-signal-load-finished ((mode history-mode) url title)
  (add-url-to-history url mode :title title)
  url)
