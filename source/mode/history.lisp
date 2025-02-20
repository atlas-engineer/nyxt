;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/history
  (:documentation "Package for `history-mode'.
 Mode to store current buffer navigation into the global history."))

(in-package :nyxt/mode/history)

(define-mode history-mode ()
  "Mode to manage navigation history."
  ((visible-in-status-p nil)
   (history-blocklist '("https://duckduckgo.com/l/")
                      :type (list-of string)
                      :documentation "URL prefixes to not save in history.
Example: DuckDuckGo redirections should be ignored or else going backward in
history after consulting a result reloads the result, not the DuckDuckGo
search.")
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
(defun blocked-p (url mode)
  "Check whether URL belongs to MODE's `history-blocklist'."
  (find-if (rcurry #'str:starts-with? (render-url url))
           (history-blocklist mode)))

(define-internal-page-command-global list-history (&key (limit 100))
    (buffer "*History list*")
  "Display the user history as a list.
Cut the display at LIMIT nodes."
  (spinneret:with-html-string
    (:nstyle (style buffer))
    (:h1 "History")
    (:table :class "resizable-table"
            (:tr (:th "Title") (:th "URL"))
            (loop for i from 0 below (min limit
                                          (length (history-vector *browser*)))
                  collect (let ((element (aref (history-vector *browser*) i)))
                            (:tr (:td (title element))
                                 (:td (:a :href (quri:render-uri (url element))
                                          (quri:render-uri (url element))))))))))

(defun add-url-to-history (url buffer mode &key (title ""))
  "Add URL to BUFFER's `history-MODE'.
Uses `history-add' internally."
  (unless (or (url-empty-p url)
              (blocked-p url mode))
    (log:debug "Notify URL ~a for buffer ~a with load status ~a"
               url
               buffer
               (slot-value buffer 'nyxt::status))
    (vector-push-extend (make-instance 'history-entry
                                       :url (quri:uri url)
                                       :title title)
                        (history-vector *browser*))
    (files:with-file-content (history (history-file *browser*))
      (setf history (history-vector *browser*)))
    url))

(defmethod nyxt:on-signal-load-finished ((mode history-mode) url title)
  (add-url-to-history url (buffer mode) mode :title title)
  url)
