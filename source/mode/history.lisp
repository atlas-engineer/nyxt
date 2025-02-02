;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/history
  (:documentation "Package for `history-mode', mode to store current buffer navigation into the global history."))
(in-package :nyxt/mode/history)

(define-mode history-mode ()
  "Mode to manage navigation history."
  ((visible-in-status-p nil)
   (history-blocklist '("https://duckduckgo.com/l/")
                      ;; TODO: Find a more automated way to do it.  WebKitGTK
                      ;; automatically removes such redirections from its
                      ;; history.  How?
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

(define-command history-backwards ()
  "Navigate backwards.")

(define-command history-forwards ()
  "Navigate forwards.")

(defun history-add (url &key (title "") (buffer (current-buffer)))
  "Add URL to the global history."
  ;; TODO
  )

(export-always 'blocked-p)
(defun blocked-p (url mode)
  "Check whether URL belongs to MODE's `history-blocklist'."
  (find-if (rcurry #'str:starts-with? (render-url url))
           (history-blocklist mode)))

(defun add-url-to-history (url buffer mode)
  "Add URL to BUFFER's `history-MODE'.
Uses `history-add' internally."
  (unless (or (url-empty-p url)
              (blocked-p url mode))
    (log:debug "Notify URL ~a for buffer ~a with load status ~a"
               url
               buffer
               (slot-value buffer 'nyxt::status))
    (unless (quri:uri= url (url buffer))
      (log:debug "Picking different buffer URL instead: ~a"
                 (url buffer)))
    (when (eq (slot-value buffer 'nyxt::status) :finished)
      (with-current-buffer buffer
        (history-add (url buffer) :title (title buffer)
                                  :buffer buffer)))
    url))

(defmethod nyxt:on-signal-load-started ((mode history-mode) url)
  (add-url-to-history url (buffer mode) mode)
  url)

(defmethod nyxt:on-signal-load-finished ((mode history-mode) url)
  (add-url-to-history url (buffer mode) mode)
  url)

