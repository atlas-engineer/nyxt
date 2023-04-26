;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/bookmark-frequent-visits
    (:documentation "Mode to bookmark frequently visited URLs."))
(in-package :nyxt/bookmark-frequent-visits)

(define-mode bookmark-frequent-visits-mode ()
  "Mode to bookmark frequently visited URLs while navigating the web."
  ((visible-in-status-p nil)
   (threshold 20)))

(defmethod enable ((mode bookmark-frequent-visits-mode) &key)
  (nyxt:on-signal-load-finished mode (url (current-buffer))))

(defun bookmark-frequent-visit (threshold)
  "Check if current URL is frequently visited and not included in the
bookmarks. If this is the case, prompt the user about bookmarking it."
  (labels ((bookmarked-url-p (url-address)
             "The local function `bookmarked-url-p' checks if the current URL is
             already bookmarked or not."
             (let ((bookmark-url-strings
                     (mapcar #'(lambda (e) (render-url (url e)))
                             (files:content (nyxt/mode/bookmark:bookmarks-file (current-buffer))))))
               (find url-address bookmark-url-strings :test #'string=))))
    (sera:and-let* ((history-entries (let ((history (buffer-history)))
                                       (mapcar #'htree:data (alex:hash-table-keys (htree:entries history)))))
                    (current-url-history
                     (find (url (current-buffer)) history-entries :test #'equalp :key #'url))
                    (implicit-visits-value
                     (nyxt::implicit-visits current-url-history))
                    (current-url-string
                     (render-url (url current-url-history))))
      (when (and (> implicit-visits-value threshold)
                 (not (bookmarked-url-p current-url-string)))
        (if-confirm ((format nil "Bookmark ~a?" current-url-string))
                    (nyxt/mode/bookmark:bookmark-url :url current-url-string))))))

(defmethod nyxt:on-signal-load-finished ((mode bookmark-frequent-visits-mode) url)
  (bookmark-frequent-visit (threshold mode))
  url)
