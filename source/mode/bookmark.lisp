;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nyxt/bookmark-mode
  (:use #:cl #:nyxt)
  (:import-from #:serapeum #:export-always)
  (:documentation "Bookmark buffer mode and command"))
(in-package :nyxt/bookmark-mode)

(export-always 'bookmark-mode)
(define-mode bookmark-mode ()
  "Mode for the bookmarks buffer."
  ((rememberable-p nil)
   (style (theme:themed-css (theme *browser*)
            ("summary"
             :background-color theme:secondary
             :color            theme:background
             :font-size        "14px"
             :padding          "16px"
             :margin           "6px"
             :width            "100%"
             :border           "none"
             :outline          "none"
             :text-align       "left")))))

(defun group-bookmarks (buffer)
  (let ((bookmarks-table (make-hash-table :test #'equalp))
        (bookmarks (nfiles:content (bookmarks-file buffer))))
    (dolist (bookmark bookmarks)
      (let ((tags (tags bookmark)))
        (if tags
            (dolist (tag tags)
              (push bookmark (gethash tag bookmarks-table nil)))
            (push bookmark (gethash tags bookmarks-table nil)))))
    bookmarks-table))

(export-always 'list-bookmarks)
(define-internal-page-command-global list-bookmarks ()
    (bookmarks-buffer "*Bookmarks*" 'bookmark-mode)
  "List all bookmarks in a new buffer."
  (let ((bookmarks (group-bookmarks bookmarks-buffer)))
    (spinneret:with-html-string
      (:style (style (find-mode bookmarks-buffer 'bookmark-mode)))
      (:h1 "Bookmarks")
      (:body
       (if (zerop (hash-table-count bookmarks))
           (format nil "No bookmarks in ~s." (nfiles:expand (bookmarks-file bookmarks-buffer)))
           (maphash
            (lambda (tag bookmarks)
              (:details
               (:summary (or tag "Unsorted"))
               (dolist (bookmark bookmarks)
                 (let ((url-display (render-url (url bookmark)))
                       (url-href (render-url (url bookmark))))
                   (:div :class "bookmark-entry"
                         (:p (:b "Title: ") (title bookmark))
                         (:p (:b "URL: ") (:a :href url-href
                                              url-display))
                         (:p (:b "Tags: ")
                             (when (tags bookmark)
                               (format nil " (~{~a~^, ~})" (tags bookmark))))
                         (:p (:button :class "button"
                                      :onclick
                                      (ps:ps
                                        (let ((section (ps:chain document active-element
                                                                 (closest ".bookmark-entry"))))
                                          (ps:chain section parent-node (remove-child section))
                                          (nyxt/ps:lisp-eval
                                           `(nyxt::delete-bookmark ,url-href))))
                                      "Delete"))
                         (:hr ""))))))
            bookmarks))))))
