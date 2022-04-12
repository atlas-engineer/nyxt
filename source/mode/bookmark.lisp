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
             :text-align       "left")
            ;; Taken from buffer.lisp to save space for big bookmark lists.
            (button
             :display "inline-block"
             :background-color theme:secondary
             :color theme:background
             :text-decoration "none"
             :border-radius "2px"
             :padding "6px"
             :margin-left "2px"
             :margin-right "2px")))))

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
      (:nstyle (style (find-mode bookmarks-buffer 'bookmark-mode)))
      (:h1 "Bookmarks")
      (cond
        ((zerop (hash-table-count bookmarks))
         (:p (format nil "No bookmarks in ~s." (nfiles:expand (bookmarks-file bookmarks-buffer)))))
        (t (maphash
            (lambda (tag bookmarks)
              (:details
               (:summary (or tag "Unsorted"))
               (dolist (bookmark bookmarks)
                 (let ((uri-host (quri:uri-host (url bookmark)))
                       (url-href (render-url (url bookmark))))
                   (:dl
                    (:dt (:button :onclick (ps:ps (delbkm (ps:lisp url-href))) "✕")
                         (serapeum:ellipsize (title bookmark) 80))
                    (:dd (:a :href url-href uri-host))
                    (when (tags bookmark)
                      (:dt "Tags")
                      (:dd (format nil " (~{~a~^, ~})" (tags bookmark)))))
                   (:hr)))))
            bookmarks)))
      (:nscript
       ;; Not exactly pretty, but saves a lot of space.
       (ps:ps (defun delbkm (url)
                (fetch (+ "lisp://" (escape (+ "(nyxt:delete-bookmark \"" url "\")/")))
                       (ps:create :mode "no-cors"))))))))
