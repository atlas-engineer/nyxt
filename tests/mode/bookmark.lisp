;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-bookmark-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (assert-true (enable-modes* 'nyxt/mode/bookmark:bookmark-mode buffer))
    (assert-true (disable-modes* 'nyxt/mode/bookmark:bookmark-mode buffer))))

(define-test persist-bookmark-creates-updates-and-skips-blank ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'modable-buffer))
         (file (make-instance 'nyxt/mode/bookmark:bookmarks-file
                              :profile (make-instance 'files:virtual-profile))))
    (enable-modes* 'nyxt/mode/bookmark:bookmark-mode buffer)
    (setf (nyxt/mode/bookmark:bookmarks-file
           (find-mode buffer 'nyxt/mode/bookmark:bookmark-mode))
          file)
    (nyxt/mode/bookmark:persist-bookmark buffer
                                         (quri:uri "https://example.com/a")
                                         :title "A"
                                         :tags '("" "zeta" "tag" "tag"))
    (let ((entries (files:content file)))
      (assert-eq 1 (length entries))
      (assert-string= "A" (title (first entries)))
      (assert-equal '("tag" "zeta") (nyxt/mode/bookmark:tags (first entries))))
    ;; Same host and path updates the existing entry, scheme included.
    (nyxt/mode/bookmark:persist-bookmark buffer
                                         (quri:uri "http://example.com/a")
                                         :title "B"
                                         :tags '("tag"))
    (let ((entries (files:content file)))
      (assert-eq 1 (length entries))
      (assert-string= "B" (title (first entries)))
      (assert-equal '("tag") (nyxt/mode/bookmark:tags (first entries))))
    (nyxt/mode/bookmark:persist-bookmark buffer (quri:uri "about:blank") :title "Nope")
    (assert-eq 1 (length (files:content file)))))

(define-test list-bookmarks-follows-the-file ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'web-buffer))
         (file (make-instance 'nyxt/mode/bookmark:bookmarks-file
                              :profile (make-instance 'files:virtual-profile))))
    (enable-modes* 'nyxt/mode/bookmark:bookmark-mode buffer)
    (setf (nyxt/mode/bookmark:bookmarks-file
           (find-mode buffer 'nyxt/mode/bookmark:bookmark-mode))
          file)
    (enable-modes* 'nyxt/mode/bookmark:bookmark-listing-mode buffer)
    (let ((mode (find-mode buffer 'nyxt/mode/bookmark:bookmark-listing-mode)))
      (flet ((page ()
               (spinneret:with-html-string
                 (user-interface:to-html (nyxt/mode/bookmark::listing mode))))
             (tag ()
               (user-interface:tag (nyxt/mode/bookmark::listing mode))))
        (assert-eq :p (tag))
        (assert-true (search "No bookmarks" (page)))
        (nyxt/mode/bookmark:persist-bookmark buffer
                                             (quri:uri "https://example.com/a")
                                             :title "Alpha")
        (assert-eq :ul (tag))
        (assert-true (search "Alpha" (page)))
        (nyxt/mode/bookmark:persist-bookmark buffer
                                             (quri:uri "https://example.com/b")
                                             :title "Gamma"
                                             :tags '("news"))
        (let ((html (page)))
          (assert-eq :div (tag))
          (assert-true (search "Alpha" html))
          (assert-true (search "Gamma" html))
          (assert-true (search "news" html))
          (assert-true (search "Unsorted" html)))
        (nyxt/mode/bookmark:persist-bookmark buffer
                                             (quri:uri "https://example.com/a")
                                             :title "Beta")
        (let ((html (page)))
          (assert-true (search "Beta" html))
          (assert-false (search "Alpha" html))
          (assert-true (search "Gamma" html)))
        (nyxt/mode/bookmark:delete-bookmark
         buffer
         '("https://example.com/a" "https://example.com/b"))
        (assert-eq :p (tag))
        (assert-true (search "No bookmarks" (page)))))))

(define-test bookmark-row-in-a-panel-opens-the-page ()
  (let* ((*browser* (make-instance 'browser))
         (entry (make-instance 'nyxt/mode/bookmark:bookmark-entry
                               :url (quri:uri "https://example.com/a")
                               :title "Alpha"))
         (panel (make-instance 'panel-buffer))
         (page (make-instance 'web-buffer))
         (panel-html (spinneret:with-html-string
                       (user-interface:to-html
                        (nyxt/mode/bookmark::bookmark-row entry panel))))
         (page-html (spinneret:with-html-string
                      (user-interface:to-html
                       (nyxt/mode/bookmark::bookmark-row entry page)))))
    (assert-true (search "Alpha" panel-html))
    (assert-true (search "href=#" panel-html))
    (assert-true (search "open-bookmark" panel-html))
    (assert-true (search "href=\"https://example.com/a\"" page-html))
    (assert-false (search "open-bookmark" page-html))
    (assert-true (search "edit" panel-html))
    (assert-true (fboundp 'nyxt/mode/bookmark:add-bookmark-panel-left))
    (assert-true (fboundp 'nyxt/mode/bookmark:add-bookmark-panel-right))))

(define-test list-bookmarks-drops-its-handler-when-the-buffer-is-deleted ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'web-buffer))
         (file (make-instance 'nyxt/mode/bookmark:bookmarks-file
                              :profile (make-instance 'files:virtual-profile))))
    (enable-modes* 'nyxt/mode/bookmark:bookmark-mode buffer)
    (setf (nyxt/mode/bookmark:bookmarks-file
           (find-mode buffer 'nyxt/mode/bookmark:bookmark-mode))
          file)
    (enable-modes* 'nyxt/mode/bookmark:bookmark-listing-mode buffer)
    (let ((mode (find-mode buffer 'nyxt/mode/bookmark:bookmark-listing-mode)))
      (nyxt::buffer-delete (id buffer))
      (nyxt/mode/bookmark:persist-bookmark buffer
                                           (quri:uri "https://example.com/a")
                                           :title "Alpha")
      (assert-eq :p (user-interface:tag (nyxt/mode/bookmark::listing mode)))
      (assert-false (search "Alpha"
                            (spinneret:with-html-string
                              (user-interface:to-html
                               (nyxt/mode/bookmark::listing mode))))))))
