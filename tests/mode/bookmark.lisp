;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-bookmark-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/bookmark:bookmark-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/bookmark:bookmark-mode buffer)))))

(define-test persist-bookmark-creates-updates-and-skips-blank ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'modable-buffer))
         (file (make-instance 'nyxt/mode/bookmark:bookmarks-file
                              :profile (make-instance 'files:virtual-profile))))
    (with-current-buffer buffer
      (enable-modes* 'nyxt/mode/bookmark:bookmark-mode buffer)
      (setf (nyxt/mode/bookmark:bookmarks-file
             (find-submode 'nyxt/mode/bookmark:bookmark-mode buffer))
            file)
      (nyxt/mode/bookmark:persist-bookmark (quri:uri "https://example.com/a")
                                           :title "A"
                                           :tags '("" "zeta" "tag" "tag"))
      (let ((entries (files:content file)))
        (assert-eq 1 (length entries))
        (assert-string= "A" (title (first entries)))
        (assert-equal '("tag" "zeta") (nyxt/mode/bookmark:tags (first entries))))
      ;; Same host and path updates the existing entry, scheme included.
      (nyxt/mode/bookmark:persist-bookmark (quri:uri "http://example.com/a")
                                           :title "B"
                                           :tags '("tag"))
      (let ((entries (files:content file)))
        (assert-eq 1 (length entries))
        (assert-string= "B" (title (first entries)))
        (assert-equal '("tag") (nyxt/mode/bookmark:tags (first entries))))
      (nyxt/mode/bookmark:persist-bookmark (quri:uri "about:blank") :title "Nope")
      (assert-eq 1 (length (files:content file))))))
