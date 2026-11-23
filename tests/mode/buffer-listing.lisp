;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-buffer-listing-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (assert-true (enable-modes* 'nyxt/mode/buffer-listing:buffer-listing-mode buffer))
    (assert-true (disable-modes* 'nyxt/mode/buffer-listing:buffer-listing-mode buffer))))

(define-test list-buffers-follows-the-buffers ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'web-buffer
                                :title "Alpha"
                                :url (quri:uri "https://example.com/a"))))
    (enable-modes* 'nyxt/mode/buffer-listing:buffer-listing-mode buffer)
    (let ((mode (find-mode buffer 'nyxt/mode/buffer-listing:buffer-listing-mode)))
      (flet ((page ()
               (spinneret:with-html-string
                 (user-interface:to-html
                  (nyxt/mode/buffer-listing::listing mode)))))
        (let ((html (page)))
          (assert-eq :ul (user-interface:tag
                          (nyxt/mode/buffer-listing::listing mode)))
          (assert-true (search "Alpha" html))
          (assert-true (search "https://example.com/a" html))
          (assert-true (search "close" html)))
        (handler-case (setf (title buffer) "Beta")
          (error ()))
        (let ((html (page)))
          (assert-true (search "Beta" html))
          (assert-false (search "Alpha" html)))
        (handler-case (setf (url buffer) (quri:uri "https://example.com/b"))
          (error ()))
        (assert-true (search "https://example.com/b" (page)))
        (let ((other (make-instance 'web-buffer :title "Gamma")))
          (assert-true (search "Gamma" (page)))
          (nyxt::buffer-delete (id other))
          (assert-false (search "Gamma" (page))))
        (assert-true (search "Beta" (page)))
        (disable-modes* 'nyxt/mode/buffer-listing:buffer-listing-mode buffer)
        (let ((after-disable (make-instance 'web-buffer :title "Delta")))
          (assert-false (search "Delta" (page)))
          (enable-modes* 'nyxt/mode/buffer-listing:buffer-listing-mode buffer)
          (assert-true (search "Delta" (page)))
          (nyxt::buffer-delete (id after-disable)))
        (let ((listing (nyxt/mode/buffer-listing::listing mode)))
          (user-interface:connect listing buffer)
          (clrhash (nyxt::lisp-url-callbacks buffer))
          (assert-eql 0 (hash-table-count (nyxt::lisp-url-callbacks buffer)))
          (user-interface:connect listing buffer)
          (assert-true (plusp (hash-table-count (nyxt::lisp-url-callbacks buffer)))))))))
