;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-command list-systems ()
  "List systems available."
  (let ((buffer (make-internal-buffer :title "*Systems*")))
    (let* ((content
             (markup:markup
              (:style (style buffer))
              (:h1 "Systems")
              (:body
               ;; temporarily limit to 100 elements
               (loop for system in (subseq (ql:system-list) 0 100)
                     collect
                        (let ((name (ql-dist:short-description system))
                              (url (ql-dist:archive-url (ql-dist:preference-parent system)))
                              (size (format nil "~a" (ql-dist:archive-size (ql-dist:preference-parent system))))
                              (dependencies (format nil "~a" (ql-dist:required-systems system))))
                          (markup:markup (:div
                                          (:p "Name: " name)
                                          (:p "URL: " (:a :href url url))
                                          (:p "Size: " size " bytes")
                                          (:p "Dependencies: " dependencies)
                                          (:p (:a :class "button"
                                                  :href (lisp-url `(ql:quickload ,name)) "Load"))
                                          (:hr ""))))))))
           (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                        (ps:lisp content)))))
      (ffi-buffer-evaluate-javascript buffer insert-content))
    (set-current-buffer buffer)
    buffer))
