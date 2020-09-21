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
               (loop for system in (ql:system-list)
                     collect (markup:markup (:div
                                             (:p "Name: "(ql-dist:short-description system))
                                             (:p "System file name: "(ql-dist:system-file-name system))
                                             (:p "Dependencies: " (format nil "~a" (ql-dist:required-systems system)))
                                             (:hr "")))))))
           (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                        (ps:lisp content)))))
      (ffi-buffer-evaluate-javascript buffer insert-content))
    (set-current-buffer buffer)
    buffer))
