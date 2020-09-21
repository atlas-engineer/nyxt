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
               (loop for message in (ql:system-list)
                     collect (markup:markup (:div
                                             (:p message)))))))
           (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                        (ps:lisp content)))))
      (ffi-buffer-evaluate-javascript buffer insert-content))
    (set-current-buffer buffer)
    buffer))
