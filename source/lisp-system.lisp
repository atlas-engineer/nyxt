;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-internal-page-command-global list-systems ()
    (buffer "*Systems*")
  "List systems available via Quicklisp."
  (spinneret:with-html-string
    (:style (style buffer))
    (:h1 "Systems")
    (:p "Listing of all available Quicklisp systems.")
    (:body
     (loop for system in (ql:system-list)
           collect
           (let ((name (ql-dist:short-description system))
                 (size (format nil "~a" (ql-dist:archive-size (ql-dist:preference-parent system))))
                 (dependencies (format nil "~a" (ql-dist:required-systems system))))
             (:div
              (:h2 name)
              (:p "Size: " size)
              (:p "Requires: " dependencies)
              (:p (:button :class "button"
                           :onclick (ps:ps (nyxt/ps:lisp-eval (:title "quickload") (ql:quickload name))) "Load"))
              (:hr)))))))

(define-class quicklisp-source (prompter:source)
  ((prompter:name "Quicklisp systems")
   (prompter:constructor (mapcar #'ql-dist:short-description (ql:system-list)))
   (prompter:return-actions (lambda-command quickload* (systems)
                              (ql:quickload (first systems))))))

(define-command load-system ()
  "Load a system from Quicklisp."
  (prompt :prompt "Load system"
          :sources 'quicklisp-source))

(define-command add-distribution ()
  "Add a new Quicklisp distribution."
  (let ((url (prompt1
              :prompt "New distribution URL"
              :sources 'prompter:raw-source)))
    (ql-dist:install-dist url :prompt nil)))
