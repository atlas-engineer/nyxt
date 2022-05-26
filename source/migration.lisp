;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/migration
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:documentation "Nyxt-specific DOM classes and functions operating on them."))
(in-package :nyxt/migration)
(use-nyxt-package-nicknames)

(defparameter +migration-guides+ (sera:dict)
  "Map between major series (strings) and the association HTML documentation.")

(defmacro define-migration (from-version-string to-version-string &body body)
  `(setf (gethash (cons ,from-version-string ,to-version-string) +migration-guides+)
         (spinneret:with-html-string
           (:div (:h2 "From " ,from-version-string " to " ,to-version-string)
                 ,@body))))

(define-internal-page-command-global migration-guide ()
    (buffer "*Migration guide*")
  "Display a guide to migrate configurations and third-party extensions across
major versions."
  (spinneret:with-html-string
    (:style (style buffer))
    (:h1 "Migration guide")
    (:p "See also the " (:code "changelog") ".")
    (dolist (guide (alex:hash-table-values +migration-guides+))
      (:raw guide))))

(define-migration "2" "3"
  (:ul
   (:li "Some file slots have been moved:"
        (:ul
         (:li (:code "download-directory") " is in " (:code "context-buffer") ".")
         (:li (:code "history-file") " is in " (:code "context-buffer") ".")
         (:li (:code "standard-output-file") " is in " (:code "context-buffer") ".")
         (:li (:code "standard-error-file") " is in " (:code "context-buffer") ".")
         (:li (:code "annotations-file") " is in " (:code "nyxt/annotate-mode:annotate-mode") ".")
         (:li (:code "auto-mode-rules-file") " is in " (:code "nyxt/auto-mode:auto-mode") ".")
         (:li (:code "bookmarks-file") " is in " (:code "nyxt/bookmark-mode:bookmark-mode") ".")))
   (:li "File path expansion and content retrieval has changed:"
        (:ul
         (:li (:code "expand-path") " is replaced by " (:code "nfiles:expand") ".")
         (:li (:code "get-data") " and " (:code "get-user-data") " are replaced by " (:code "nfiles:content") ".")
         (:li (:code "with-data-access") " and " (:code "with-data-unsafe") " are replaced by " (:code "nfiles:with-file-content") ".")
         ))))
