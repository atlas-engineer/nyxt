;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/migration
  (:documentation "Nyxt-specific DOM classes and functions operating on them."))
(in-package :nyxt/migration)

(defparameter +migration-suggestions+ (sera:dict)
  "Map between symbols and `suggestion's.")

(define-class suggestion ()
  ((symbols
    '()
    :type (maybe (cons symbol *))
    :documentation "List of symbols concerned by this tip.")
   (version
    nil
    :type (maybe string)
    :documentation "First major version concerned with the migration suggestion.
NIL means suggestion concerns all versions.")
   (tip
    ""
    :type string
    :documentation "Suggestion how to update the symbols."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defun suggestion= (suggestion1 suggestion2)
  (and (string= (version suggestion1)
                (version suggestion2))
       (equal (symbols suggestion1)
              (symbols suggestion2))))

(defmethod initialize-instance :after ((suggestion suggestion) &key)
  (dolist (sym (symbols suggestion))
    ;; TODO: Hash by symbol-name, since package may be missing?
    (when (gethash sym +migration-suggestions+)
      (alex:deletef (gethash sym +migration-suggestions+)
                    suggestion
                    :test #'suggestion=))
    (push suggestion (gethash sym +migration-suggestions+))))

(defun version-suggestions (major-version)
  (let ((result '()))
    (maphash (lambda (key suggestions)
               (declare (ignore key))
               (setf result (append (sera:keep major-version
                                               suggestions
                                               :key #'version
                                               :test #'string=)
                                    result)))
             +migration-suggestions+)
    (delete-duplicates result)))

(defmethod render-version-migration (major-version)
  (spinneret:with-html-string
    (:div
     (:h2 "For major version " major-version)
     (:table
      (:tr
       (:td "Concerned symbols")
       (:td "Suggestion"))
      (dolist (suggestion (version-suggestions major-version))
        (:tr
         (:td (format nil "~{~a~^, ~}" (symbols suggestion)))
         (:td (:raw (tip suggestion)))))))))

(define-internal-page-command-global migration-guide ()
    (buffer "*Migration guide*")
  "Display a guide to migrate configurations and third-party extensions across
major versions."
  (spinneret:with-html-string
    (:style (style buffer))
    (:h1 "Migration guide")
    (:p "See also the " (:code "changelog") ".")
    (:raw (render-version-migration (write-to-string (first (nyxt::version)))))))

;; TODO: Fix this, does not work!
(spinneret:deftag xref (symbol attr)
  `(:a :href (javascript-url
              (ps:ps (nyxt/ps:lisp-eval
                      (:title "describe-any")
                      (nyxt::describe-any (princ-to-string ,@symbol)))))
       (:code ,@attr ,@symbol)))


(export-always 'find-suggestions)
(defun find-suggestions (string)
  (gethash (alex:symbolicate (string-upcase string)) +migration-suggestions+))

(defmacro define-migration (major-version-string &body body)
  (let ((result '()))
    (cons 'progn
          (alex:doplist (symbols tip body result)
            (push `(make-instance 'suggestion
                                  :symbols (uiop:ensure-list ',symbols)
                                  :tip (spinneret:with-html-string ,tip)
                                  :version ,major-version-string)
                  result)))))

(define-migration "3"
  (download-directory)
  (:p (:code "download-directory") " is in " (:code "context-buffer") ".")

  (history-file)
  (:p (:code "history-file") " is in " (:code "context-buffer") ".")

  (standard-output-file standard-error-file)
  (:p (:code "standard-output-file") " and " (:code "standard-error-file")
      " are in " (:code "context-buffer") ".")

  (annotations-file)
  (:p (:code "annotations-file") " is in " (:code "nyxt/annotate-mode:annotate-mode") ".")

  (auto-mode-rules-file)
  (:p (:code "auto-mode-rules-file") " is in " (:code "nyxt/auto-mode:auto-mode") ".")

  (bookmarks-file)
  (:p (:code "bookmarks-file") " is in " (:code "nyxt/bookmark-mode:bookmark-mode") ".")

  (expand-path)
  (:p (:code "expand-path") " is replaced by " (:code "nfiles:expand") ".")

  (get-data get-user-data)
  (:p (:code "get-data") " and " (:code "get-user-data") " are replaced by " (:code "nfiles:content") ".")

  (with-data-access with-data-unsafe)
  (:p (:code "with-data-access") " and " (:code "with-data-unsafe") " are replaced by " (:code "nfiles:with-file-content") "."))
