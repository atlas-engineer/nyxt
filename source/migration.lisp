;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/migration
  (:documentation "Nyxt-specific DOM classes and functions operating on them."))
(in-package :nyxt/migration)

(defparameter +migration-suggestions+ (sera:dict)
  "Map between symbol names and `suggestion's.

We use symbol names because when an error occurs because of an unbound symbol,
we cannot predict the package in which it happened.")

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
    :export t
    :writer t
    :reader nil
    :type (or cons string function)
    :documentation "Suggestion how to update the symbols.
It can be initialized with a string or a form; if the latter, it's automatically
passed to `spinneret' on render.
This is useful to delay the evaluation of the tip until it's rendered."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod print-object ((object suggestion) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a" (symbols  object))))

(defun suggestion= (suggestion1 suggestion2)
  (and (string= (version suggestion1)
                (version suggestion2))
       (equal (symbols suggestion1)
              (symbols suggestion2))))

(defmethod initialize-instance :after ((suggestion suggestion) &key)
  (dolist (sym (symbols suggestion))
    (when (gethash (symbol-name sym) +migration-suggestions+)
      (alex:deletef (gethash (symbol-name sym) +migration-suggestions+)
                    suggestion
                    :test #'suggestion=))
    (push suggestion (gethash (symbol-name sym) +migration-suggestions+))))

(defun version-suggestions (major-version)
  "Return suggestions corresponding to MAJOR-VERSION.
Order is stable."
  (let ((result '()))
    (maphash (lambda (key suggestions)
               (declare (ignore key))
               (setf result (append (sera:keep major-version
                                               suggestions
                                               :key #'version
                                               :test #'string=)
                                    result)))
             +migration-suggestions+)
    (sort  (delete-duplicates result)
           #'string<
           :key (compose #'first #'uiop:ensure-list #'symbols))))

(defmethod tip ((suggestion suggestion))
  (if (stringp (slot-value suggestion 'tip))
      (slot-value suggestion 'tip)
      (funcall (slot-value suggestion 'tip))))

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


(export-always 'find-suggestions)
(defun find-suggestions (string)
  (alex:when-let ((sym (ignore-errors (uiop:safe-read-from-string string))))
    (gethash (symbol-name sym) +migration-suggestions+)))

(defmacro define-migration (major-version-string &body body)
  (let ((result '()))
    (cons 'progn
          (alex:doplist (symbols tip body result)
            (push `(make-instance 'suggestion
                                  :symbols (uiop:ensure-list ',symbols)
                                  :tip ,(if (stringp tip)
                                            tip
                                            `(lambda () (spinneret:with-html-string ,tip)))
                                  :version ,major-version-string)
                  result)))))

(define-migration "3"
  (download-directory)
  (:p (:code "download-directory") " is in " (:nxref "context-buffer") ".")

  (history-file)
  (:p (:code "history-file") " is in " (:nxref "context-buffer") ".")

  (standard-output-file standard-error-file)
  (:p (:nxref "standard-output-file") " and " (:nxref "standard-error-file")
      " are in " (:code "context-buffer") ".")

  (annotations-file)
  (:p (:nxref "annotations-file") " is in " (:nxref "nyxt/annotate-mode:annotate-mode") ".")

  (auto-mode-rules-file)
  (:p (:nxref "auto-mode-rules-file") " is in " (:nxref "nyxt/auto-mode:auto-mode") ".")

  (bookmarks-file)
  (:p (:nxref "bookmarks-file") " is in " (:nxref "nyxt/bookmark-mode:bookmark-mode") ".")

  (expand-path)
  (:p (:code "expand-path") " is replaced by " (:nxref "nfiles:expand") ".")

  (get-data get-user-data)
  (:p (:code "get-data") " and " (:code "get-user-data") " are replaced by " (:nxref "nfiles:content") ".")

  (with-data-access with-data-unsafe)
  (:p (:code "with-data-access") " and " (:code "with-data-unsafe") " are replaced by " (:nxref "nfiles:with-file-content") "."))
