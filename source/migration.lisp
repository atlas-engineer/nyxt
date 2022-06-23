;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/migration
  (:documentation "Nyxt-specific DOM classes and functions operating on them."))
(in-package :nyxt/migration)

(defparameter +migration-guides+ (sera:dict)
  "Map between major series (strings) and the association HTML documentation.")

(define-class guide-entry ()
  ((symbols
    '()
    :type (maybe (cons symbol *))
    :documentation "List of symbols concerned by this tip.")
   (tip
    ""
    :type string
    :documentation "Suggestion how to update the symbols."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

;; (defmethod initialize-instance :around ((entry guide-entry) &key symbols tip &allow-other-keys)
;;   (call-next-method :symbols (uiop:ensure-list symbols)
;;                     :tip tip))

(defun make-entry (symbols tip)
  (make-instance 'guide-entry :symbols (uiop:ensure-list symbols) :tip tip))

(define-class guide ()
  ((from-version
    ""
    :type string
    :documentation "Past major version concerned by the guide.")
   (to-version
    ""
    :type string
    :documentation "Target major version concerned by the guide.")
   (tips
    '()
    :type (maybe (cons guide-entry *))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod initialize-instance :after ((guide guide) &key)
  (setf (gethash (cons (from-version guide) (to-version guide))
                 +migration-guides+)
        guide ))


(defun make-migration (from-version to-version tips)
  "TIPS are in the form:

\((SYMBOLS1 TIP1)
 (SYMBOLS2 TIP2)
 ...)"
  (make-instance
   'guide
   :from-version from-version
   :to-version to-version
   :tips (mapcar (lambda (args) (apply #'make-entry args))
                 tips)))

;; TODO: Use `resolve-backtick-quote-links' in


(defmethod render-migration ((guide guide))
  (spinneret:with-html-string
    (:div
     (:h2 "From " (from-version guide) " to " (to-version guide))
     (:table
      (:tr
       (:td "Concerned symbols")
       (:td "Suggestion"))
      (dolist (entry (tips guide))
        (:tr
         (:td (format nil "~{~a~^, ~}" (symbols entry)))
         (:td (:raw (tip entry)))))))))

(define-internal-page-command-global migration-guide ()
    (buffer "*Migration guide*")
  "Display a guide to migrate configurations and third-party extensions across
major versions."
  (spinneret:with-html-string
    (:style (style buffer))
    (:h1 "Migration guide")
    (:p "See also the " (:code "changelog") ".")
    (dolist (guide (alex:hash-table-values +migration-guides+))
      (:raw (render-migration guide)))))

(defmacro define-migration (from-version-string to-version-string &body body)
  `(make-migration ,from-version-string ,to-version-string
                   (list ,@(loop :for (syms tip) :on body :by #'cddr
                               :collect (list 'list `',syms `(spinneret:with-html-string ,tip))))))


(define-migration "2" "3"
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
