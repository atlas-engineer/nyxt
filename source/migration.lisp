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
    :type (or cons string function)
    :documentation "Suggestion how to update the symbols.
It can be initialized with a string or a form; if the latter, it's automatically
passed to `spinneret' on render.
This is useful to delay the evaluation of the tip until it's rendered."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:documentation "Representation of migration suggestion.
`symbols' are the offending ones deprecated in `version'."))

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
    (sort (delete-duplicates result)
          #'string<
          :key (compose #'first #'uiop:ensure-list #'symbols))))

(defmethod tip :around ((suggestion suggestion))
  (let ((value (call-next-method)))
    (if (stringp value)
        value
        (funcall value))))

(defmethod render-version-migration (major-version)
  (spinneret:with-html-string
    (:div
     (:h2 "For major version " major-version)
     (:table
      (:tr
       (:th "Concerned symbols")
       (:th "Suggestion"))
      (dolist (suggestion (version-suggestions major-version))
        (:tr
         (:td (format nil "~{~a~^, ~}" (symbols suggestion)))
         (:td (:raw (tip suggestion)))))))))

(define-internal-page-command-global migration-guide ()
    (buffer "*Migration guide*")
  "Display a guide to migrate configurations and third-party extensions across
major versions."
  (spinneret:with-html-string
    (:h1 "Migration guide")
    (:p "See also the " (:a :href (nyxt-url 'changelog) (:code "changelog")) ".")
    (:raw (render-version-migration (write-to-string (nyxt::version))))))


(export-always 'find-suggestions)
(defun find-suggestions (string)
  "Find the migration suggestions that match the symbol from STRING."
  (alex:when-let ((sym (ignore-errors (uiop:safe-read-from-string
                                       string :package (find-package :nyxt)))))
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
  (%slot-default)
  (:p "Use " (:nxref :variable '%slot-default%) " or " (:nxref :variable '%slot-value%) " instead.")

  (download-directory)
  (:p (:nxref :slot 'download-directory :class-name 'context-buffer)
      " is in " (:nxref :class-name 'context-buffer) ".")

  (history-file)
  (:p (:nxref :slot 'history-file :class-name 'context-buffer)
      " is in " (:nxref :class-name 'context-buffer) ".")

  (standard-output-file error-output-file)
  (:p (:nxref :variable 'nyxt:*log-file*))

  (annotations-file)
  (:p (:nxref :slot 'nyxt/mode/annotate:annotations-file :class-name 'nyxt/mode/annotate:annotate-mode)
      " is in " (:nxref :class-name 'nyxt/mode/annotate:annotate-mode) ".")

  (auto-mode-rules-file)
  (:p (:code "auto-rules-file") "(previously " (:code "auto-mode-rules-file")
      " is in " (:nxref :class-name 'modable-buffer)
      ", like all the other mode-related settings from the deprecated auto-mode.")

  (bookmarks-file)
  (:p (:nxref :slot 'nyxt/mode/bookmark:bookmarks-file :class-name 'nyxt/mode/bookmark:bookmark-mode)
      " is in " (:nxref :class-name 'nyxt/mode/bookmark:bookmark-mode) ".")

  (expand-path)
  (:p (:code "expand-path") " is replaced by " (:nxref :function 'files:expand) ".")

  (get-data get-user-data)
  (:p (:code "get-data") " and " (:code "get-user-data") " are replaced by "
      (:nxref :function 'files:content) ".")

  (with-data-access with-data-unsafe)
  (:p (:code "with-data-access") " and " (:code "with-data-unsafe")
      " are replaced by " (:code "files:with-file-content") ".")

  (copy-password copy-password-prompt-details save-new-password copy-username)
  (:p (:code "copy-password") ", "
      (:code "copy-password-prompt-details") ", "
      (:code "save-new-password") "  and "
      (:code "copy-username")
      " have been moved to the " (:code "nyxt/mode/password") " mode package.")

  (session-restore-prompt)
  (:p (:code "session-restore-prompt") " is now "
      (:code "restore-session-on-startup-p") " and is a boolean.")

  (scheme-keymap)
  (:p (:code "scheme-keymap") " is now "
      (:code "get-keymap") ".")

  (peval pflet)
  (:p "Those are renamed to " (:nxref :macro 'ps-eval) " and "
      (:nxref :macro 'ps-labels) " respectively.")

  (*after-init-hook* *after-startup-hook*)
  (:p "Those are " (:nxref :slot 'after-init-hook :class-name 'browser) " and "
      (:nxref :slot 'after-startup-hook :class-name 'browser) " now.")

  (auto-follow-hints-p compute-hints-in-view-port-p fit-to-prompt-p)
  (:p "Deprecated in favor of "
      (:nxref :slot 'nyxt/mode/hint:hinting-type :class-name 'nyxt/mode/hint:hint-mode)
      ".")

  nyxt/annotate-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/annotate) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/autofill-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/autofill) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/blocker-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/blocker) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/bookmark-frequent-visits
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/bookmark-frequent-visits) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/bookmark-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/bookmark) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/bookmarklets-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/bookmarklets) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/buffer-listing-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/buffer-listing) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/certificate-exception-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/certificate-exception) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/cruise-control-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/cruise-control) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/document-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/document) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/download-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/download) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/editor-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/editor) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/emacs-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/emacs) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/expedition-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/expedition) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/file-manager-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/file-manager) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/force-https-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/force-https) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/help-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/help) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/hint-prompt-buffer-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/hint-prompt-buffer) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/hint-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/hint) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/history-tree-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/history-tree) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/history-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/history) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/input-edit-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/input-edit) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/keyscheme-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/keyscheme) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/list-history-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/list-history) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/macro-edit-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/macro-edit) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/message-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/message) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/no-image-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/no-image) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/no-procrastinate-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/no-procrastinate) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/no-script-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/no-script) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/no-sound-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/no-sound) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/no-webgl-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/no-webgl) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/passthrough-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/passthrough) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/password-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/password) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/preview-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/preview) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/process-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/process) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/prompt-buffer-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/prompt-buffer) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/proxy-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/proxy) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/reading-line-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/reading-line) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/record-input-field-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/record-input-field) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/reduce-bandwidth-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/reduce-bandwidth) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/reduce-tracking-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/reduce-tracking) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/remembrance-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/remembrance) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/repeat-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/repeat) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/repl-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/repl) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/search-buffer-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/search-buffer) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/small-web-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/small-web) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/spell-check-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/spell-check) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/style-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/style) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/tts-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/tts) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/user-script-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/user-script) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/vi-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/vi) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/visual-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/visual) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") ".")

  nyxt/watch-mode
  (:p "Renamed to " (:nxref :class-name 'nyxt/mode/watch) "."
      " Query replace the regexp "
      (:code "nyxt/\([a-z-]*\)-mode") " with " (:code "nyxt/mode/\\1") "."))

