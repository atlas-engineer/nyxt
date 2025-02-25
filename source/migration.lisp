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
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (symbols object))))

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
  (when-let ((sym (ignore-errors
                   (uiop:safe-read-from-string string
                                               :package (find-package :nyxt)))))
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

(define-migration "4"
  (modifier-translator)
  (:p "See slot " (:code "modifier-plist") ".")

  (ffi-buffer-make)
  (:p "Use " (:code "(make-instance 'buffer)") " instead.")

  (ffi-window-make)
  (:p "Use " (:code "(make-instance 'window)") " instead.")

  (search-engines)
  (:p "Moved to " (:nxref :slot 'search-engines :class-name 'browser) ".")

  (search-auto-complete-p)
  (:p "Renamed and moved to "
      (:nxref :slot 'search-engine-suggestions-p :class-name 'browser) ".")

  (search-always-auto-complete-p)
  (:p "Deleted.")

  (ffi-buffer-load-html)
  (:p "Deleted.")

  (override-map)
  (:p "Deleted.")

  (select-all)
  (:p "Deleted in favor of "
      (:nxref :command 'nyxt/mode/prompt-buffer:select-input-text) ".")

  (toggle-actions-on-current-suggestion-enabled)
  (:p "Deleted in favor of "
      (:nxref :command 'nyxt/mode/prompt-buffer:toggle-actions-on-current-suggestion)
      ".")

  (hide-single-source-header-p)
  (:p "Deleted.")

  (prompt-buffer-open-height)
  (:p "Deleted since " (:nxref :slot 'height :class-name 'prompt-buffer) " suffices.")

  (status-buffer-position)
  (:p "Deleted.")

  (hints-offset-x)
  (:p "Deleted in favor of "
      (:nxref :slot 'nyxt/mode/hint:x-translation :class-name 'nyxt/mode/hint:hint-mode) ".")

  (hints-offset-y)
  (:p "Deleted in favor of "
      (:nxref :slot 'nyxt/mode/hint:y-translation :class-name 'nyxt/mode/hint:hint-mode) ".")

  (message-buffer-height)
  (:p "Deleted in favor of " (:nxref :slot 'height :class-name 'message-buffer) ".")

  (message-buffer-style)
  (:p "Deleted in favor of " (:nxref :slot 'style :class-name 'message-buffer) ".")

  (ffi-window-message-buffer-height)
  (:p "Deleted since it is now handled by " (:nxref :function 'ffi-height) ".")

  (external-editor-program)
  (:p "No support for lists as a value.  Strings are the only valid values.")

  (conservative-history-movement-p)
  (:p "Deleted in favor of " (:nxref :slot 'global-history-p :class-name 'buffer) ".")

  (nyxt/mode/reduce-bandwidth
   nyxt/mode/editor
   nyxt/mode/plaintext-editor
   nyxt/mode/repl
   nyxt/mode/no-procrastinate
   nyxt/mode/preview
   nyxt/mode/record-input-field
   nyxt/mode/remembrance
   nyxt/mode/tts)
  (:p "Deleted modes.")

  (save-exact-modes-for-future-visits
   save-non-default-modes-for-future-visits
   editor-open-file
   editor-write-file
   copy-placeholder
   #:clean-configuration
   dashboard
   duplicate-buffer
   duplicate-buffer-with-current-modes
   follow-hint-with-current-modes-new-buffer
   force-reload-buffers
   go-next
   go-previous
   go-to-homepage
   go-up
   print-bindings
   reload-buffer
   reload-with-modes
   resume-prompt)
  (:p "Deleted commands."))
