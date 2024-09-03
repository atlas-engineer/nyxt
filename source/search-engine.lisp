;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class search-engine ()
  ((name
    (alex:required-argument 'name)
    :type string
    :documentation "Name of the search engine.")
   (shortcut
    (alex:required-argument 'shortcut)
    :type string
    :documentation "Alternative shorter name of the search engine.
Useful for commands such as `set-url', whose prompt buffer sources include
`url-or-query-source'.")
   ;; An alternative to control strings is to leverage `quri:uri-query-params'.
   (control-url
    (alex:required-argument 'control-url)
    :type string
    :documentation "Format string to request search queries.")
   (control-completion-url
    nil
    :type (maybe string)
    :documentation "Format string to request search query suggestions.
When nil, search suggestions aren't computed."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A representation of search engines.

See configuration slots `search-engines' and `search-engine-suggestions-p'."))

(defmethod print-object ((search-engine search-engine) stream)
  (print-unreadable-object (search-engine stream :type t)
    (format stream "~a" (name search-engine))))

(defmethod format-url (query (search-engine search-engine))
  (format nil (control-url search-engine) (quri:url-encode query)))

(defmethod format-completion-url (query (search-engine search-engine))
  (format nil (control-completion-url search-engine) (quri:url-encode query)))

(defmethod format-query (query (search-engine search-engine))
  (format nil "~a ~a" (shortcut search-engine) query))

(defmethod suggestions (query (search-engine search-engine))
  "Return a list of search suggestions based on QUERY."
  (unless (str:blankp query)
    (flet ((request (url) (j:decode (dex:get url))))
      (alex:switch ((control-completion-url search-engine) :test 'string=)
        ("https://duckduckgo.com/ac/?q=~a"
         (map 'list
              (lambda (hash-table) (first (alex:hash-table-values hash-table)))
              (request (format-completion-url query search-engine))))
        ("https://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=~a"
           (coerce (j:get 1 (request (format-completion-url query search-engine)))
                 'list))
        (t
         (log:debug "Search suggestions aren't supported for ~a." (name search-engine))
         nil)))))

(defmethod prompter:object-attributes ((engine search-engine) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(name engine) (:width 3))
    ("Shortcut" ,(shortcut engine) (:width 1))))

(define-class search-engine-source (prompter:source)
  ((prompter:name "Search engines")
   (prompter:constructor (search-engines *browser*))
   (prompter:filter-preprocessor #'prompter:filter-exact-matches))
  (:documentation "Source listing all the search engines."))

(define-command query-selection-in-search-engine (&key (query-in-new-buffer-p t))
  "Search selected text using the queried search engine.
When QUERY-IN-NEW-BUFFER-P is non-nil, open the results in a new buffer."
  (buffer-load (format-url (ffi-buffer-copy (current-buffer))
                           (prompt1 :prompt "Search engine"
                                    :sources 'search-engine-source))
               :buffer (if query-in-new-buffer-p (make-buffer-focus) (current-buffer))))
