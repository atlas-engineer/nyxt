;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class search-engine ()
  ((name nil
         :type (maybe string)
         :documentation "Human-readable name of the search engine, like \"Wikipedia\" or \"Searx\".
In case no name is defined, `shortcut' is used as the engine name.")
   (shortcut
    (alex:required-argument 'shortcut)
    :type string
    :documentation "The word or set of letters used to refer to the search engine.
For instance, `shortcut's are used in `set-url' commands.")
   (search-url
    (alex:required-argument 'search-url)
    :type string
    :documentation "Format string with an '~a' to be replaced with the search query.")
   (fallback-url
    nil
    :type (or null quri:uri)
    :writer t
    :documentation "The URL to fall back to when given an empty query.
This is optional: if NIL, use `search-url' instead with ~a expanded to the empty
string.")
   (completion-function
    nil
    :type (or null function)
    :documentation "A function taking a user input and returning a list of suggested search queries.
The list should contain either
- strings with text completion,
- or a list of form (TEXT URL), where
  - TEXT is completion text, and
  - URL is the string.

Simple completion functions can be built via `make-search-completion-function'"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "A representation of search engine, as used in Nyxt.

Minimal search engines can contain as little as `search-url' and `shortcut'.
More involved engines can have:
- Human-readable `name's.
- `fallback-url' as the search home/error page.
- And `completion-function' to list search suggestions in Nyxt
  prompts (`set-url' in particular).

For the actual uses and configuration of search engines, see:
- `search-engines'.
- `search-auto-complete-p'.
- `search-always-auto-complete-p'."))

(defmethod url ((object search-engine))
  (fallback-url object))

(defmethod fallback-url ((engine search-engine))
  (or (slot-value engine 'fallback-url)
      (quri:uri (format nil (search-url engine) ""))))

(export-always 'make-search-engine)
(defun make-search-engine (shortcut search-url &optional fallback-url)
  "Utility to create simple `search-engine's.
Sets `shortcut', `search-url', and `fallback-url' of the search engine to the
values of respective arguments."
  (make-instance 'search-engine
                 :shortcut shortcut
                 :search-url search-url
                 :fallback-url (ensure-url fallback-url)))

(export-always 'make-search-completion-function)
(-> make-search-completion-function
    (&key (:base-url string)
          (:request-function (function (string &rest list) t))
          (:request-args list)
          (:processing-function (function (t) (list-of string))))
    (function (string) (list-of string)))
(defun make-search-completion-function (&key base-url
                                          (request-function
                                           #'(lambda (url &rest args)
                                               (handler-case (apply #'dex:get url args)
                                                 (usocket:ns-host-not-found-error ()
                                                   (echo-warning "There's no Internet connection to make search completion")
                                                   nil))))
                                          request-args
                                          (processing-function (lambda (data) (coerce (j:decode data) 'list))))
  "Return a function suitable to be a `completion-function' of `search-engine'.

BASE-URL is a one-placeholder format string (e.g.,
\"https://duckduckgo.com/ac/?q=~a\") to request completions from.
REQUEST-FUNCTION is the function to make this request with. Defaults to
`dex:get'. Takes a URL built from user input and BASE-URL.
PROCESSING-FUNCTION is a function to process whatever the REQUEST-FUNCTION
returns. Should return a list of strings.

Example (Tor-proxied completion function for Wikipedia):
\(make-search-completion-function
 :base-url \"https://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=~a\"
 :processing-function (compose #'second #'njson:decode)
 :request-args '(:proxy \"socks5://localhost:9050\"))"
  #'(lambda (input)
      (funcall processing-function
               (apply request-function
                      (cons (format nil base-url (quri:url-encode input))
                            request-args)))))

(defmethod prompter:object-attributes ((engine search-engine) (source prompter:source))
  (declare (ignore source))
  `(("Name" ,(or (name engine) (shortcut engine)))
    ("Search URL" ,(search-url engine) nil 3)))

(defun all-search-engines ()
  "Return the `search-engine's from the `current-buffer'.
If there's no buffer, create a dummy one and get search engines from there."
  (let* ((current-buffer (current-buffer))
         (buffer (or current-buffer
                     (make-instance 'context-buffer))))
    (unwind-protect
         (search-engines buffer)
      (unless current-buffer
        (buffer-delete buffer)))))

(define-generic default-search-engine (&optional (search-engines (all-search-engines)))
  "Return the default search engine out of the SEARCH-ENGINES.
Right now default search engine is the last one."
  (first (last search-engines)))

(define-class search-engine-source (prompter:source)
  ((prompter:name "Search Engines")
   (prompter:constructor (all-search-engines))
   (prompter:filter-preprocessor #'prompter:filter-exact-matches))
  (:documentation "Source listing `all-search-engines' in the current buffer."))

(define-class search-engine-url-source (prompter:source)
  ((prompter:name "Search Engines")
   (prompter:constructor (delete nil (mapcar #'fallback-url (all-search-engines))))
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:enable-marks-p t))
  (:documentation "Source listing the `fallback-url's of all the search engines in the buffer."))

(define-command query-selection-in-search-engine (&key (query-in-new-buffer-p t))
  "Search selected text using the queried search engine.
QUERY-IN-NEW-BUFFER creates a new buffer with the search results."
  (let* ((selection (ffi-buffer-copy (current-buffer)))
         (engine (prompt1 :prompt "Search engine"
                          :sources 'search-engine-source))
         (target-buffer (if query-in-new-buffer-p
                            (make-buffer-focus)
                            (current-buffer))))
    (when engine
      (buffer-load (make-instance 'new-url-query :query selection :engine engine)
                   :buffer target-buffer))))
