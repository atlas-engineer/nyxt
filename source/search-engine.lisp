;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class search-engine ()
  ((shortcut (error "Slot `shortcut' must be set")
             :type string
             :documentation "The word used to refer to the search engine, for
instance from the `set-url' commands.")
   (search-url (error "Slot `search-url' must be set")
               :type string
               :documentation "The URL containing a '~a' which will be replaced with the search query.")
   (fallback-url nil
                 :type (or null quri:uri)
                 :writer t
                 :documentation "The URL to fall back to when given an empty
query.  This is optional: if nil, use `search-url' instead with ~a expanded to
the empty string.")
   (completion-function nil
                        :type (or null function)
                        :documentation "A function taking a user input and returning a list of suggested search queries.
The list should contain either
- strings with text completions,
- or a list of form (TEXT URL), where
  - TEXT is completion text, and
  - URL is the string.

Simple completion functions can be built via `make-search-completion-function'"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod fallback-url ((engine search-engine))
  (or (slot-value engine 'fallback-url)
      (quri:uri (format nil (search-url engine) ""))))

(export-always 'make-search-engine)
(defun make-search-engine (shortcut search-url &optional fallback-url)
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
                                          (processing-function #'j:decode))
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
  `(("Shortcut" ,(shortcut engine))
    ("Search URL" ,(search-url engine) nil 3)))

(defun all-search-engines ()
  "Return the `search-engines' from the current buffer."
  (let* ((current-buffer (current-buffer))
         (buffer (or current-buffer
                     (make-instance 'context-buffer))))
    (unwind-protect
         (search-engines buffer)
      (unless current-buffer
        (buffer-delete buffer)))))

(defun default-search-engine (&optional (search-engines (all-search-engines)))
  "Return the last search engine of the SEARCH-ENGINES."
  (first (last search-engines)))

(define-class search-engine-source (prompter:source)
  ((prompter:name "Search Engines")
   (prompter:constructor (all-search-engines))))

(define-class search-engine-url-source (prompter:source)
  ((prompter:name "Search Engines")
   (prompter:constructor (delete nil (mapcar #'fallback-url (all-search-engines))))
   (prompter:enable-marks-p t)))

(define-command query-selection-in-search-engine (&key (query-in-new-buffer-p t))
  "Search selected text using the queried search engine."
  (let* ((selection (ffi-buffer-copy (current-buffer)))
         (engine (prompt1 :prompt "Search engine"
                          :sources 'search-engine-source))
         (target-buffer (if query-in-new-buffer-p
                            (make-buffer-focus)
                            (current-buffer))))
    (when engine
      (buffer-load (make-instance 'new-url-query :query selection :engine engine)
                   :buffer target-buffer))))
