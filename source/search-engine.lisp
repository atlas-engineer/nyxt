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
                 :type (or string null)
                 :documentation "The URL to fall back to when given an empty
query.  This is optional: if nil, use `search-url' instead with ~a expanded to
the empty string."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity))

(export-always 'make-search-engine)
(defun make-search-engine (shortcut search-url &optional (fallback-url ""))
  (make-instance 'search-engine
                 :shortcut shortcut
                 :search-url search-url
                 :fallback-url fallback-url))

(defmethod object-string ((engine search-engine))
  (shortcut engine))

(defmethod object-display ((engine search-engine))
  (format nil "~a~a ~a"
          (shortcut engine)
          (make-string (max 0 (- 10 (length (shortcut engine)))) :initial-element #\no-break_space)
          (search-url engine)))

(defun bookmark-search-engines (&optional (bookmarks (get-data (bookmarks-path
                                                                (current-buffer)))))
  (mapcar (lambda (b)
            (make-instance 'search-engine
                           :shortcut (shortcut b)
                           :search-url (if (quri:uri-scheme (quri:uri (search-url b)))
                                           (search-url b)
                                           (str:concat (object-display (url b)) (search-url b)))
                           :fallback-url (object-string (url b))))
          (remove-if (lambda (b) (or (str:emptyp (search-url b))
                                     (str:emptyp (shortcut b))))
                     bookmarks)))

(defun all-search-engines ()
  "Return the `search-engines' from the `browser' instance plus those in
bookmarks."
  (when *browser*
    (append (search-engines *browser*)
            (bookmark-search-engines))))

(defun default-search-engine (&optional (search-engines (all-search-engines)))
  "Return the search engine with the 'default' shortcut, or the first one if
there is none."
  (or (find "default"
            search-engines :test #'string= :key #'shortcut)
      (first search-engines)))
