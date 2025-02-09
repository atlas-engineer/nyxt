;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class history-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"history/default")
   (files:name "history"))
  (:export-class-name-p t))

(export-always 'buffer-history)
(defun buffer-history (&optional (buffer (current-buffer)))
  "Get the history of BUFFER.
Not modifiable."
  (files:content (history-file buffer)))

(define-class history-entry ()
  ((url
    (quri:uri "")
    :writer nil
    :type (or quri:uri string))
   (title "")
   (implicit-visits
    0
    :type integer
    :documentation "Number of times the URL was visited.")
   (scroll-position
    '()
    :type (list-of number)
    :documentation "The scroll position user was at when last visiting the page.
It's a list of a form (Y &OPTIONAL X)."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Entry for the global history."))

(defmethod (setf url) (value (he history-entry))
  (setf (slot-value he 'url) (url value)))

(defmethod prompter:object-attributes ((entry history-entry) (source prompter:source))
  (declare (ignore source))
  `(("Title" ,(title entry) (:width 3))
    ("URL" ,(render-url (url entry)) (:width 2))
    ("Visits" ,(implicit-visits entry) (:width 1))))

(export-always 'equals)
(defmethod equals ((e1 history-entry) (e2 history-entry))
  (quri:uri= (url e1) (url e2)))

(defmethod url ((he history-entry))
  "This accessor ensures we always return a `quri:uri'.
This is useful in cases the URL is originally stored as a string (for instance
when deserializing a `history-entry').

We can't use `initialize-instance :after' to convert the URL because
`s-serialization:deserialize-sexp' sets the slots manually after making the
class."
  (unless (quri:uri-p (slot-value he 'url))
    (setf (slot-value he 'url) (url (slot-value he 'url))))
  (slot-value he 'url))

(defmethod s-serialization::serialize-sexp-internal ((uri quri:uri)
                                                     stream
                                                     serialization-state)
  "Serialize `history-entry' by turning the URL and last access into strings."
  (declare (ignore serialization-state))
  (prin1 (quri:render-uri uri) stream))

(defmethod s-serialization::serialize-sexp-internal ((timestamp time:timestamp)
                                                     stream
                                                     serialization-state)
  "Serialize `history-entry' by turning the URL and last access into strings."
  (declare (ignore serialization-state))
  (prin1 (time:format-timestring nil timestamp :timezone time:+utc-zone+)
         stream))

(defun history-tree-key (history-entry)
  (url history-entry))

(defmethod files:serialize ((profile nyxt-profile) (file history-file) stream &key)
  (let ((*package* (find-package :nyxt))
        (*print-length* nil))
    ;; We need to make sure current package is :nyxt so that symbols are printed
    ;; with consistent namespaces.
    (write
     (with-input-from-string (in (with-output-to-string (out)
                                   (s-serialization:serialize-sexp
                                    (list +version+ (files:content file))
                                    out)))
       ;; We READ the output of serialize-sexp to make it more
       ;; human-readable.
       (safe-read in))
     :stream stream)))

;; REVIEW: This works around the issue of cl-prevalence to deserialize structs
;; with custom constructors: https://github.com/40ants/cl-prevalence/issues/16.
(setf (fdefinition 'quri.uri::make-uri) #'quri.uri::%make-uri)

(defun histories-directory (&optional (buffer (current-buffer)))
  "Get the directory where history files are stored, based on `history-file' of BUFFER."
  (when (context-buffer-p buffer)
    (files:parent (files:expand (history-file buffer)))))

(defun histories-list (&optional (buffer (current-buffer)))
  "List all the files with persisted history.
Uses `histories-directory' of the BUFFER to get files."
  (when-let ((dir (histories-directory buffer)))
    (sera:keep "lisp" (uiop:directory-files dir)
               :test 'string-equal
               :key #'files:pathname-type*)))

(define-class history-name-source (prompter:source)
  ((prompter:name "Histories")
   (prompter:constructor (mapcar #'pathname-name (histories-list)))))
