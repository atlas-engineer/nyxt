;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class history-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"history/default")
   (files:name "history"))
  (:export-class-name-p t))

(define-class history-entry ()
  ((url
    (quri:uri "")
    :writer nil
    :type (or quri:uri string))
   (title ""))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Entry for the global history."))

(defmethod (setf url) (value (he history-entry))
  (setf (slot-value he 'url) (url value)))

(defmethod prompter:object-attributes
    ((entry history-entry) (source prompter:source))
  (declare (ignore source))
  `(("Title" ,(title entry) (:width 3))
    ("URL" ,(render-url (url entry)) (:width 2))))

(export-always 'equals)
(defmethod equals ((e1 history-entry) (e2 history-entry))
  (quri:uri= (url e1) (url e2)))

(defmethod url ((he history-entry))
  "This accessor ensures we always return a `quri:uri'.
This is useful in cases the URL is originally stored as a string (for instance
when deserializing a `history-entry')."
  (unless (quri:uri-p (slot-value he 'url))
    (setf (slot-value he 'url) (url (slot-value he 'url))))
  (slot-value he 'url))

(defmethod serialize-history-entry ((entry history-entry) stream)
  (unless (url-empty-p (url entry))
    (flet ((write-slot (slot)
             (let ((entry-slot (funcall slot entry)))
               (unless (str:emptyp entry-slot)
                 (format t " :~a ~s"
                         (str:downcase slot)
                         entry-slot)))))
      (let ((*standard-output* stream))
        (write-string "(")
        (write-string ":url ")
        (format t "~s" (render-url (url entry)))
        (write-slot 'title)
        (write-string ")")))))

(defmethod files:serialize
    ((profile nyxt-profile) (file history-file) stream &key)
  (let ((entries
          (let ((content (files:content file)))
            (cond
              (*browser*
               (bt:with-recursive-lock-held ((history-lock *browser*))
                 (copy-seq (if (and (zerop (length (history-vector *browser*)))
                                    content)
                               content
                               (history-vector *browser*)))))
              (content (copy-seq content))
              (t #())))))
    (uiop:with-safe-io-syntax (:package :nyxt)
      (write-string "(" stream)
      (sera:do-each (entry entries)
        (write-string +newline+ stream)
        (serialize-history-entry entry stream))
      (write-string +newline+ stream)
      (write-string ")" stream))))

(defmethod files:deserialize
    ((profile nyxt-profile) (path history-file) raw-content &key)
  (let* ((*package* (find-package :nyxt))
         (entries (safe-read raw-content nil nil)))
    (when (listp entries)
      (delete nil
              (mapcar (lambda (entry)
                        (when (and (listp entry) (getf entry :url))
                          (let ((parsed-url (handler-case (quri:uri (getf entry :url))
                                              (quri:uri-error () nil))))
                            (when (and parsed-url (not (url-empty-p parsed-url)))
                              (make-instance 'history-entry
                                             :url parsed-url
                                             :title (if (stringp (getf entry :title))
                                                        (getf entry :title)
                                                        ""))))))
                      entries)))))
