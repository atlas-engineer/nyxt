;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class annotations-file (nfiles:data-file nyxt-lisp-file)
  ((nfiles:base-path "annotations-file")
   (nfiles:name "bookmarks"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class annotation ()
  ((data
    ""
    :documentation "The annotation data.")
   (tags
    '()
    :type list-of-strings)
   (date (local-time:now)))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class url-annotation (annotation)
  ((url nil)
   (page-title ""))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class snippet-annotation (url-annotation)
  ((snippet nil :documentation "The snippet of text being annotated."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod render ((annotation url-annotation))
  (spinneret:with-html-string
    (:p (:b "URL: ") (render-url (url annotation)))
    (:p (:b "Title: ") (page-title annotation))
    (:p (:b "Annotation: ") (data annotation))
    (:p (:b "Tags: ") (format nil "~{~a ~}" (tags annotation)))))

(defmethod render ((annotation snippet-annotation))
  (spinneret:with-html-string
    (:p (:b "Snippet: ") (snippet annotation))
    (:p (:b "URL: ") (render-url (url annotation)))
    (:p (:b "Title: ") (page-title annotation))
    (:p (:b "Annotation: ") (data annotation))
    (:p (:b "Tags: ") (format nil "~{~a ~}" (tags annotation)))))

(defun annotation-add (annotation)
  (nfiles:with-file-content (annotations (annotations-file (current-buffer)))
    (push annotation annotations)))

(defun annotations ()
  (nfiles:content (annotations-file (current-buffer))))

(define-command annotate-current-url (&optional (buffer-id (id (current-buffer))))
  "Create a annotation of the URL of buffer with BUFFER-ID."
  (let* ((buffer (buffers-get buffer-id))
         (data (prompt1
                    :prompt "Annotation"
                    :sources (list (make-instance 'prompter:raw-source
                                                  :name "Note"))))
         (tags (prompt
                :prompt "Tag(s)"
                :sources (list (make-instance 'prompter:word-source
                                              :name "New tags"
                                              :multi-selection-p t)
                               (make-instance 'keyword-source :buffer buffer)
                               (make-instance 'annotation-tag-source))))
         (annotation (make-instance 'url-annotation
                                    :url (url buffer)
                                    :data data
                                    :page-title (title buffer)
                                    :tags tags)))
    (annotation-add annotation)))

(define-command annotate-highlighted-text (&optional (buffer (current-buffer)))
  "Create a annotation for the highlighted text of BUFFER."
  (with-current-buffer buffer
    (let* ((snippet (%copy))
           (data (prompt1
                   :prompt "Annotation"
                   :sources (list (make-instance 'prompter:raw-source
                                                 :name "Note"))))
           (tags (prompt
                  :prompt "Tag(s)"
                  :sources (list (make-instance 'prompter:word-source
                                                :name "New tags"
                                                :multi-selection-p t)
                                 (make-instance 'keyword-source :buffer buffer)
                                 (make-instance 'annotation-tag-source))))
           (annotation (make-instance 'snippet-annotation
                                      :snippet snippet
                                      :url (url buffer)
                                      :page-title (title buffer)
                                      :data data
                                      :tags tags)))
      (annotation-add annotation))))

(defun render-annotations (&key annotations)
  "Show the ANNOTATIONS in a new buffer"
  (spinneret:with-html-string
    (loop for annotation in annotations
          collect (:div (:raw (render annotation))
                        (:hr)))))

(define-internal-page-command-global show-annotations-for-current-url
    (&key (source-buffer-id (id (current-buffer))))
    (buffer "*Annotations*" 'base-mode)
  "Create a new buffer with the annotations of the current URL of BUFFER."
  (let ((annotations (nfiles:content (annotations-file buffer))))
    (let ((filtered-annotations (remove-if-not (lambda (i)
                                                 (url-equal (quri:uri (url i)) (url (buffers-get source-buffer-id))))
                                               annotations)))
      (render-annotations :annotations filtered-annotations))))

(define-class annotation-source (prompter:source)
  ((prompter:name "Annotations")
   (prompter:constructor (get-data (annotations-file (current-buffer))))
   (prompter:multi-selection-p t)))

(define-class annotation-tag-source (prompter:source)
  ((prompter:name "Tags")
   (prompter:filter-preprocessor
    (lambda (initial-suggestions-copy source input)
      (prompter:delete-inexact-matches
       initial-suggestions-copy
       source
       (last-word input))))
   (prompter:filter
    (lambda (suggestion source input)
      (prompter:fuzzy-match suggestion source (last-word input))))
   (prompter:multi-selection-p t)
   (prompter:constructor
    (let ((annotations (nfiles:content (annotations-file (current-buffer)))))
      (sort (remove-duplicates
             (apply #'append (mapcar #'tags annotations))
             :test #'string-equal)
            #'string-lessp))))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-internal-page-command-global show-annotation ()
    (buffer "*Annotations*" 'base-mode)
  "Show an annotation(s)."
  (let ((selected-annotations
          (prompt
           :prompt "Show annotation(s)"
           :sources (make-instance 'annotation-source
                                   :actions nil))))
    (render-annotations :annotations selected-annotations)))

(define-internal-page-command-global show-annotations ()
    (buffer "*Annotations*" 'base-mode)
  "Show all annotations"
  (let ((annotations (nfiles:content (annotations-file buffer))))
    (render-annotations :annotations annotations)))
