;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

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

(defmethod store ((profile data-profile) (path annotations-data-path) &key &allow-other-keys)
  "Store the annotations to the buffer `annotations-path'."
  (with-data-file (file path :direction :output)
    (%set-data path (get-data path))
    (s-serialization:serialize-sexp (get-data path) file))
  t)

(defmethod restore ((profile data-profile) (path annotations-data-path) &key &allow-other-keys)
  "Restore the bookmarks from the buffer `annotations-path'."
  (handler-case
      (let ((data (with-data-file (file path)
                    (when file
                      (s-serialization:deserialize-sexp file)))))
        (when data
          (%set-data path data)))
    (error (c)
      (echo-warning "Failed to load annotations from ~s: ~a" (expand-path path) c))))

(defun annotation-add (annotation)
  (with-data-access (annotations (annotations-path (current-buffer)))
    (push annotation annotations)))

(defun annotations ()
  (with-data-access (annotations (annotations-path (current-buffer)))
    annotations))

(define-command annotate-current-url (&optional (buffer (current-buffer)))
  "Create a annotation of the URL of BUFFER."
  (let* ((data (first (prompt
                       :prompt "Annotation"
                       :sources (list (make-instance 'prompter:raw-source
                                                     :name "Note")))))
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
           (data (first (prompt
                         :prompt "Annotation"
                         :sources (list (make-instance 'prompter:raw-source
                                                       :name "Note")))))
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

(defun render-annotations (annotations)
  (with-current-html-buffer (buffer "*Annotations*" 'base-mode)
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 "Annotations")
      (loop for annotation in annotations
            collect (:div (:raw (render annotation))
                          (:hr))))))

(define-command show-annotations-for-current-url (&optional (source-buffer (current-buffer)))
  "Create a new buffer with the annotations of the current URL of BUFFER."
  (with-data-access (annotations (annotations-path (current-buffer)))
    (let ((filtered-annotations (remove-if-not (lambda (i) (url-equal (quri:uri (url i)) (url source-buffer))) annotations)))
      (render-annotations filtered-annotations))))

(define-class annotation-source (prompter:source)
  ((prompter:name "Annotations")
   (prompter:constructor (get-data (annotations-path (current-buffer))))
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
    (with-data-unsafe (annotations (annotations-path (current-buffer)))
      (sort (remove-duplicates
             (apply #'append (mapcar #'tags annotations))
             :test #'string-equal)
            #'string-lessp))))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-command show-annotation ()
  "Show an annotation(s)."
  (let ((selected-annotations
          (prompt
           :prompt "Show annotation(s)"
           :sources (make-instance 'annotation-source
                                   :actions nil))))
    (render-annotations selected-annotations)))

(define-command show-annotations ()
  "Show all annotations"
  (with-data-access (annotations (annotations-path (current-buffer)))
    (render-annotations annotations)))
