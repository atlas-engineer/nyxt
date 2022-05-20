;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/annotate-mode
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:serapeum #:->)
  (:documentation "Mode to annotate documents.
Annotations are persisted to disk."))
(in-package :nyxt/annotate-mode)
(use-nyxt-package-nicknames)

(define-mode annotate-mode ()
  "Annotate document with arbitrary comments.
Annotations are persisted to disk, see the `annotations-file' mode slot."
  ((annotations-file
    (make-instance 'annotations-file)
    :type annotations-file
    :documentation "The file where to save annotations.")))

(defmethod annotations-file ((buffer buffer))
  (annotations-file (find-submode 'annotate-mode buffer)))

(define-class annotations-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"annotations")
   (files:name "annotations"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class annotation ()
  ((data
    ""
    :export nil
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
  (files:with-file-content (annotations (annotations-file (current-buffer)))
    (push annotation annotations)))

(defun annotations ()
  (files:content (annotations-file (current-buffer))))

(define-command annotate-current-url (&optional (buffer (current-buffer)))
  "Create an annotation of the URL of BUFFER."
  (let* ((data (prompt1
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
  "Create an annotation for the highlighted text of BUFFER."
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
    (&key (source-buffer (current-buffer)))
    (buffer "*Annotations*" 'base-mode)
  "Create a new buffer with the annotations of the current URL of BUFFER."
  (let ((annotations (files:content (annotations-file buffer))))
    (let ((filtered-annotations (remove-if-not (lambda (i)
                                                 (url-equal (quri:uri (url i)) (url source-buffer)))
                                               annotations)))
      (render-annotations :annotations filtered-annotations))))

(define-class annotation-source (prompter:source)
  ((prompter:name "Annotations")
   (prompter:constructor (files:content (annotations-file (current-buffer))))
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
    (let ((annotations (files:content (annotations-file (current-buffer)))))
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
                                   :return-actions nil))))
    (render-annotations :annotations selected-annotations)))

(define-internal-page-command-global show-annotations ()
    (buffer "*Annotations*" 'base-mode)
  "Show all annotations"
  (let ((annotations (files:content (annotations-file buffer))))
    (render-annotations :annotations annotations)))
