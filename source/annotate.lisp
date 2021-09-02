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
  ((url nil))
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
    (:p (:b "Annotation: ") (data annotation))))

(defmethod render ((annotation snippet-annotation))
  (spinneret:with-html-string
    (:p (:b "Snippet: ") (snippet annotation))
    (:p (:b "URL: ") (render-url (url annotation)))
    (:p (:b "Annotation: ") (data annotation))))

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
                       :sources (list (make-instance 'prompter:raw-source)))))
         (annotation (make-instance 'url-annotation
                                    :url (url buffer)
                                    :data data)))
    (annotation-add annotation)))

(define-command annotate-highlighted-text (&optional (buffer (current-buffer)))
  "Create a annotation for the highlighted text of BUFFER."
  (with-current-buffer buffer
    (print (url buffer))
    (let* ((snippet (%copy))
           (data (first (prompt
                         :prompt "Annotation"
                         :sources (list (make-instance 'prompter:raw-source)))))
           (annotation (make-instance 'snippet-annotation
                                      :snippet snippet
                                      :url (url buffer)
                                      :data data)))
      (annotation-add annotation))))

(defun render-annotations (annotations)
  (with-current-html-buffer (buffer "*Annotations*" 'base-mode)
        (spinneret:with-html-string
          (:style (style buffer))
          (:h1 "Annotations")
          (loop for annotation in annotations
                collect (:div (:raw (render annotation))
                              (:hr))))))

(define-command show-annotations-for-current-buffer (&optional (source-buffer (current-buffer)))
  "Create a new buffer with the annotations of the current URL of BUFFER."
  (with-data-access (annotations (annotations-path (current-buffer)))
    (let ((filtered-annotations (remove-if-not (lambda (i) (url-equal (quri:uri (url i)) (url source-buffer))) annotations)))
      (render-annotations filtered-annotations))))

(define-class annotation-source (prompter:source)
  ((prompter:name "Annotations")
   (prompter:constructor (get-data (annotations-path (current-buffer))))
   (prompter:multi-selection-p t)))

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
