;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class annotation ()
  ((reference
    nil
    :documentation "The object the annotation is pointing to.")
   (data
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
  "Create/set the annotation of the URL of BUFFER."
  (let* ((data (prompt
                :prompt "Annotation"
                :sources (list (make-instance 'prompter:raw-source))))
         (annotation (make-instance 'url-annotation
                                    :url (url buffer)
                                    :data data)))
    (annotation-add annotation)))
