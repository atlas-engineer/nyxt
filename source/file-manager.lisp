;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun directory-elements (directory)
  "Return list of all the files and subdirectories inside DIRECTORY."
  (let ((directory (pathname directory)))
    (append (uiop:subdirectories directory)
            (uiop:directory-files directory))))

(defmethod prompter:object-attributes ((path pathname))
  ;; TODO: Add dirname, basename, extension.
  ;; It will be useful when we have per-attribute filtering.
  `(("Path" ,(namestring path))))

(defun match-externsion (ext)
  (lambda (pathname)
    (string-equal (pathname-type pathname) ext)))

(defun make-file-source-preprocessor (&rest filters)
  "Return a preprocessor that lists all files satisfying all FILTERS.
It's suitable for `prompter:filter-preprocessor'."
  (lambda (suggestions source input)
    (declare (ignore suggestions))
    (let ((pathname (pathname input)))
      (prompter:filter-exact-matches
       ;; TODO: Export `ensure-suggestions-list'?
       (prompter::ensure-suggestions-list
        source
        (sera:filter
         (apply #'alex:conjoin (or filters (list #'identity)))
         (directory-elements (if (uiop:directory-pathname-p pathname)
                                 pathname
                                 (uiop:pathname-directory-pathname pathname)))))
       source
       input))))

(define-class file-source (prompter:source)
  ((prompter:name "Files")
   (prompter:filter-preprocessor (make-file-source-preprocessor))
   (prompter:multi-selection-p t))
  (:export-class-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Prompt source for file(s) on the disk."))
