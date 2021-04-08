;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun directory-elements (directory)
  "Return list of all the files and subdirectories inside DIRECTORY."
  (let ((directory (pathname directory)))
    (append (uiop:subdirectories directory)
            (uiop:directory-files directory))))

(defun make-file-suggestions (suggestions source input)
  (declare (ignore suggestions))
  (let* ((pathname (pathname input)))
    (mapcar (lambda (file)
              (make-instance 'prompter:suggestion
                             :value file
                             :match-data (namestring file)
                             :attributes (prompter:object-attributes file)
                             :source source
                             :input input))
            (directory-elements (if (uiop:directory-pathname-p pathname)
                                    pathname
                                    (uiop:pathname-directory-pathname pathname))))))

(define-class file-source (prompter:source)
  ((prompter:name "Files")
   (prompter:constructor (directory-elements (uiop:getcwd)))
   (prompter:filter-preprocessor 'make-file-suggestions)
   (prompter:multi-selection-p t))
  (:export-class-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Prompt source for file(s) on the disk."))
