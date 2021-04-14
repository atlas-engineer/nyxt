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

;; TODO: Separate package?
(define-mode file-mode (nyxt/prompt-buffer-mode:prompt-buffer-mode)
  "Prompt-buffer mode forthe Nyxt-openable file filtering."
  ((find-file-in-new-buffer t :documentation "If nil, don't open files and directories in a new buffer.")
   (supported-media-types '("mp3" "ogg" "mp4" "flv" "wmv" "webm" "mkv")
                          :type list-of-strings
                          :documentation "Supported media types.
Used for file filtering in `find-file'.")
   (find-file-function #'default-find-file-function)))

(defun supported-media-or-directory (filename)
  "Return T if this filename's extension is a media that Nyxt can open (or a directory).
See `supported-media-types' of `file-mode'."
  (or (and (uiop:directory-pathname-p filename)
           (uiop:directory-exists-p filename))
      (sera:and-let* ((extension (pathname-type filename))
                      (mode (find-submode (current-prompt-buffer) 'file-mode))
                      (extensions (supported-media-types mode)))
        (find extension extensions :test #'string-equal))))

#+linux
(defvar *xdg-open-program* "xdg-open")

(export-always 'default-find-file-function)
(defun default-find-file-function (filename &key (new-buffer-p
                                                  (find-file-in-new-buffer
                                                   (find-submode (current-prompt-buffer)
                                                                 'file-mode))))
  "Open FILENAME in Nyxt if supported, or externally otherwise.
FILENAME is the full path of the file (or directory).

See `supported-media-types' to customize the file types that are opened in
Nyxt and those that are opened externally.

NEW-BUFFER-P defines whether the file/directory will be open in a new buffer.

Can be used as an `find-file-function'."
  (handler-case
      (if (supported-media-or-directory filename)
          (if new-buffer-p
              (make-buffer-focus :url (format nil "file://~a" filename))
              (buffer-load (format nil "file://~a" filename)))
          (uiop:launch-program
           #+linux
           (list *xdg-open-program* (namestring filename))
           #+darwin
           (list "open" (namestring filename))))
    ;; We can probably signal something and display a notification.
    (error (c) (log:error "Opening ~a: ~a~&" filename c))))


(define-command find-file (&key (default-directory (user-homedir-pathname)))
  "Open a file from the filesystem.

The user is prompted with the prompt-buffer, files are browsable with
fuzzy suggestion.

DEFAULT-DIRECTORY specifies which directory to start from. Defaults to user home
directory.

By default, it uses the `xdg-open' command. The user can override the
`find-file-function' of `file-mode' which takes the filename (or
directory name) as parameter."
  ;; TODO: How do we set the current directory permanently?
  (flet ((supported-media-or-directory-filter (suggestions source input)
           (remove-if-not #'supported-media-or-directory
                          (make-file-suggestions suggestions source input)
                          :key #'prompter:value)))
    (prompt
     :input (namestring default-directory)
     :prompt "Open file"
     :sources (list (make-instance
                     'file-source
                     :actions (list (make-command open* (files)
                                      (dolist (file files)
                                        (funcall
                                         (find-file-function
                                          (find-submode
                                           (first (old-prompt-buffers *browser*)) 'file-mode))
                                         file))))
                     :filter-preprocessor #'supported-media-or-directory-filter)))))
