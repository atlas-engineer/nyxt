;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/file-manager-mode)
(trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum)

(defparameter *open-file-in-new-buffer* t
  "If nil, don't open files and directories in a new buffer.")

(defparameter *supported-media-types* '("mp3" "ogg" "mp4" "flv" "wmv" "webm" "mkv")
  ;XXX: either trully read supported MIME types, either try to open
  ;all, either make this list a different purpose (for a user selection).
  "Supported media types.")

(defun supported-media (filename)
  "Return T if this filename's extension is a media that Nyxt can open.
See `*supported-media-types*'."
  (when (uiop:file-pathname-p filename)
    (let ((extension (str:downcase (pathname-type filename))))
      (find extension *supported-media-types* :test #'string-equal))))

;; TODO: By default, we should open directories in a new buffer (see NEW-BUFFER-P), and open supported media in a new buffer (see `supported-media').
(serapeum:export-always 'open-file-function)
(defun open-file-function (filename &key ;; (new-buffer-p *open-file-in-new-buffer*)
                                      )
  "Open FILENAME in Nyxt if supported, or externally otherwise.
FILENAME is the full path of the file (or directory).

See `*supported-media-types*' to customize the file types that are opened in
Nyxt and those that are opened externally.

Can be used as a `*open-file-function*'."
  (handler-case
      (cond
        ((and (uiop:directory-pathname-p filename)
              (uiop:directory-exists-p filename))
         (make-buffer-focus :url (format nil "file://~a" (uiop:ensure-directory-pathname filename))))
        ((supported-media filename)
         (make-buffer-focus :url (format nil "file://~a" filename)))
        (t
         (uiop:launch-program
          #+linux
          (list "xdg-open" (namestring filename))
          #+darwin
          (list "open" (namestring filename)))))
    ;; We can probably signal something and display a notification.
    (error (c) (log:error "Opening ~a: ~a~&" filename c))))

(defparameter *open-file-function* #'open-file-function "Function triggered to open files.")

(define-class directory-source (prompter:source)
  ((prompter:name "Files")
   (directory-path :accessor directory-path :initarg :directory-path)
   (prompter:must-match-p t)
   (prompter:constructor 
    (lambda (source)
      (uiop:directory-files (directory-path source))))))

(define-command nyxt::open-file ()
  "Open a file from the filesystem.

The user is prompted with the minibuffer, files are browsable with
fuzzy suggestion.

The default directory is the one computed by
`download-manager:default-download-directory' (usually `~/Downloads').

By default, it uses the `xdg-open' command. The user can override the
`nyxt:open-file-function' function, which takes the filename (or directory
name) as parameter."
  ;; TODO: How do we set the current directory permanently?
  (uiop:with-current-directory ((uiop:getcwd))
    ;; Allow the current minibuffer to recognize our keybindings.
    (let ((filename (first (prompt
                            :prompt (namestring (uiop:getcwd))
                            :sources (list (make-instance 'directory-source
                                                          :directory-path (uiop:getcwd)))))))
      (funcall nyxt/file-manager-mode::*open-file-function* (namestring filename)))))
