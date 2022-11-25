;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/file-manager-mode
    (:documentation "Mode for file management from the prompt buffer."))
(in-package :nyxt/file-manager-mode)

(nyxt/prompt-buffer-mode::define-command-prompt directory-up (prompt-buffer)
  "Remove one level of directory nesting from the current PROMPT-BUFFER file input."
  (let* ((input (prompter:input prompt-buffer))
         (path (uiop:parse-native-namestring input))
         (parent (if (uiop:directory-pathname-p path)
                     (uiop:pathname-parent-directory-pathname path)
                     (uiop:pathname-directory-pathname path))))
    (nyxt:set-prompt-buffer-input (namestring parent) prompt-buffer)))

(define-mode file-manager-mode (nyxt/prompt-buffer-mode:prompt-buffer-mode)
  "Prompt buffer mode for file choosing."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "file-manager-mode" ()
      keyscheme:default
      (list
       "C-backspace" 'directory-up)
      keyscheme:emacs
      (list
       "C-l" 'directory-up)))))

(export-always 'directory-elements)
(defun directory-elements (directory)
  "Return list of all the files and subdirectories inside DIRECTORY."
  (let ((directory (pathname directory)))
    (append (uiop:subdirectories directory)
            (uiop:directory-files directory))))

(export-always 'recursive-directory-elements)
(defun recursive-directory-elements (directory &key include-directories-p)
  (loop with included-directories = '()
        with files = (directory-elements directory)
        for directories = (sera:filter #'uiop:directory-pathname-p files)
          then (sera:filter #'uiop:directory-pathname-p files)
        while directories
        do (dolist (dir directories)
             (when include-directories-p
               (push dir included-directories))
             (setf files (delete dir (append files (directory-elements dir))
                                 :test #'uiop:pathname-equal)))
        finally (return (if include-directories-p
                            (append files included-directories)
                            files))))

(defun current-user ()
  #+sbcl
  (sb-posix:passwd-name (sb-posix:getpwuid (sb-posix:getuid)))
  #-sbcl
  (uiop:getenv "USER"))

(defun group-id (user)
  "Return the group ID of USER name."
  #+sbcl
  (sb-posix:passwd-gid (sb-posix:getpwnam user))
  #-sbcl
  (alex:assoc-value (osicat:user-info user) :group-id))

(defun file-group-id (file)
  #+sbcl
  (sb-posix:stat-gid (sb-posix:lstat file))
  #-sbcl
  (osicat-posix:stat-gid (osicat-posix:lstat file)))

(-> executable-p ((or types:pathname-designator) &key (:user string)) boolean)
(defun executable-p (file &key (user (current-user)))
  "Return non-nil if FILE is executable for USER name.
When the user is unspecified, take the current one."
  (sera:true
   (let* ((filename (uiop:native-namestring file))
          (permissions (iolib/os:file-permissions filename)))
     (or (and (string= (file-author file) user)
              (member :user-exec permissions))
         (and (= (file-group-id filename)
                 (group-id user))
              (member :group-exec permissions))
         (member :other-exec permissions)))))

(export-always 'executables)
(defun executables ()
  "List of pathnames of user-executable programs under PATH enviroment variable."
  (let ((paths (str:split ":" (uiop:getenv "PATH") :omit-nulls t)))
    (sera:filter
     #'executable-p
     (remove-if
      #'uiop:hidden-pathname-p
      (mapcar #'uiop:resolve-symlinks
              (mappend #'uiop:directory-files paths))))))

(define-class program-source (prompter:source)
  ((prompter:name "Programs")
   (prompter:constructor (executables))
   (prompter:multi-selection-p t))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Prompt source for user-accessible programs.")
  (:metaclass user-class))

(defmethod prompter:object-attributes ((path pathname) (source prompter:source))
  (declare (ignore source))
  `(("Path" ,(uiop:native-namestring path))
    ("Name" ,(if (uiop:directory-pathname-p path)
                 (enough-namestring path (files:parent path))
                 (pathname-name path)))
    ("Extension" ,(or (files:pathname-type* path) ""))
    ("Directory" ,(uiop:native-namestring (files:parent path)))))

(defun match-extension (ext)
  (lambda (pathname)
    (string-equal (pathname-type pathname) ext)))

(defun make-file-source-preprocessor ()
  "Return a preprocessor that lists all files satisfying `extensions' and `allow-directories'.
It's suitable for `prompter:filter-preprocessor'."
  (lambda (suggestions source input)
    (declare (ignore suggestions))
    (let* ((pathname (uiop:ensure-pathname (if (uiop:emptyp input)
                                               *default-pathname-defaults*
                                               input)))
           (directory (uiop:pathname-directory-pathname pathname)))
      (prompter:filter-exact-matches
       (prompter:ensure-suggestions-list
        source
        (sera:filter
         (or (path-filter source)
             (lambda (path)
               (or (and (uiop:directory-pathname-p path)
                        (allow-directories source))
                   (and (uiop:file-pathname-p path)
                        (or (null (extensions source))
                            (str:s-member (extensions source) (pathname-type path)))))))
         (directory-elements directory)))
       source
       input))))

(define-class file-source (prompter:source)
  ((prompter:name "Files")
   (prompter:active-attributes-keys
    '("Name" "Extension" "Directory")
    :accessor nil)
   (prompter:filter-preprocessor (make-file-source-preprocessor))
   (prompter:multi-selection-p t)
   (open-file-in-new-buffer-p
    t
    :documentation "Whether to open files and directories in a new buffer.")
   (extensions
    nil
    :type (list-of string)
    :documentation "List of extensions that are displayed.
When nil, all extensions are whitelisted.")
   (allow-directories
    t
    :type boolean
    :documentation "Whether directories are displayed.")
   (path-filter
    nil
    :type (or null (function (pathname) boolean))
    :documentation "Function defining a predicate to filter files.
It takes a pathname and returns a boolean.  For simpler cases, use
`allow-directories'.")
   (supported-media-types
    '("xhtml" "html" "mp3" "ogg" "mp4" "flv" "wmv" "webm" "mkv")
    :type (list-of string)
    :documentation "Media types that Nyxt opens.
Other formats are opened relying on the OS.")
   (open-file-function
    #'default-open-file-function
    ;; TODO: Allow `data-path's?
    :type (function ((or string pathname) &key
                     (:supported-p boolean)
                     (:new-buffer-p boolean)))
    :documentation "Function used to open files.
Takes the name of the file as the first argument and accepts two keyword arguments:

- :supported-p as to whether the file extension is supported by Nyxt (i.e. its
  extension is one of `supported-media-types');
- :new-buffer-p as to whether the file should be opened in a new buffer."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Prompt source for file(s) on the disk.")
  (:metaclass user-class))

(define-class open-file-source (file-source) ()
  (:metaclass user-class))

(defun supported-media-or-directory (filename
                                     &optional (file-source (make-instance 'file-source)))
  "Return T if this filename's extension is a media that Nyxt can open (or a directory).
See `supported-media-types' of `file-mode'."
  (or (and (uiop:directory-pathname-p filename)
           (uiop:directory-exists-p filename))
      (sera:and-let* ((extension (pathname-type filename))
                      (extensions (supported-media-types file-source)))
        (find extension extensions :test #'string-equal))))

(define-command-global edit-file-with-external-editor
    (&optional (files (prompt :input "File(s) to edit"
                              :sources 'file-source)))
  "Edit the FILES using `external-editor-program'.
If FILES are not provided, prompt for them."
  (if (external-editor-program *browser*)
      (progn
        (echo "Using \"~{~a~^ ~}\" to edit ~s." (external-editor-program *browser*) files)
        (uiop:launch-program `(,@(external-editor-program *browser*)
                               ,@(mapcar #'uiop:native-namestring files))))
      (echo-warning "Please set `external-editor-program' browser slot.")))

(defmethod initialize-instance :after ((source open-file-source) &key)
  (setf (slot-value source 'prompter:return-actions)
        (append
         (list (lambda-command open-file* (files)
                 "Open files with `open-file-function' (a sensible default)."
                 (let* ((new-buffer-p (open-file-in-new-buffer-p source)))
                   ;; Open first file according to `open-file-in-new-buffer-p'
                   (funcall (open-file-function source) (first files)
                            :new-buffer-p new-buffer-p
                            :supported-p (supported-media-or-directory (first files) source))
                   ;; Open the rest of the files in new buffers unconditionally.
                   (dolist (file (rest files))
                     (funcall (open-file-function source) file
                              :new-buffer-p t
                              :supported-p (supported-media-or-directory file source)))))
               (lambda-command delete-file* (files)
                 "Deletes the chosen files."
                 (mapcar #'delete-file files))
               (lambda-command rename-file* (files)
                 "Rename the first chosen file."
                 (let* ((file (first files))
                        (name (files:basename file)))
                   (rename-file file (prompt1 :prompt (format nil "New name for ~a" name)
                                              :sources 'prompter:raw-source
                                              :input name))))
               (lambda-command edit-file-with-external-editor* (files)
                 "Edit files in external editor."
                 (edit-file-with-external-editor files))
               ;; TODO: Edit files in Nyxt-internal editor.
               ;; TODO: File/directory copying.
               (lambda-command open-with* (files)
                 "Open files with the selected program."
                 (let* ((program (prompt1
                                  :prompt "The program to open the selected files with"
                                  :sources 'program-source)))
                   (uiop:launch-program (cons (uiop:native-namestring program) (mapcar #'uiop:native-namestring files))))))
         (slot-value source 'prompter:return-actions))))

(export-always 'default-open-file-function)
(defun default-open-file-function (filename &key supported-p new-buffer-p)
  "Open FILENAME in Nyxt if supported, or externally otherwise.
FILENAME is the full path of the file (or directory).

See `supported-media-types' to customize the file types that are opened in
Nyxt and those that are opened externally.

NEW-BUFFER-P defines whether the file/directory is opened in a new buffer.
SUPPORTED-P says whether the file can be opened by Nyxt.

Can be used as a `open-file-function'."
  (handler-case
      (cond
        (supported-p
         (let ((file-url (quri::make-uri-file :path filename)))
           (if new-buffer-p
               (make-buffer-focus :url file-url)
               (buffer-load file-url))))
        ((not (null *open-program*))
         (let ((process (uiop:launch-program
                         (list *open-program* (uiop:native-namestring filename))
                         :error-output :stream)))
           (nyxt:echo "Opening ~s with ~s." filename *open-program*)
           (run-thread "file opener"
             (let ((status (uiop:wait-process process)))
               (unless (= 0 status)
                 (echo-warning "When opening file ~s with ~s : ~a"
                               filename
                               *open-program*
                               (alex:read-stream-content-into-string
                                (uiop:process-info-error-output process))))))))
        (t (nyxt:echo "Cannot open ~s with an external program." filename)))
    ;; We can probably signal something and display a notification.
    (error (c) (log:error "Opening ~a: ~a~&" filename c))))

(define-command-global open-file (&key (default-directory
                                        (if (quri:uri-file-p (url (current-buffer)))
                                            (uiop:pathname-directory-pathname
                                             (quri:uri-path (url (current-buffer))))
                                            *default-pathname-defaults*)))
  "Open a file from the filesystem.

The user is prompted with the prompt-buffer, files are browsable with
fuzzy suggestion.

DEFAULT-DIRECTORY specifies which directory to start from. Defaults to user home
directory.

By default, it uses the `xdg-open' command. The user can override the
`open-file-function' of `file-source' which takes the filename (or
directory name) as parameter.

`file-source' also has `supported-media-types'. You can append new types to
it. Every type in `supported-media-types' will be opened directly in Nyxt."
  (prompt :prompt "Open file"
          :extra-modes 'file-manager-mode
          :input (uiop:native-namestring default-directory)
          :sources 'open-file-source))

(define-command-global download-open-file ()
  "Open file in Nyxt or externally."
  (open-file :default-directory (files:expand (download-directory (current-buffer)))))
